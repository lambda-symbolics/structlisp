;;;; priority-queue.lisp

(in-package #:structlisp)


;;;; -- Conditions --

(define-condition priority-queue-error (error)
  ((queue
    :initarg :queue
    :reader priority-queue-error-queue
    :documentation "The priority queue involved in the error."))
  (:documentation "Base condition for priority queue failures."))

(define-condition priority-queue-duplicate-key (priority-queue-error)
  ((key
    :initarg :key
    :reader priority-queue-duplicate-key-key
    :documentation "The duplicate key."))
  (:report
   (lambda (condition stream)
     (format stream "Priority queue key ~S is already present."
             (priority-queue-duplicate-key-key condition))))
  (:documentation "An indexed priority queue insertion reused an existing key."))


;;;; -- Priority queue --

(defstruct (priority-queue-entry
            (:constructor priority-queue--make-entry))
  item
  priority
  key
  (indexed-p nil :type boolean)
  (sequence 0 :type (integer 0 *))
  (index 0 :type (integer 0 *)))

(defstruct (priority-queue
            (:constructor priority-queue--make))
  "A stable binary min-heap with optional unique key indexing."
  (storage (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (lessp #'< :type function)
  (key-function nil :type (or null function))
  (key-index (make-hash-table) :type hash-table)
  (next-sequence 0 :type (integer 0 *)))

(defun make-priority-queue (&key (lessp #'<) key-function (key-test 'eql))
  "Create a stable min-priority queue.

LESSP compares priorities. Equal priorities are popped in insertion order.
KEY-FUNCTION optionally derives a unique cancellation key from each item.
Individual insertions may instead supply an explicit key. KEY-TEST is one of
the standard hash-table tests."
  (check-type lessp (or function symbol))
  (check-type key-function (or null function symbol))
  (priority-queue--make
   :lessp        (coerce lessp 'function)
   :key-function (and key-function (coerce key-function 'function))
   :key-index    (make-hash-table :test key-test)))

(defun priority-queue-count (queue)
  "Return the number of entries in QUEUE."
  (length (priority-queue-storage queue)))

(defun priority-queue-empty-p (queue)
  "Return true when QUEUE has no entries."
  (zerop (priority-queue-count queue)))

(defun priority-queue-push (queue item priority &key (key nil key-supplied-p))
  "Insert ITEM with PRIORITY into QUEUE and return ITEM.

When KEY is supplied, or QUEUE has a key function, the entry may later be
cancelled or reprioritized in O(log n). Indexed keys must be unique."
  (let* ((indexed-p (or key-supplied-p (priority-queue-key-function queue)))
         (effective-key (if key-supplied-p
                            key
                            (when (priority-queue-key-function queue)
                              (funcall (priority-queue-key-function queue) item)))))
    (when (and indexed-p
               (nth-value 1 (gethash effective-key
                                     (priority-queue-key-index queue))))
      (error 'priority-queue-duplicate-key
             :queue queue
             :key effective-key))
    (let* ((storage (priority-queue-storage queue))
           (index (length storage))
           (entry (priority-queue--make-entry
                   :item      item
                   :priority  priority
                   :key       effective-key
                   :indexed-p (not (null indexed-p))
                   :sequence  (priority-queue-next-sequence queue)
                   :index     index)))
      (incf (priority-queue-next-sequence queue))
      (vector-push-extend entry storage)
      (when indexed-p
        (setf (gethash effective-key (priority-queue-key-index queue)) entry))
      (priority-queue--sift-up queue index)
      item)))

(defun priority-queue-peek (queue &optional default)
  "Return the minimum item, priority, key, and true without removing it.

When QUEUE is empty, return DEFAULT, NIL, NIL, and NIL."
  (if (priority-queue-empty-p queue)
      (values default nil nil nil)
      (let ((entry (aref (priority-queue-storage queue) 0)))
        (values (priority-queue-entry-item entry)
                (priority-queue-entry-priority entry)
                (priority-queue-entry-key entry)
                t))))

(defun priority-queue-pop (queue &optional default)
  "Remove and return the minimum item, priority, key, and true.

When QUEUE is empty, return DEFAULT, NIL, NIL, and NIL."
  (if (priority-queue-empty-p queue)
      (values default nil nil nil)
      (priority-queue--remove-at queue 0)))

(defun priority-queue-cancel (queue key &optional default)
  "Remove the entry indexed by KEY and return its item, priority, and true.

When KEY is absent, return DEFAULT, NIL, and NIL."
  (multiple-value-bind (entry present-p)
      (gethash key (priority-queue-key-index queue))
    (if present-p
        (multiple-value-bind (item priority ignored-key removed-p)
            (priority-queue--remove-at queue (priority-queue-entry-index entry))
          (declare (ignore ignored-key removed-p))
          (values item priority t))
        (values default nil nil))))

(defun priority-queue-change-priority (queue key new-priority)
  "Set the entry identified by KEY to NEW-PRIORITY and return its item.

Return NIL and NIL when KEY is absent. The second value is true on success."
  (multiple-value-bind (entry present-p)
      (gethash key (priority-queue-key-index queue))
    (if (not present-p)
        (values nil nil)
        (let ((index (priority-queue-entry-index entry)))
          (setf (priority-queue-entry-priority entry) new-priority)
          (if (and (> index 0)
                   (priority-queue--entry-before-p
                    queue entry
                    (aref (priority-queue-storage queue)
                          (priority-queue--parent-index index))))
              (priority-queue--sift-up queue index)
              (priority-queue--sift-down queue index))
          (values (priority-queue-entry-item entry) t)))))

(defun priority-queue-clear (queue)
  "Remove every entry from QUEUE and return QUEUE."
  (setf (fill-pointer (priority-queue-storage queue)) 0)
  (clrhash (priority-queue-key-index queue))
  queue)

(defun priority-queue->vector (queue)
  "Return a fresh vector of QUEUE's items in non-destructive priority order.

Items with equivalent priorities retain insertion order. Mutating the returned
vector does not mutate QUEUE."
  (let ((entries (copy-seq (priority-queue-storage queue))))
    (sort entries (lambda (left right)
                    (priority-queue--entry-before-p queue left right)))
    (map 'vector #'priority-queue-entry-item entries)))

(defun priority-queue->list (queue)
  "Return a fresh list of QUEUE's items in non-destructive priority order."
  (coerce (priority-queue->vector queue) 'list))

(defun top-k (sequence count &key (key #'identity) (lessp #'<))
  "Return the best COUNT elements of SEQUENCE as a best-first vector.

KEY maps elements to priorities and LESSP defines a better priority. Selection
uses O(COUNT) storage and O(n log COUNT) comparisons. Ties retain input order."
  (check-type count (integer 0 *))
  (check-type key (or function symbol))
  (check-type lessp (or function symbol))
  (when (zerop count)
    (return-from top-k #()))
  (let ((key-function (coerce key 'function))
        (less-function (coerce lessp 'function))
        (heap (make-array 0 :adjustable t :fill-pointer 0))
        (sequence-number 0))
    (map nil
         (lambda (item)
           (let ((candidate (vector item
                                    (funcall key-function item)
                                    sequence-number)))
             (incf sequence-number)
             (if (< (length heap) count)
                 (progn
                   (vector-push-extend candidate heap)
                   (top-k--sift-up-worst heap (1- (length heap)) less-function))
                 (when (top-k--better-p candidate (aref heap 0) less-function)
                   (setf (aref heap 0) candidate)
                   (top-k--sift-down-worst heap 0 less-function)))))
         sequence)
    (let ((result (sort (coerce heap 'vector)
                        (lambda (left right)
                          (top-k--better-p left right less-function)))))
      (map 'vector (lambda (candidate) (aref candidate 0)) result))))


;;;; -- Priority queue internals --

(defun priority-queue--parent-index (index)
  (floor (1- index) 2))

(defun priority-queue--entry-before-p (queue left right)
  (let* ((lessp (priority-queue-lessp queue))
         (left-priority (priority-queue-entry-priority left))
         (right-priority (priority-queue-entry-priority right)))
    (or (funcall lessp left-priority right-priority)
        (and (not (funcall lessp right-priority left-priority))
             (< (priority-queue-entry-sequence left)
                (priority-queue-entry-sequence right))))))

(defun priority-queue--swap (queue left-index right-index)
  (let* ((storage (priority-queue-storage queue))
         (left (aref storage left-index))
         (right (aref storage right-index)))
    (rotatef (aref storage left-index) (aref storage right-index))
    (setf (priority-queue-entry-index left) right-index
          (priority-queue-entry-index right) left-index)))

(defun priority-queue--sift-up (queue index)
  (loop while (> index 0)
        for parent = (priority-queue--parent-index index)
        while (priority-queue--entry-before-p
               queue
               (aref (priority-queue-storage queue) index)
               (aref (priority-queue-storage queue) parent))
        do (priority-queue--swap queue index parent)
           (setf index parent))
  index)

(defun priority-queue--sift-down (queue index)
  (let ((storage (priority-queue-storage queue)))
    (loop
      (let* ((count (length storage))
             (left (+ (* 2 index) 1))
             (right (1+ left))
             (best index))
        (when (and (< left count)
                   (priority-queue--entry-before-p
                    queue (aref storage left) (aref storage best)))
          (setf best left))
        (when (and (< right count)
                   (priority-queue--entry-before-p
                    queue (aref storage right) (aref storage best)))
          (setf best right))
        (when (= best index)
          (return index))
        (priority-queue--swap queue index best)
        (setf index best)))))

(defun priority-queue--remove-at (queue index)
  (let* ((storage (priority-queue-storage queue))
         (last-index (1- (length storage)))
         (removed (aref storage index)))
    (when (priority-queue-entry-indexed-p removed)
      (remhash (priority-queue-entry-key removed)
               (priority-queue-key-index queue)))
    (if (= index last-index)
        (decf (fill-pointer storage))
        (let ((replacement (aref storage last-index)))
          (setf (aref storage index) replacement
                (priority-queue-entry-index replacement) index)
          (decf (fill-pointer storage))
          (if (and (> index 0)
                   (priority-queue--entry-before-p
                    queue replacement
                    (aref storage (priority-queue--parent-index index))))
              (priority-queue--sift-up queue index)
              (priority-queue--sift-down queue index))))
    (values (priority-queue-entry-item removed)
            (priority-queue-entry-priority removed)
            (priority-queue-entry-key removed)
            t)))


;;;; -- Top-K internals --

(defun top-k--better-p (left right lessp)
  (let ((left-priority (aref left 1))
        (right-priority (aref right 1)))
    (or (funcall lessp left-priority right-priority)
        (and (not (funcall lessp right-priority left-priority))
             (< (aref left 2) (aref right 2))))))

(defun top-k--worse-p (left right lessp)
  (top-k--better-p right left lessp))

(defun top-k--swap (heap left-index right-index)
  (rotatef (aref heap left-index) (aref heap right-index)))

(defun top-k--sift-up-worst (heap index lessp)
  (loop while (> index 0)
        for parent = (floor (1- index) 2)
        while (top-k--worse-p (aref heap index) (aref heap parent) lessp)
        do (top-k--swap heap index parent)
           (setf index parent)))

(defun top-k--sift-down-worst (heap index lessp)
  (loop
    (let* ((count (length heap))
           (left (+ (* 2 index) 1))
           (right (1+ left))
           (worst index))
      (when (and (< left count)
                 (top-k--worse-p (aref heap left) (aref heap worst) lessp))
        (setf worst left))
      (when (and (< right count)
                 (top-k--worse-p (aref heap right) (aref heap worst) lessp))
        (setf worst right))
      (when (= worst index)
        (return))
      (top-k--swap heap index worst)
      (setf index worst))))
