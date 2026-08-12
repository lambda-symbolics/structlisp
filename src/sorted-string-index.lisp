;;;; sorted-string-index.lisp

(in-package #:structlisp)


;;;; -- Conditions --

(define-condition sorted-string-index-error (error)
  ((index
    :initarg :index
    :reader sorted-string-index-error-index
    :documentation "The sorted string index involved in the error."))
  (:documentation "Base condition for sorted string index failures."))

(define-condition sorted-string-index-key-error (sorted-string-index-error)
  ((item
    :initarg :item
    :reader sorted-string-index-key-error-item
    :documentation "The item whose normalized key was invalid.")
   (key
    :initarg :key
    :reader sorted-string-index-key-error-key
    :documentation "The invalid normalized key."))
  (:report
   (lambda (condition stream)
     (format stream "Sorted string index item ~S produced non-string key ~S."
             (sorted-string-index-key-error-item condition)
             (sorted-string-index-key-error-key condition))))
  (:documentation "An item did not produce a string key."))


;;;; -- Sorted string index --

(defstruct (sorted-string-index
            (:constructor sorted-string-index--make))
  "A sorted vector of items with cached normalized string keys."
  (items #() :type simple-vector)
  (keys #() :type simple-vector)
  (key-function #'identity :type function)
  (normalizer #'identity :type function))

(defun make-sorted-string-index (&key initial-contents
                                      (key #'identity)
                                      (normalizer #'identity))
  "Create an index over INITIAL-CONTENTS sorted by normalized string key.

KEY extracts a string from each item. NORMALIZER transforms both item keys and
queries, for example with STRING-DOWNCASE for case-insensitive lookup. Keys are
copied and cached, so normalization happens once per insertion rather than once
per query and candidate. Later mutation of an item does not update its key.
Equal keys retain input order."
  (check-type key (or function symbol))
  (check-type normalizer (or function symbol))
  (let* ((index (sorted-string-index--make
                 :key-function (coerce key 'function)
                 :normalizer   (coerce normalizer 'function)))
         (entries (make-array 0 :adjustable t :fill-pointer 0)))
    (map nil
         (lambda (item)
           (vector-push-extend
            (cons (sorted-string-index--item-key index item) item)
            entries))
         initial-contents)
    (setf entries
          (stable-sort entries #'string< :key #'first))
    (let ((count (length entries)))
      (setf (sorted-string-index-items index) (make-array count)
            (sorted-string-index-keys index) (make-array count))
      (loop for entry across entries
            for position from 0
            do (setf (aref (sorted-string-index-keys index) position)
                     (first entry)
                     (aref (sorted-string-index-items index) position)
                     (rest entry))))
    index))

(defun sorted-string-index-count (index)
  "Return the number of items in INDEX."
  (length (sorted-string-index-items index)))

(defun sorted-string-index-empty-p (index)
  "Return true when INDEX contains no items."
  (zerop (sorted-string-index-count index)))

(defun sorted-string-index-ref (index position)
  "Return the item at sorted zero-based POSITION."
  (aref (sorted-string-index-items index) position))

(defun sorted-string-index-key-ref (index position)
  "Return a fresh copy of the cached normalized key at sorted POSITION."
  (copy-seq (aref (sorted-string-index-keys index) position)))

(defun sorted-string-index-lower-bound (index query)
  "Return the first position whose normalized key is not less than QUERY."
  (let ((key (sorted-string-index--query-key index query)))
    (sorted-string-index--lower-bound-key index key)))

(defun sorted-string-index-upper-bound (index query)
  "Return the first position whose normalized key is greater than QUERY."
  (let ((key (sorted-string-index--query-key index query)))
    (sorted-string-index--upper-bound-key index key)))

(defun sorted-string-index-equal-range (index query)
  "Return the inclusive-start, exclusive-end range of keys equal to QUERY."
  (let ((key (sorted-string-index--query-key index query)))
    (values (sorted-string-index--lower-bound-key index key)
            (sorted-string-index--upper-bound-key index key))))

(defun sorted-string-index-prefix-range (index prefix)
  "Return the inclusive-start, exclusive-end range matching PREFIX.

Both bounds are found with binary search. An empty prefix selects the complete
index."
  (let ((key (sorted-string-index--query-key index prefix)))
    (values (sorted-string-index--prefix-bound index key nil)
            (sorted-string-index--prefix-bound index key t))))

(defun sorted-string-index-prefix-items (index prefix)
  "Return a fresh vector of items whose normalized keys begin with PREFIX."
  (multiple-value-bind (start end)
      (sorted-string-index-prefix-range index prefix)
    (subseq (sorted-string-index-items index) start end)))

(defun sorted-string-index-insert (index item)
  "Insert ITEM after existing equal keys and return its sorted position.

The normalized key is copied; later mutation of ITEM does not update it."
  (let* ((key (sorted-string-index--item-key index item))
         (position (sorted-string-index--upper-bound-key index key)))
    (setf (sorted-string-index-items index)
          (sorted-string-index--vector-insert
           (sorted-string-index-items index) position item)
          (sorted-string-index-keys index)
          (sorted-string-index--vector-insert
           (sorted-string-index-keys index) position key))
    position))

(defun sorted-string-index-remove-at (index position)
  "Remove and return the item at sorted POSITION."
  (let ((item (sorted-string-index-ref index position)))
    (setf (sorted-string-index-items index)
          (sorted-string-index--vector-remove
           (sorted-string-index-items index) position)
          (sorted-string-index-keys index)
          (sorted-string-index--vector-remove
           (sorted-string-index-keys index) position))
    item))

(defun sorted-string-index-remove (index item &key (test #'eql))
  "Remove the first matching ITEM and return it and true.

The item vector is searched directly, so removal still works if an item's key
has changed since insertion. Return NIL and NIL when no item matches."
  (loop for position below (sorted-string-index-count index)
        for candidate = (sorted-string-index-ref index position)
        when (funcall test item candidate)
          do (return (values (sorted-string-index-remove-at index position) t))
        finally (return (values nil nil))))

(defun sorted-string-index->vector (index)
  "Return a fresh vector of every item in sorted order."
  (copy-seq (sorted-string-index-items index)))


;;;; -- Internal mechanics --

(defun sorted-string-index--item-key (index item)
  (let ((key (funcall (sorted-string-index-normalizer index)
                      (funcall (sorted-string-index-key-function index) item))))
    (unless (stringp key)
      (error 'sorted-string-index-key-error
             :index index
             :item item
             :key key))
    (copy-seq key)))

(defun sorted-string-index--query-key (index query)
  (let ((key (funcall (sorted-string-index-normalizer index) query)))
    (unless (stringp key)
      (error 'sorted-string-index-key-error
             :index index
             :item query
             :key key))
    key))

(defun sorted-string-index--key-at (index position)
  (aref (sorted-string-index-keys index) position))

(defun sorted-string-index--lower-bound-key (index query-key)
  (let ((low 0)
        (high (sorted-string-index-count index)))
    (loop while (< low high)
          for middle = (floor (+ low high) 2)
          if (string< (sorted-string-index--key-at index middle) query-key)
            do (setf low (1+ middle))
          else
            do (setf high middle))
    low))

(defun sorted-string-index--upper-bound-key (index query-key)
  (let ((low 0)
        (high (sorted-string-index-count index)))
    (loop while (< low high)
          for middle = (floor (+ low high) 2)
          if (string< query-key (sorted-string-index--key-at index middle))
            do (setf high middle)
          else
            do (setf low (1+ middle)))
    low))

(defun sorted-string-index--prefix-compare (key prefix)
  (let* ((key-length (length key))
         (prefix-length (length prefix))
         (shared-length (min key-length prefix-length)))
    (loop for position below shared-length
          for key-character = (char key position)
          for prefix-character = (char prefix position)
          when (char< key-character prefix-character)
            do (return-from sorted-string-index--prefix-compare -1)
          when (char> key-character prefix-character)
            do (return-from sorted-string-index--prefix-compare 1))
    (cond
      ((< key-length prefix-length) -1)
      (t 0))))

(defun sorted-string-index--prefix-bound (index prefix upper-p)
  (let ((low 0)
        (high (sorted-string-index-count index)))
    (loop while (< low high)
          for middle = (floor (+ low high) 2)
          for comparison = (sorted-string-index--prefix-compare
                            (sorted-string-index--key-at index middle)
                            prefix)
          if (if upper-p (<= comparison 0) (< comparison 0))
            do (setf low (1+ middle))
          else
            do (setf high middle))
    low))

(defun sorted-string-index--vector-insert (vector position item)
  (let* ((count (length vector))
         (result (make-array (1+ count))))
    (replace result vector :end1 position :end2 position)
    (setf (aref result position) item)
    (replace result vector :start1 (1+ position) :start2 position)
    result))

(defun sorted-string-index--vector-remove (vector position)
  (let* ((count (length vector))
         (result (make-array (1- count))))
    (replace result vector :end1 position :end2 position)
    (replace result vector :start1 position :start2 (1+ position))
    result))
