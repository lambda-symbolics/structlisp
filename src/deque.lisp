;;;; deque.lisp

(in-package #:structlisp)


;;;; -- Conditions --

(define-condition deque-error (error)
  ((deque
    :initarg :deque
    :reader deque-error-deque
    :documentation "The deque involved in the error."))
  (:documentation "Base condition for deque operation failures."))

(define-condition deque-index-error (deque-error)
  ((index
    :initarg :index
    :reader deque-index-error-index
    :documentation "The invalid zero-based index.")
   (minimum
    :initarg :minimum
    :reader deque-index-error-minimum
    :documentation "The smallest accepted index.")
   (maximum
    :initarg :maximum
    :reader deque-index-error-maximum
    :documentation "The largest accepted index."))
  (:report
   (lambda (condition stream)
     (format stream "Deque index ~D is outside the accepted range ~D through ~D."
             (deque-index-error-index condition)
             (deque-index-error-minimum condition)
             (deque-index-error-maximum condition))))
  (:documentation "An operation received an index outside its accepted range."))

(define-condition deque-empty-error (deque-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The deque is empty." stream)))
  (:documentation "A required deque element was not present."))

(define-condition deque-weight-error (deque-error)
  ((element
    :initarg :element
    :reader deque-weight-error-element
    :documentation "The element whose weight was invalid.")
   (weight
    :initarg :weight
    :reader deque-weight-error-weight
    :documentation "The invalid weight value."))
  (:report
   (lambda (condition stream)
     (format stream "Deque element ~S has invalid weight ~S; expected a non-negative integer."
             (deque-weight-error-element condition)
             (deque-weight-error-weight condition))))
  (:documentation "A deque weight function returned an invalid value."))


;;;; -- Deque --

(defstruct (deque
            (:constructor %make-deque))
  "A circular array deque with optional count and weight budgets."
  (storage (make-array 8 :initial-element nil) :type simple-vector)
  (weights nil :type (or null simple-vector))
  (start 0 :type (integer 0 *))
  (count 0 :type (integer 0 *))
  (total-weight 0 :type (integer 0 *))
  (maximum-count nil :type (or null (integer 0 *)))
  (maximum-weight nil :type (or null (integer 0 *)))
  (weight-function nil :type (or null function))
  (eviction-end :front :type (member :front :back)))

(defun make-deque (&key (initial-capacity 8)
                        initial-contents
                        maximum-count
                        weight-function
                        maximum-weight
                        (eviction-end :front))
  "Create a deque, optionally bounded by element count and total weight.

WEIGHT-FUNCTION must return a non-negative integer for each element. When a
limit is exceeded, elements are removed from EVICTION-END until both limits
hold. Initial contents are inserted from left to right under the same policy."
  (check-type initial-capacity (integer 0 *))
  (check-type maximum-count (or null (integer 0 *)))
  (check-type maximum-weight (or null (integer 0 *)))
  (check-type weight-function (or null function symbol))
  (check-type eviction-end (member :front :back))
  (when (and maximum-weight (null weight-function))
    (error "MAXIMUM-WEIGHT requires WEIGHT-FUNCTION."))
  (let* ((contents (coerce initial-contents 'vector))
         (capacity (max 1 initial-capacity (length contents)))
         (deque (%make-deque
                 :storage         (make-array capacity :initial-element nil)
                 :weights         (and weight-function
                                       (make-array capacity :initial-element nil))
                 :maximum-count   maximum-count
                 :maximum-weight  maximum-weight
                 :weight-function (and weight-function
                                       (coerce weight-function 'function))
                 :eviction-end    eviction-end)))
    (loop for element across contents
          do (deque-push-back deque element))
    deque))

(defun deque-empty-p (deque)
  "Return true when DEQUE contains no elements."
  (zerop (deque-count deque)))

(defun deque-capacity (deque)
  "Return DEQUE's current internal element capacity."
  (length (deque-storage deque)))

(defun deque-ref (deque index)
  "Return the element at zero-based INDEX in DEQUE."
  (deque--check-index deque index nil)
  (aref (deque-storage deque) (deque--physical-index deque index)))

(defun (setf deque-ref) (new-value deque index)
  "Replace the element at zero-based INDEX and return NEW-VALUE.

If DEQUE maintains weights, the cached total is adjusted. Replacement does
not evict other elements; an overweight replacement signals an error."
  (deque--check-index deque index nil)
  (let* ((physical-index (deque--physical-index deque index))
         (old-weight (deque--physical-weight deque physical-index))
         (new-weight (deque--element-weight deque new-value))
         (new-total (+ (- (deque-total-weight deque) old-weight) new-weight)))
    (when (and (deque-maximum-weight deque)
               (> new-total (deque-maximum-weight deque)))
      (error "Replacing deque index ~D would exceed the maximum weight." index))
    (setf (aref (deque-storage deque) physical-index) new-value)
    (when (deque-weights deque)
      (setf (aref (deque-weights deque) physical-index) new-weight))
    (setf (deque-total-weight deque) new-total)
    new-value))

(defun deque-push-front (deque element)
  "Insert ELEMENT at the front of DEQUE.

Return ELEMENT and, as a second value, a vector of elements evicted in removal
order. If the inserted element is itself evicted, the first value is still
ELEMENT."
  (let ((weight (deque--element-weight deque element)))
    (deque--ensure-capacity deque (1+ (deque-count deque)))
    (setf (deque-start deque)
          (mod (1- (deque-start deque)) (deque-capacity deque)))
    (deque--store-at-physical-index deque (deque-start deque) element weight)
    (incf (deque-count deque))
    (incf (deque-total-weight deque) weight)
    (values element (deque--evict-to-limits deque))))

(defun deque-push-back (deque element)
  "Insert ELEMENT at the back of DEQUE.

Return ELEMENT and, as a second value, a vector of elements evicted in removal
order. If the inserted element is itself evicted, the first value is still
ELEMENT."
  (let ((weight (deque--element-weight deque element)))
    (deque--ensure-capacity deque (1+ (deque-count deque)))
    (let ((physical-index (deque--physical-index deque (deque-count deque))))
      (deque--store-at-physical-index deque physical-index element weight))
    (incf (deque-count deque))
    (incf (deque-total-weight deque) weight)
    (values element (deque--evict-to-limits deque))))

(defun deque-pop-front (deque &optional default)
  "Remove and return DEQUE's front element and true.

When DEQUE is empty, return DEFAULT and NIL."
  (if (deque-empty-p deque)
      (values default nil)
      (values (deque--remove-front deque) t)))

(defun deque-pop-back (deque &optional default)
  "Remove and return DEQUE's back element and true.

When DEQUE is empty, return DEFAULT and NIL."
  (if (deque-empty-p deque)
      (values default nil)
      (values (deque--remove-back deque) t)))

(defun deque-front (deque &optional default)
  "Return DEQUE's front element and true, or DEFAULT and NIL when empty."
  (if (deque-empty-p deque)
      (values default nil)
      (values (deque-ref deque 0) t)))

(defun deque-back (deque &optional default)
  "Return DEQUE's back element and true, or DEFAULT and NIL when empty."
  (if (deque-empty-p deque)
      (values default nil)
      (values (deque-ref deque (1- (deque-count deque))) t)))

(defun deque-insert (deque index element)
  "Insert ELEMENT before zero-based INDEX in DEQUE.

INDEX may equal the current count. Return ELEMENT and a vector of evicted
items. Insertion shifts the nearer end and is O(min(INDEX, COUNT-INDEX))."
  (deque--check-index deque index t)
  (cond
    ((zerop index)
     (deque-push-front deque element))
    ((= index (deque-count deque))
     (deque-push-back deque element))
    (t
     (let ((weight (deque--element-weight deque element)))
       (deque--ensure-capacity deque (1+ (deque-count deque)))
       (if (< index (- (deque-count deque) index))
           (progn
             (setf (deque-start deque)
                   (mod (1- (deque-start deque)) (deque-capacity deque)))
             (loop for logical-index from 0 below index
                   do (deque--copy-logical-slot deque
                                                (1+ logical-index)
                                                logical-index)))
           (loop for logical-index downfrom (deque-count deque) above index
                 do (deque--copy-logical-slot deque
                                              (1- logical-index)
                                              logical-index)))
       (deque--store-at-physical-index
        deque (deque--physical-index deque index) element weight)
       (incf (deque-count deque))
       (incf (deque-total-weight deque) weight)
       (values element (deque--evict-to-limits deque))))))

(defun deque-remove-at (deque index)
  "Remove and return the element at zero-based INDEX in DEQUE."
  (deque--check-index deque index nil)
  (cond
    ((zerop index)
     (deque--remove-front deque))
    ((= index (1- (deque-count deque)))
     (deque--remove-back deque))
    (t
     (let* ((physical-index (deque--physical-index deque index))
            (element (aref (deque-storage deque) physical-index))
            (weight (deque--physical-weight deque physical-index)))
       (if (< index (- (deque-count deque) index 1))
           (progn
             (loop for logical-index downfrom index above 0
                   do (deque--copy-logical-slot deque
                                                (1- logical-index)
                                                logical-index))
             (deque--clear-physical-slot deque (deque-start deque))
             (setf (deque-start deque)
                   (mod (1+ (deque-start deque)) (deque-capacity deque))))
           (progn
             (loop for logical-index from index below (1- (deque-count deque))
                   do (deque--copy-logical-slot deque
                                                (1+ logical-index)
                                                logical-index))
             (deque--clear-physical-slot
              deque (deque--physical-index deque (1- (deque-count deque))))))
       (decf (deque-count deque))
       (decf (deque-total-weight deque) weight)
       element))))

(defun deque-split-at (deque index)
  "Destructively split DEQUE before INDEX and return the removed suffix.

DEQUE retains elements before INDEX. The returned deque has the same weight
function but no count or weight limits, so the split cannot discard data. The
operation is linear in the suffix length."
  (deque--check-index deque index t)
  (let ((suffix (make-deque :initial-capacity (- (deque-count deque) index)
                            :weight-function (deque-weight-function deque))))
    (loop while (> (deque-count deque) index)
          do (deque-push-front suffix (deque--remove-back deque)))
    suffix))

(defun deque-clear (deque)
  "Remove all elements from DEQUE and return DEQUE."
  (fill (deque-storage deque) nil)
  (when (deque-weights deque)
    (fill (deque-weights deque) nil))
  (setf (deque-start deque) 0
        (deque-count deque) 0
        (deque-total-weight deque) 0)
  deque)

(defun deque->vector (deque)
  "Return a fresh vector containing DEQUE's elements from front to back."
  (let ((result (make-array (deque-count deque))))
    (loop for index below (deque-count deque)
          do (setf (aref result index) (deque-ref deque index)))
    result))

(defun deque->list (deque)
  "Return a fresh list containing DEQUE's elements from front to back."
  (loop for index below (deque-count deque)
        collect (deque-ref deque index)))

(defun deque-copy (deque)
  "Return an independent deque preserving DEQUE's contents and configuration."
  (%make-deque :storage (copy-seq (deque-storage deque))
               :weights (and (deque-weights deque)
                             (copy-seq (deque-weights deque)))
               :start (deque-start deque)
               :count (deque-count deque)
               :total-weight (deque-total-weight deque)
               :maximum-count (deque-maximum-count deque)
               :maximum-weight (deque-maximum-weight deque)
               :weight-function (deque-weight-function deque)
               :eviction-end (deque-eviction-end deque)))

(defun deque-position-if (predicate deque &key from-end
                                               (start 0)
                                               end
                                               (key #'identity))
  "Return the position of the first element satisfying PREDICATE and true.

Search the half-open range from START through END, which defaults to the deque
count. FROM-END reverses the search direction. KEY is applied to each element
before PREDICATE. Return NIL and NIL when no element matches."
  (check-type predicate (or function symbol))
  (check-type key (or function symbol))
  (check-type start (integer 0 *))
  (check-type end (or null (integer 0 *)))
  (let ((end (or end (deque-count deque))))
    (unless (<= start end (deque-count deque))
      (error "Deque search range [~D, ~D) exceeds count ~D."
             start end (deque-count deque)))
    (let ((predicate-function (coerce predicate 'function))
          (key-function (coerce key 'function)))
      (if from-end
          (loop for index downfrom (1- end) to start
                when (funcall predicate-function
                              (funcall key-function (deque-ref deque index)))
                  do (return (values index t))
                finally (return (values nil nil)))
          (loop for index from start below end
                when (funcall predicate-function
                              (funcall key-function (deque-ref deque index)))
                  do (return (values index t))
                finally (return (values nil nil)))))))

(defun deque-position (item deque &key (key #'identity)
                                       (test #'eql test-supplied-p)
                                       (test-not nil test-not-supplied-p))
  "Return the position of the first element matching ITEM and true.

KEY is applied to each deque element before comparison. TEST and TEST-NOT have
the same complementary semantics as their Common Lisp sequence counterparts
and may not both be supplied. Return NIL and NIL when no element matches."
  (deque--position item deque key test test-supplied-p
                   test-not test-not-supplied-p))

(defun deque-find (item deque &key (key #'identity)
                                   (test #'eql test-supplied-p)
                                   (test-not nil test-not-supplied-p))
  "Return the first element matching ITEM and true, or NIL and NIL if absent.

KEY, TEST, and TEST-NOT follow DEQUE-POSITION semantics. The presence value
distinguishes a stored NIL element from absence."
  (multiple-value-bind (index present-p)
      (deque--position item deque key test test-supplied-p
                       test-not test-not-supplied-p)
    (if present-p
        (values (deque-ref deque index) t)
        (values nil nil))))

(defun deque-delete (item deque &key (key #'identity)
                                     (test #'eql test-supplied-p)
                                     (test-not nil test-not-supplied-p))
  "Remove and return the first element matching ITEM and true.

KEY, TEST, and TEST-NOT follow DEQUE-POSITION semantics. Removal uses indexed
shifting and maintains DEQUE's cached weight. Return NIL and NIL if absent."
  (multiple-value-bind (index present-p)
      (deque--position item deque key test test-supplied-p
                       test-not test-not-supplied-p)
    (if present-p
        (values (deque-remove-at deque index) t)
        (values nil nil))))

(defun deque-append (deque sequence)
  "Append SEQUENCE's elements to DEQUE in order.

SEQUENCE may be another deque or a Common Lisp sequence. Traversal uses a
snapshot, so DEQUE may also be the source. Return DEQUE and, as a second value,
a vector containing all policy-evicted elements in removal order."
  (let ((elements (if (deque-p sequence)
                      (deque->vector sequence)
                      (coerce sequence 'vector)))
        (evicted (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for element across elements
          do (multiple-value-bind (ignored newly-evicted)
                 (deque-push-back deque element)
               (declare (ignore ignored))
               (loop for evicted-element across newly-evicted
                     do (vector-push-extend evicted-element evicted))))
    (values deque (coerce evicted 'simple-vector))))

(defun deque-prepend (deque sequence &key (key #'identity))
  "Prepend SEQUENCE's elements to DEQUE in order after applying KEY.

Traversal and KEY application use a snapshot, so DEQUE may also be the source.
Return DEQUE, a vector of policy-evicted elements in removal order, and a vector
of the transformed elements in front-to-back order."
  (let ((elements (deque--mapped-vector sequence key))
        (evicted (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for element across elements
          do (deque--element-weight deque element))
    (loop for index downfrom (1- (length elements)) to 0
          do (multiple-value-bind (ignored newly-evicted)
                 (deque-push-front deque (aref elements index))
               (declare (ignore ignored))
               (loop for evicted-element across newly-evicted
                     do (vector-push-extend evicted-element evicted))))
    (values deque (coerce evicted 'simple-vector) elements)))

(defun deque-move-if (predicate source target &key (key #'identity))
  "Move SOURCE elements satisfying PREDICATE to TARGET in order.

KEY is applied before PREDICATE, while the original element moves. Predicate,
key, and weight callbacks run against snapshots before either deque changes.
Return TARGET, a vector of policy-evicted target elements in removal order, and
a vector of moved source elements. Retained SOURCE elements keep their order.
Moving a deque to itself is a no-op and does not call callbacks. Any operation
error leaves both deques unchanged."
  (check-type predicate (or function symbol))
  (check-type key (or function symbol))
  (when (eq source target)
    (return-from deque-move-if (values target #() #())))
  (let* ((predicate-function (coerce predicate 'function))
         (key-function (coerce key 'function))
         (elements (deque->vector source))
         (moved (make-array 0 :adjustable t :fill-pointer 0))
         (retained (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for element across elements
          do (vector-push-extend
              element
              (if (funcall predicate-function
                           (funcall key-function element))
                  moved
                  retained)))
    (when (zerop (length moved))
      (return-from deque-move-if (values target #() #())))
    (let ((staged-source (deque-copy source))
          (staged-target (deque-copy target)))
      (multiple-value-bind (ignored evicted)
          (deque-append staged-target moved)
        (declare (ignore ignored))
        (deque-clear staged-source)
        (deque-append staged-source retained)
        (deque--replace-state target staged-target)
        (deque--replace-state source staged-source)
        (values target evicted (coerce moved 'simple-vector))))))

(defun deque-move-all (source target &key (key #'identity))
  "Move every transformed SOURCE element to TARGET in front-to-back order.

KEY is applied to a snapshot before either deque changes. TARGET applies its own
weight function and eviction policy. Return TARGET, a vector containing all
policy-evicted elements in removal order, and a vector of transformed elements.
On success SOURCE is empty with zero maintained weight. Moving a deque to itself
is a no-op and does not call KEY."
  (when (eq source target)
    (return-from deque-move-all (values target #() #())))
  (let ((elements (deque--mapped-vector source key)))
    (loop for element across elements
          do (deque--element-weight target element))
    (multiple-value-bind (result evicted) (deque-append target elements)
      (deque-clear source)
      (values result evicted elements))))


;;;; -- Internal mechanics --

(defun deque--replace-state (target source)
  "Replace TARGET's complete state with SOURCE's and return TARGET."
  (setf (deque-storage target) (deque-storage source)
        (deque-weights target) (deque-weights source)
        (deque-start target) (deque-start source)
        (deque-count target) (deque-count source)
        (deque-total-weight target) (deque-total-weight source)
        (deque-maximum-count target) (deque-maximum-count source)
        (deque-maximum-weight target) (deque-maximum-weight source)
        (deque-weight-function target) (deque-weight-function source)
        (deque-eviction-end target) (deque-eviction-end source))
  target)

(defun deque--mapped-vector (sequence key)
  (check-type key (or function symbol))
  (let* ((elements (if (deque-p sequence)
                       (deque->vector sequence)
                       (coerce sequence 'vector)))
         (result (make-array (length elements)))
         (function (coerce key 'function)))
    (loop for index below (length elements)
          do (setf (aref result index)
                   (funcall function (aref elements index))))
    result))

(defun deque--physical-index (deque logical-index)
  (mod (+ (deque-start deque) logical-index) (deque-capacity deque)))

(defun deque--check-index (deque index allow-end-p)
  (check-type index integer)
  (let ((maximum (if allow-end-p
                     (deque-count deque)
                     (1- (deque-count deque)))))
    (unless (<= 0 index maximum)
      (error 'deque-index-error
             :deque deque
             :index index
             :minimum 0
             :maximum maximum))))


(defun deque--position (item deque key test test-supplied-p
                        test-not test-not-supplied-p)
  (when (and test-supplied-p test-not-supplied-p)
    (error "TEST and TEST-NOT may not both be supplied."))
  (check-type key (or function symbol))
  (check-type test (or function symbol))
  (when test-not-supplied-p
    (check-type test-not (or function symbol)))
  (let ((key-function (coerce key 'function))
        (test-function (coerce (if test-not-supplied-p test-not test)
                               'function)))
    (loop for index below (deque-count deque)
          for element = (deque-ref deque index)
          for matched-p = (funcall test-function
                                   item
                                   (funcall key-function element))
          when (if test-not-supplied-p (not matched-p) matched-p)
            do (return (values index t))
          finally (return (values nil nil)))))

(defun deque--element-weight (deque element)
  (let ((weight (if (deque-weight-function deque)
                    (funcall (deque-weight-function deque) element)
                    0)))
    (unless (typep weight '(integer 0 *))
      (error 'deque-weight-error
             :deque deque
             :element element
             :weight weight))
    weight))

(defun deque--physical-weight (deque physical-index)
  (if (deque-weights deque)
      (aref (deque-weights deque) physical-index)
      0))

(defun deque--store-at-physical-index (deque physical-index element weight)
  (setf (aref (deque-storage deque) physical-index) element)
  (when (deque-weights deque)
    (setf (aref (deque-weights deque) physical-index) weight)))

(defun deque--clear-physical-slot (deque physical-index)
  (setf (aref (deque-storage deque) physical-index) nil)
  (when (deque-weights deque)
    (setf (aref (deque-weights deque) physical-index) nil)))

(defun deque--copy-logical-slot (deque source-index target-index)
  (let ((source-physical (deque--physical-index deque source-index))
        (target-physical (deque--physical-index deque target-index)))
    (setf (aref (deque-storage deque) target-physical)
          (aref (deque-storage deque) source-physical))
    (when (deque-weights deque)
      (setf (aref (deque-weights deque) target-physical)
            (aref (deque-weights deque) source-physical)))))

(defun deque--ensure-capacity (deque required-capacity)
  (when (> required-capacity (deque-capacity deque))
    (let* ((old-capacity (deque-capacity deque))
           (new-capacity (max required-capacity (* 2 old-capacity)))
           (new-storage (make-array new-capacity :initial-element nil))
           (new-weights (and (deque-weights deque)
                             (make-array new-capacity :initial-element nil))))
      (loop for index below (deque-count deque)
            for old-index = (deque--physical-index deque index)
            do (setf (aref new-storage index)
                     (aref (deque-storage deque) old-index))
               (when new-weights
                 (setf (aref new-weights index)
                       (aref (deque-weights deque) old-index))))
      (setf (deque-storage deque) new-storage
            (deque-weights deque) new-weights
            (deque-start deque) 0))))

(defun deque--remove-front (deque)
  (let* ((physical-index (deque-start deque))
         (element (aref (deque-storage deque) physical-index))
         (weight (deque--physical-weight deque physical-index)))
    (deque--clear-physical-slot deque physical-index)
    (setf (deque-start deque)
          (if (= (deque-count deque) 1)
              0
              (mod (1+ physical-index) (deque-capacity deque))))
    (decf (deque-count deque))
    (decf (deque-total-weight deque) weight)
    element))

(defun deque--remove-back (deque)
  (let* ((physical-index (deque--physical-index deque (1- (deque-count deque))))
         (element (aref (deque-storage deque) physical-index))
         (weight (deque--physical-weight deque physical-index)))
    (deque--clear-physical-slot deque physical-index)
    (decf (deque-count deque))
    (decf (deque-total-weight deque) weight)
    (when (zerop (deque-count deque))
      (setf (deque-start deque) 0))
    element))

(defun deque--over-limit-p (deque)
  (or (and (deque-maximum-count deque)
           (> (deque-count deque) (deque-maximum-count deque)))
      (and (deque-maximum-weight deque)
           (> (deque-total-weight deque) (deque-maximum-weight deque)))))

(defun deque--evict-to-limits (deque)
  (let ((evicted (make-array 0 :adjustable t :fill-pointer 0)))
    (loop while (deque--over-limit-p deque)
          do (vector-push-extend
              (ecase (deque-eviction-end deque)
                (:front (deque--remove-front deque))
                (:back (deque--remove-back deque)))
              evicted))
    (coerce evicted 'simple-vector)))
