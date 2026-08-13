;;;; fifo-cache.lisp

(in-package #:structlisp)


;;;; -- Conditions --

(define-condition fifo-cache-error (error)
  ((cache
    :initarg :cache
    :reader fifo-cache-error-cache
    :documentation "The cache involved in the error."))
  (:documentation "Base condition for FIFO cache failures."))

(define-condition fifo-cache-weight-error (fifo-cache-error)
  ((key
    :initarg :key
    :reader fifo-cache-weight-error-key
    :documentation "The key whose entry weight was invalid.")
   (value
    :initarg :value
    :reader fifo-cache-weight-error-value
    :documentation "The value whose entry weight was invalid.")
   (weight
    :initarg :weight
    :reader fifo-cache-weight-error-weight
    :documentation "The invalid weight."))
  (:report
   (lambda (condition stream)
     (format stream "FIFO cache entry ~S => ~S has invalid weight ~S."
             (fifo-cache-weight-error-key condition)
             (fifo-cache-weight-error-value condition)
             (fifo-cache-weight-error-weight condition))))
  (:documentation "A FIFO cache weight function returned an invalid value."))

(define-condition fifo-cache-callback-mutation-error (fifo-cache-error)
  ((callback
    :initarg :callback
    :reader fifo-cache-callback-mutation-error-callback
    :documentation "The callback kind active when mutation was attempted.")
   (operation
    :initarg :operation
    :reader fifo-cache-callback-mutation-error-operation
    :documentation "The attempted mutating operation."))
  (:report
   (lambda (condition stream)
     (format stream "Cannot perform ~S while a FIFO cache ~S callback is active."
             (fifo-cache-callback-mutation-error-operation condition)
             (fifo-cache-callback-mutation-error-callback condition))))
  (:documentation "A FIFO cache callback attempted to mutate its cache."))

(define-condition fifo-cache-eviction-callback-error (fifo-cache-error)
  ((evicted-entries
    :initarg :evicted-entries
    :reader fifo-cache-eviction-callback-error-evicted-entries
    :documentation "All key/value entries removed before callback delivery.")
   (failures
    :initarg :failures
    :reader fifo-cache-eviction-callback-error-failures
    :documentation "Callback failure records in eviction order."))
  (:report
   (lambda (condition stream)
     (format stream
             "~D FIFO cache eviction callbacks failed after ~D entries were removed."
             (length (fifo-cache-eviction-callback-error-failures condition))
             (length (fifo-cache-eviction-callback-error-evicted-entries
                      condition)))))
  (:documentation
   "One or more eviction callbacks failed after all callbacks were attempted."))

(defstruct (fifo-cache-callback-failure
            (:constructor fifo-cache--make-callback-failure))
  "One failed FIFO cache eviction callback invocation."
  (index 0 :type (integer 0 *))
  key
  value
  condition)


;;;; -- FIFO cache --

(defstruct (fifo-cache-entry
            (:constructor fifo-cache--make-entry))
  value
  (weight 0 :type (integer 0 *)))

(defstruct (fifo-cache
            (:constructor fifo-cache--make)
            (:conc-name fifo-cache--slot-))
  "A keyed insertion-ordered cache with maintained count and weight budgets."
  (entries (make-ordered-map) :type ordered-map)
  (total-weight 0 :type (integer 0 *))
  (maximum-count nil :type (or null (integer 0 *)))
  (maximum-weight nil :type (or null (integer 0 *)))
  (weight-function nil :type (or null function))
  (eviction-function nil :type (or null function))
  (active-callback nil :type (member nil :weight :eviction :predicate)))

(defun make-fifo-cache (&key maximum-count
                             maximum-weight
                             weight-function
                             eviction-function
                             (test 'eql)
                             initial-contents)
  "Create a keyed FIFO cache bounded by entry count, total weight, or both.

WEIGHT-FUNCTION receives a key and value and must return a non-negative integer.
EVICTION-FUNCTION, when supplied, receives each budget-evicted key and value in
oldest-first order. Neither callback may mutate the cache. TEST is passed to
MAKE-HASH-TABLE. INITIAL-CONTENTS is a sequence of key/value conses inserted
from left to right under the same policy. Zero limits are valid and retain no
positive-weight entries or, for a zero count limit, no entries at all."
  (check-type maximum-count (or null (integer 0 *)))
  (check-type maximum-weight (or null (integer 0 *)))
  (check-type weight-function (or null function symbol))
  (check-type eviction-function (or null function symbol))
  (when (and maximum-weight (null weight-function))
    (error "MAXIMUM-WEIGHT requires WEIGHT-FUNCTION."))
  (let ((cache (fifo-cache--make
                :entries           (make-ordered-map :test test)
                :maximum-count     maximum-count
                :maximum-weight    maximum-weight
                :weight-function   (and weight-function
                                        (coerce weight-function 'function))
                :eviction-function (and eviction-function
                                        (coerce eviction-function 'function)))))
    (map nil
         (lambda (entry)
           (fifo-cache-put cache (first entry) (rest entry)))
         initial-contents)
    cache))

(defun fifo-cache-count (cache)
  "Return the number of retained entries in CACHE."
  (ordered-map-count (fifo-cache--slot-entries cache)))

(defun fifo-cache-empty-p (cache)
  "Return true when CACHE contains no entries."
  (zerop (fifo-cache-count cache)))

(defun fifo-cache-total-weight (cache)
  "Return CACHE's maintained sum of entry weights."
  (fifo-cache--slot-total-weight cache))

(defun fifo-cache-maximum-count (cache)
  "Return CACHE's maximum entry count, or NIL when it is unbounded by count."
  (fifo-cache--slot-maximum-count cache))

(defun fifo-cache-maximum-weight (cache)
  "Return CACHE's maximum total weight, or NIL when it is unbounded by weight."
  (fifo-cache--slot-maximum-weight cache))

(defun fifo-cache-get (cache key &optional default)
  "Return KEY's value and true without changing insertion order.

When KEY is absent, return DEFAULT and NIL. NIL values are retained normally."
  (multiple-value-bind (entry present-p)
      (ordered-map-get (fifo-cache--slot-entries cache) key)
    (if present-p
        (values (fifo-cache-entry-value entry) t)
        (values default nil))))

(defun fifo-cache-peek (cache key &optional default)
  "Return KEY's value and true without changing insertion order.

This is an explicit non-promoting alias for FIFO-CACHE-GET."
  (fifo-cache-get cache key default))

(defun fifo-cache-put (cache key value)
  "Associate KEY with VALUE, preserve existing position, and enforce budgets.

New keys are appended at the back. Return VALUE and a fresh vector of evicted
key/value conses in oldest-first eviction order. An entry whose own weight
exceeds a budget is inserted and immediately evicted."
  (fifo-cache--ensure-mutable cache 'fifo-cache-put)
  (let ((weight (fifo-cache--entry-weight cache key value)))
    (multiple-value-bind (entry present-p)
        (ordered-map-get (fifo-cache--slot-entries cache) key)
      (if present-p
          (progn
            (decf (fifo-cache--slot-total-weight cache)
                  (fifo-cache-entry-weight entry))
            (setf (fifo-cache-entry-value entry) value
                  (fifo-cache-entry-weight entry) weight)
            (incf (fifo-cache--slot-total-weight cache) weight))
          (progn
            (ordered-map-set
             (fifo-cache--slot-entries cache)
             key
             (fifo-cache--make-entry :value value :weight weight))
            (incf (fifo-cache--slot-total-weight cache) weight))))
    (values value (fifo-cache--evict-to-limits cache))))

(defun fifo-cache-delete (cache key &optional default)
  "Remove KEY and return its value and true, or DEFAULT and NIL when absent."
  (fifo-cache--ensure-mutable cache 'fifo-cache-delete)
  (multiple-value-bind (entry present-p)
      (ordered-map-delete (fifo-cache--slot-entries cache) key)
    (if present-p
        (progn
          (decf (fifo-cache--slot-total-weight cache)
                (fifo-cache-entry-weight entry))
          (values (fifo-cache-entry-value entry) t))
        (values default nil))))

(defun fifo-cache-oldest (cache &optional default)
  "Return the oldest key, value, and true without changing insertion order."
  (multiple-value-bind (key entry present-p)
      (ordered-map-first (fifo-cache--slot-entries cache) default)
    (if present-p
        (values key (fifo-cache-entry-value entry) t)
        (values default default nil))))

(defun fifo-cache-newest (cache &optional default)
  "Return the newest key, value, and true without changing insertion order."
  (multiple-value-bind (key entry present-p)
      (ordered-map-last (fifo-cache--slot-entries cache) default)
    (if present-p
        (values key (fifo-cache-entry-value entry) t)
        (values default default nil))))

(defun fifo-cache-pop-oldest (cache &optional default)
  "Remove and return the oldest key, value, and true.

When CACHE is empty, return DEFAULT, DEFAULT, and NIL. Explicit removal does not
call the eviction function."
  (fifo-cache--ensure-mutable cache 'fifo-cache-pop-oldest)
  (multiple-value-bind (key entry present-p)
      (ordered-map-pop-first (fifo-cache--slot-entries cache) default)
    (if present-p
        (progn
          (decf (fifo-cache--slot-total-weight cache)
                (fifo-cache-entry-weight entry))
          (values key (fifo-cache-entry-value entry) t))
        (values default default nil))))

(defun fifo-cache-delete-first-if (predicate cache)
  "Delete the oldest entry satisfying PREDICATE and return key, value, and true.

PREDICATE receives each key and value in insertion order. It may inspect but not
mutate CACHE. When no entry matches, return NIL, NIL, and NIL. The search does
not allocate an order snapshot."
  (fifo-cache--ensure-mutable cache 'fifo-cache-delete-first-if)
  (multiple-value-bind (entry-key entry present-p)
      (ordered-map--delete-first-if
       (lambda (entry-key entry)
         (fifo-cache--call-client
          cache
          :predicate
          predicate
          entry-key
          (fifo-cache-entry-value entry)))
       (fifo-cache--slot-entries cache))
    (if present-p
        (progn
          (decf (fifo-cache--slot-total-weight cache)
                (fifo-cache-entry-weight entry))
          (values entry-key (fifo-cache-entry-value entry) t))
        (values nil nil nil))))

(defun fifo-cache-move-to-back (cache key &optional default)
  "Move KEY to the newest position and return its value and true.

When KEY is absent, return DEFAULT and NIL."
  (fifo-cache--ensure-mutable cache 'fifo-cache-move-to-back)
  (multiple-value-bind (entry present-p)
      (ordered-map-move-to-back (fifo-cache--slot-entries cache) key)
    (if present-p
        (values (fifo-cache-entry-value entry) t)
        (values default nil))))

(defun fifo-cache-map (function cache)
  "Call FUNCTION with each key and value from oldest to newest, then return CACHE.

Traversal uses a detached key/value snapshot taken before the first call.
Mutating CACHE from FUNCTION does not alter which entries are visited or their
values."
  (let ((entries (fifo-cache->alist cache)))
    (dolist (entry entries)
      (funcall function (first entry) (rest entry))))
  cache)

(defun fifo-cache-keys (cache)
  "Return a fresh vector of CACHE's keys from oldest to newest."
  (let ((keys (make-array (fifo-cache-count cache)))
        (position 0))
    (fifo-cache-map
     (lambda (key value)
       (declare (ignore value))
       (setf (aref keys position) key)
       (incf position))
     cache)
    keys))

(defun fifo-cache-values (cache)
  "Return a fresh vector of CACHE's values from oldest to newest."
  (let ((values (make-array (fifo-cache-count cache)))
        (position 0))
    (fifo-cache-map
     (lambda (key value)
       (declare (ignore key))
       (setf (aref values position) value)
       (incf position))
     cache)
    values))

(defun fifo-cache->alist (cache)
  "Return a fresh association list of CACHE's entries from oldest to newest."
  (let ((entries nil))
    (ordered-map-map
     (lambda (key entry)
       (push (cons key (fifo-cache-entry-value entry)) entries))
     (fifo-cache--slot-entries cache))
    (nreverse entries)))

(defun fifo-cache-clear (cache)
  "Remove all entries without calling the eviction function, then return CACHE."
  (fifo-cache--ensure-mutable cache 'fifo-cache-clear)
  (ordered-map-clear (fifo-cache--slot-entries cache))
  (setf (fifo-cache--slot-total-weight cache) 0)
  cache)


;;;; -- Internal mechanics --

(defun fifo-cache--ensure-mutable (cache operation)
  (when (fifo-cache--slot-active-callback cache)
    (error 'fifo-cache-callback-mutation-error
           :cache cache
           :callback (fifo-cache--slot-active-callback cache)
           :operation operation))
  cache)

(defun fifo-cache--call-client (cache callback function &rest arguments)
  (let ((previous-callback (fifo-cache--slot-active-callback cache)))
    (unwind-protect
         (progn
           (setf (fifo-cache--slot-active-callback cache) callback)
           (apply function arguments))
      (setf (fifo-cache--slot-active-callback cache) previous-callback))))

(defun fifo-cache--entry-weight (cache key value)
  (let ((weight
          (if (fifo-cache--slot-weight-function cache)
              (fifo-cache--call-client
               cache
               :weight
               (fifo-cache--slot-weight-function cache)
               key
               value)
              0)))
    (unless (typep weight '(integer 0 *))
      (error 'fifo-cache-weight-error
             :cache cache
             :key key
             :value value
             :weight weight))
    weight))

(defun fifo-cache--over-limit-p (cache)
  (or (and (fifo-cache--slot-maximum-count cache)
           (> (fifo-cache-count cache)
              (fifo-cache--slot-maximum-count cache)))
      (and (fifo-cache--slot-maximum-weight cache)
           (> (fifo-cache--slot-total-weight cache)
              (fifo-cache--slot-maximum-weight cache)))))

(defun fifo-cache--evict-to-limits (cache)
  (let ((evicted (make-array 0 :adjustable t :fill-pointer 0)))
    (loop while (fifo-cache--over-limit-p cache)
          do (multiple-value-bind (key entry present-p)
                 (ordered-map-pop-first (fifo-cache--slot-entries cache))
               (declare (ignore present-p))
               (decf (fifo-cache--slot-total-weight cache)
                     (fifo-cache-entry-weight entry))
               (vector-push-extend
                (cons key (fifo-cache-entry-value entry))
                evicted)))
    (when (fifo-cache--slot-eviction-function cache)
      (let ((failures (make-array 0 :adjustable t :fill-pointer 0)))
        (loop for entry across evicted
              for index from 0
              do (handler-case
                     (fifo-cache--call-client
                      cache
                      :eviction
                      (fifo-cache--slot-eviction-function cache)
                      (first entry)
                      (rest entry))
                   (error (condition)
                     (vector-push-extend
                      (fifo-cache--make-callback-failure
                       :index     index
                       :key       (first entry)
                       :value     (rest entry)
                       :condition condition)
                      failures))))
        (unless (zerop (length failures))
          (error 'fifo-cache-eviction-callback-error
                 :cache cache
                 :evicted-entries (coerce evicted 'simple-vector)
                 :failures (coerce failures 'simple-vector)))))
    (coerce evicted 'simple-vector)))
