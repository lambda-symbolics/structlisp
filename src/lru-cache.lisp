;;;; lru-cache.lisp

(in-package #:structlisp)


;;;; -- Conditions --

(define-condition lru-cache-error (error)
  ((cache
    :initarg :cache
    :reader lru-cache-error-cache
    :documentation "The cache involved in the error."))
  (:documentation "Base condition for LRU cache failures."))

(define-condition lru-cache-weight-error (lru-cache-error)
  ((key
    :initarg :key
    :reader lru-cache-weight-error-key
    :documentation "The key whose entry weight was invalid.")
   (value
    :initarg :value
    :reader lru-cache-weight-error-value
    :documentation "The value whose entry weight was invalid.")
   (weight
    :initarg :weight
    :reader lru-cache-weight-error-weight
    :documentation "The invalid weight."))
  (:report
   (lambda (condition stream)
     (format stream "LRU cache entry ~S => ~S has invalid weight ~S."
             (lru-cache-weight-error-key condition)
             (lru-cache-weight-error-value condition)
             (lru-cache-weight-error-weight condition))))
  (:documentation "A cache weight function returned a negative or non-integer value."))


;;;; -- LRU cache --

(defstruct (lru-cache-node
            (:constructor lru-cache--make-node))
  key
  value
  (weight 0 :type (integer 0 *))
  previous
  next)

(defstruct (lru-cache
            (:constructor lru-cache--make))
  "A least-recently-used cache with maintained count and weight budgets."
  (table (make-hash-table) :type hash-table)
  first-node
  last-node
  (total-weight 0 :type (integer 0 *))
  (maximum-count nil :type (or null (integer 0 *)))
  (maximum-weight nil :type (or null (integer 0 *)))
  (weight-function nil :type (or null function))
  (eviction-function nil :type (or null function)))

(defun make-lru-cache (&key maximum-count
                            maximum-weight
                            weight-function
                            eviction-function
                            (test 'eql))
  "Create an LRU cache bounded by entry count, total weight, or both.

WEIGHT-FUNCTION receives a key and value and must return a non-negative integer.
EVICTION-FUNCTION, when supplied, is called with key and value after each
budget-triggered eviction. Zero limits are valid and retain no entries."
  (check-type maximum-count (or null (integer 0 *)))
  (check-type maximum-weight (or null (integer 0 *)))
  (check-type weight-function (or null function symbol))
  (check-type eviction-function (or null function symbol))
  (when (and maximum-weight (null weight-function))
    (error "MAXIMUM-WEIGHT requires WEIGHT-FUNCTION."))
  (lru-cache--make
   :table             (make-hash-table :test test)
   :maximum-count     maximum-count
   :maximum-weight    maximum-weight
   :weight-function   (and weight-function
                           (coerce weight-function 'function))
   :eviction-function (and eviction-function
                           (coerce eviction-function 'function))))

(defun lru-cache-count (cache)
  "Return the number of retained entries in CACHE."
  (hash-table-count (lru-cache-table cache)))

(defun lru-cache-empty-p (cache)
  "Return true when CACHE contains no entries."
  (zerop (lru-cache-count cache)))

(defun lru-cache-get (cache key &optional default)
  "Return KEY's value and true, promoting it to most recently used.

When KEY is absent, return DEFAULT and NIL."
  (multiple-value-bind (node present-p)
      (gethash key (lru-cache-table cache))
    (if present-p
        (progn
          (lru-cache--move-to-back cache node)
          (values (lru-cache-node-value node) t))
        (values default nil))))

(defun lru-cache-peek (cache key &optional default)
  "Return KEY's value and true without changing recency.

When KEY is absent, return DEFAULT and NIL."
  (multiple-value-bind (node present-p)
      (gethash key (lru-cache-table cache))
    (if present-p
        (values (lru-cache-node-value node) t)
        (values default nil))))

(defun lru-cache-put (cache key value)
  "Associate KEY with VALUE, mark it most recent, and enforce budgets.

Return VALUE and a vector of evicted key/value conses in eviction order. An
entry whose own weight exceeds the budget is inserted and immediately evicted."
  (let ((weight (lru-cache--entry-weight cache key value)))
    (multiple-value-bind (node present-p)
        (gethash key (lru-cache-table cache))
      (if present-p
          (progn
            (decf (lru-cache-total-weight cache) (lru-cache-node-weight node))
            (setf (lru-cache-node-value node) value
                  (lru-cache-node-weight node) weight)
            (incf (lru-cache-total-weight cache) weight)
            (lru-cache--move-to-back cache node))
          (let ((new-node (lru-cache--make-node
                           :key      key
                           :value    value
                           :weight   weight
                           :previous (lru-cache-last-node cache))))
            (if (lru-cache-last-node cache)
                (setf (lru-cache-node-next (lru-cache-last-node cache))
                      new-node)
                (setf (lru-cache-first-node cache) new-node))
            (setf (lru-cache-last-node cache) new-node
                  (gethash key (lru-cache-table cache)) new-node)
            (incf (lru-cache-total-weight cache) weight))))
    (values value (lru-cache--evict-to-limits cache))))

(defun lru-cache-delete (cache key &optional default)
  "Remove KEY and return its value and true, or DEFAULT and NIL when absent."
  (multiple-value-bind (node present-p)
      (gethash key (lru-cache-table cache))
    (if present-p
        (progn
          (lru-cache--remove-node cache node nil)
          (values (lru-cache-node-value node) t))
        (values default nil))))

(defun lru-cache-least-recent (cache &optional default)
  "Return the least-recent key, value, and true without changing recency."
  (let ((node (lru-cache-first-node cache)))
    (if node
        (values (lru-cache-node-key node)
                (lru-cache-node-value node)
                t)
        (values default default nil))))

(defun lru-cache-most-recent (cache &optional default)
  "Return the most-recent key, value, and true without changing recency."
  (let ((node (lru-cache-last-node cache)))
    (if node
        (values (lru-cache-node-key node)
                (lru-cache-node-value node)
                t)
        (values default default nil))))

(defun lru-cache-map (function cache)
  "Call FUNCTION with key and value from least to most recent, then return CACHE.

Traversal uses a key/value snapshot taken before the first call. Mutating CACHE
from FUNCTION does not alter which entries are visited or their values."
  (let ((entries (make-array (lru-cache-count cache)))
        (position 0))
    (loop for node = (lru-cache-first-node cache)
                    then (lru-cache-node-next node)
          while node
          do (setf (aref entries position)
                   (cons (lru-cache-node-key node)
                         (lru-cache-node-value node)))
             (incf position))
    (loop for entry across entries
          do (funcall function (first entry) (rest entry))))
  cache)

(defun lru-cache-keys (cache)
  "Return a fresh vector of keys from least to most recent."
  (let ((keys (make-array (lru-cache-count cache)))
        (position 0))
    (lru-cache-map
     (lambda (key value)
       (declare (ignore value))
       (setf (aref keys position) key)
       (incf position))
     cache)
    keys))

(defun lru-cache-clear (cache)
  "Remove all entries without calling the eviction function, then return CACHE."
  (clrhash (lru-cache-table cache))
  (setf (lru-cache-first-node cache) nil
        (lru-cache-last-node cache) nil
        (lru-cache-total-weight cache) 0)
  cache)


;;;; -- Memo cache --

(defstruct (memo-cache
            (:constructor memo-cache--make))
  "A producer-backed memoization cache."
  (cache (make-lru-cache) :type lru-cache))

(defun make-memo-cache (&rest lru-cache-arguments)
  "Create a memo cache using MAKE-LRU-CACHE arguments."
  (memo-cache--make :cache (apply #'make-lru-cache lru-cache-arguments)))

(defun memo-cache-count (cache)
  "Return the number of memoized values in CACHE."
  (lru-cache-count (memo-cache-cache cache)))

(defun memo-cache-get (cache key producer)
  "Return KEY's memoized value, computing it with PRODUCER on a miss.

PRODUCER is called with no arguments. The second value is true for a cache hit
and NIL when the value was newly computed. NIL values are cached normally."
  (multiple-value-bind (value present-p)
      (lru-cache-get (memo-cache-cache cache) key)
    (if present-p
        (values value t)
        (let ((new-value (funcall producer)))
          (lru-cache-put (memo-cache-cache cache) key new-value)
          (values new-value nil)))))

(defun memo-cache-delete (cache key &optional default)
  "Remove KEY and return its value and true, or DEFAULT and NIL when absent."
  (lru-cache-delete (memo-cache-cache cache) key default))

(defun memo-cache-clear (cache)
  "Remove all memoized values and return CACHE."
  (lru-cache-clear (memo-cache-cache cache))
  cache)


;;;; -- Internal mechanics --

(defun lru-cache--entry-weight (cache key value)
  (let ((weight (if (lru-cache-weight-function cache)
                    (funcall (lru-cache-weight-function cache) key value)
                    0)))
    (unless (typep weight '(integer 0 *))
      (error 'lru-cache-weight-error
             :cache cache
             :key key
             :value value
             :weight weight))
    weight))

(defun lru-cache--unlink-node (cache node)
  (let ((previous (lru-cache-node-previous node))
        (next (lru-cache-node-next node)))
    (if previous
        (setf (lru-cache-node-next previous) next)
        (setf (lru-cache-first-node cache) next))
    (if next
        (setf (lru-cache-node-previous next) previous)
        (setf (lru-cache-last-node cache) previous))
    (setf (lru-cache-node-previous node) nil
          (lru-cache-node-next node) nil)
    node))

(defun lru-cache--move-to-back (cache node)
  (unless (eq node (lru-cache-last-node cache))
    (lru-cache--unlink-node cache node)
    (setf (lru-cache-node-previous node) (lru-cache-last-node cache)
          (lru-cache-node-next node) nil)
    (if (lru-cache-last-node cache)
        (setf (lru-cache-node-next (lru-cache-last-node cache)) node)
        (setf (lru-cache-first-node cache) node))
    (setf (lru-cache-last-node cache) node))
  node)

(defun lru-cache--remove-node (cache node budget-eviction-p)
  (lru-cache--unlink-node cache node)
  (remhash (lru-cache-node-key node) (lru-cache-table cache))
  (decf (lru-cache-total-weight cache) (lru-cache-node-weight node))
  (when (and budget-eviction-p (lru-cache-eviction-function cache))
    (funcall (lru-cache-eviction-function cache)
             (lru-cache-node-key node)
             (lru-cache-node-value node)))
  node)

(defun lru-cache--over-limit-p (cache)
  (or (and (lru-cache-maximum-count cache)
           (> (lru-cache-count cache) (lru-cache-maximum-count cache)))
      (and (lru-cache-maximum-weight cache)
           (> (lru-cache-total-weight cache) (lru-cache-maximum-weight cache)))))

(defun lru-cache--evict-to-limits (cache)
  (let ((evicted (make-array 0 :adjustable t :fill-pointer 0)))
    (loop while (lru-cache--over-limit-p cache)
          for node = (lru-cache-first-node cache)
          do (lru-cache--remove-node cache node t)
             (vector-push-extend
              (cons (lru-cache-node-key node)
                    (lru-cache-node-value node))
              evicted))
    (coerce evicted 'simple-vector)))
