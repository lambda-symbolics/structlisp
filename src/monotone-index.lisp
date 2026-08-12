;;;; monotone-index.lisp

(in-package #:structlisp)


;;;; -- Conditions --

(define-condition monotone-integer-index-error (error)
  ((index
    :initarg :index
    :reader monotone-integer-index-error-index
    :documentation "The monotone index involved in the error."))
  (:documentation "Base condition for monotone integer index failures."))

(define-condition monotone-integer-index-order-error (monotone-integer-index-error)
  ((value
    :initarg :value
    :reader monotone-integer-index-order-error-value
    :documentation "The rejected value.")
   (last-value
    :initarg :last-value
    :reader monotone-integer-index-order-error-last-value
    :documentation "The current final value."))
  (:report
   (lambda (condition stream)
     (format stream "Cannot append integer ~D after ~D to a monotone index."
             (monotone-integer-index-order-error-value condition)
             (monotone-integer-index-order-error-last-value condition))))
  (:documentation "An append would violate nondecreasing order."))

(define-condition monotone-integer-index-value-error (monotone-integer-index-error)
  ((value
    :initarg :value
    :reader monotone-integer-index-value-error-value
    :documentation "The out-of-range value.")
   (maximum
    :initarg :maximum
    :reader monotone-integer-index-value-error-maximum
    :documentation "The largest accepted value."))
  (:report
   (lambda (condition stream)
     (format stream "Integer ~S is outside the index range 0 through ~D."
             (monotone-integer-index-value-error-value condition)
             (monotone-integer-index-value-error-maximum condition))))
  (:documentation "A value was negative or exceeded the configured integer width."))


;;;; -- Monotone integer index --

(defstruct (monotone-integer-index
            (:constructor monotone-integer-index--make))
  "A compact nondecreasing integer vector with binary-search queries."
  (values (make-array 0
                      :element-type '(unsigned-byte 32)
                      :adjustable t
                      :fill-pointer 0)
          :type vector)
  (maximum-value (1- (ash 1 32)) :type (integer 0 *)))

(defun make-monotone-integer-index (&key initial-contents
                                         (element-bits 32)
                                         (initial-capacity 0))
  "Create a nondecreasing integer index with ELEMENT-BITS storage.

ELEMENT-BITS may be 8, 16, 32, or 64. Values are unsigned and appends must not
decrease. Binary-search operations are O(log n)."
  (check-type element-bits (member 8 16 32 64))
  (check-type initial-capacity (integer 0 *))
  (let* ((maximum-value (1- (ash 1 element-bits)))
         (index (monotone-integer-index--make
                 :values (make-array initial-capacity
                                     :element-type `(unsigned-byte ,element-bits)
                                     :adjustable t
                                     :fill-pointer 0)
                 :maximum-value maximum-value)))
    (map nil (lambda (value)
               (monotone-integer-index-append index value))
         initial-contents)
    index))

(defun monotone-integer-index-count (index)
  "Return the number of values in INDEX."
  (length (monotone-integer-index-values index)))

(defun monotone-integer-index-empty-p (index)
  "Return true when INDEX contains no values."
  (zerop (monotone-integer-index-count index)))

(defun monotone-integer-index-ref (index position)
  "Return the value at zero-based POSITION."
  (aref (monotone-integer-index-values index) position))

(defun monotone-integer-index-append (index value)
  "Append VALUE and return its zero-based position.

VALUE must fit the configured unsigned width and be at least the current final
value. Duplicate values are allowed."
  (monotone-integer-index--check-value index value)
  (unless (monotone-integer-index-empty-p index)
    (let ((last-value (monotone-integer-index-ref
                       index (1- (monotone-integer-index-count index)))))
      (when (< value last-value)
        (error 'monotone-integer-index-order-error
               :index index
               :value value
               :last-value last-value))))
  (vector-push-extend value (monotone-integer-index-values index))
  (1- (monotone-integer-index-count index)))

(defun monotone-integer-index-lower-bound (index value)
  "Return the first position whose value is not less than VALUE."
  (let ((low 0)
        (high (monotone-integer-index-count index)))
    (loop while (< low high)
          for middle = (floor (+ low high) 2)
          if (< (monotone-integer-index-ref index middle) value)
            do (setf low (1+ middle))
          else
            do (setf high middle))
    low))

(defun monotone-integer-index-upper-bound (index value)
  "Return the first position whose value is greater than VALUE."
  (let ((low 0)
        (high (monotone-integer-index-count index)))
    (loop while (< low high)
          for middle = (floor (+ low high) 2)
          if (<= (monotone-integer-index-ref index middle) value)
            do (setf low (1+ middle))
          else
            do (setf high middle))
    low))

(defun monotone-integer-index-find (index value)
  "Return the first position equal to VALUE and true, or NIL and NIL."
  (let ((position (monotone-integer-index-lower-bound index value)))
    (if (and (< position (monotone-integer-index-count index))
             (= (monotone-integer-index-ref index position) value))
        (values position t)
        (values nil nil))))

(defun monotone-integer-index-before (index value)
  "Return the last value below VALUE, its position, and true.

Return NIL, NIL, and NIL when no indexed value is below VALUE."
  (let ((position (1- (monotone-integer-index-lower-bound index value))))
    (if (>= position 0)
        (values (monotone-integer-index-ref index position) position t)
        (values nil nil nil))))

(defun monotone-integer-index-at-or-before (index value)
  "Return the last value at or below VALUE, its position, and true."
  (let ((position (1- (monotone-integer-index-upper-bound index value))))
    (if (>= position 0)
        (values (monotone-integer-index-ref index position) position t)
        (values nil nil nil))))

(defun monotone-integer-index-at-or-after (index value)
  "Return the first value at or above VALUE, its position, and true."
  (let ((position (monotone-integer-index-lower-bound index value)))
    (if (< position (monotone-integer-index-count index))
        (values (monotone-integer-index-ref index position) position t)
        (values nil nil nil))))

(defun monotone-integer-index-after (index value)
  "Return the first value above VALUE, its position, and true."
  (let ((position (monotone-integer-index-upper-bound index value)))
    (if (< position (monotone-integer-index-count index))
        (values (monotone-integer-index-ref index position) position t)
        (values nil nil nil))))

(defun monotone-integer-index-range (index start end)
  "Return the position range whose values satisfy START <= value < END."
  (values (monotone-integer-index-lower-bound index start)
          (monotone-integer-index-lower-bound index end)))

(defun monotone-integer-index->vector (index)
  "Return a fresh specialized vector containing INDEX's values."
  (copy-seq (monotone-integer-index-values index)))

(defun monotone-integer-index-clear (index)
  "Remove all values from INDEX and return INDEX."
  (setf (fill-pointer (monotone-integer-index-values index)) 0)
  index)


;;;; -- Internal mechanics --

(defun monotone-integer-index--check-value (index value)
  (unless (and (integerp value)
               (<= 0 value (monotone-integer-index-maximum-value index)))
    (error 'monotone-integer-index-value-error
           :index index
           :value value
           :maximum (monotone-integer-index-maximum-value index))))
