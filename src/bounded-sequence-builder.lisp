;;;; bounded-sequence-builder.lisp

(in-package #:structlisp)


;;;; -- Conditions --

(define-condition bounded-sequence-builder-overflow (error)
  ((builder
    :initarg :builder
    :reader bounded-sequence-builder-overflow-builder
    :documentation "The builder that could not accept the requested elements.")
   (requested-count
    :initarg :requested-count
    :reader bounded-sequence-builder-overflow-requested-count
    :documentation "The number of elements requested by the append operation.")
   (requested-weight
    :initarg :requested-weight
    :reader bounded-sequence-builder-overflow-requested-weight
    :documentation "The total weight requested by the append operation.")
   (remaining-capacity
    :initarg :remaining-capacity
    :reader bounded-sequence-builder-overflow-remaining-capacity
    :documentation "The element capacity remaining before the append.")
   (remaining-weight
    :initarg :remaining-weight
    :reader bounded-sequence-builder-overflow-remaining-weight
    :documentation "The weight capacity remaining before the append, or NIL."))
  (:report
   (lambda (condition stream)
     (format stream
             "Cannot append ~D element~:P of weight ~D; ~D element~:P~@[ and weight ~D~] remain."
             (bounded-sequence-builder-overflow-requested-count condition)
             (bounded-sequence-builder-overflow-requested-weight condition)
             (bounded-sequence-builder-overflow-remaining-capacity condition)
             (bounded-sequence-builder-overflow-remaining-weight condition))))
  (:documentation "An exact append exceeded a bounded sequence builder budget."))

(define-condition bounded-sequence-builder-weight-error (error)
  ((builder
    :initarg :builder
    :reader bounded-sequence-builder-weight-error-builder
    :documentation "The builder whose weight function returned an invalid value.")
   (element
    :initarg :element
    :reader bounded-sequence-builder-weight-error-element
    :documentation "The element whose weight was invalid.")
   (weight
    :initarg :weight
    :reader bounded-sequence-builder-weight-error-weight
    :documentation "The invalid weight value."))
  (:report
   (lambda (condition stream)
     (format stream "Builder element ~S has invalid weight ~S."
             (bounded-sequence-builder-weight-error-element condition)
             (bounded-sequence-builder-weight-error-weight condition))))
  (:documentation "A builder weight function returned a negative or non-integer value."))

(define-condition bounded-sequence-builder-callback-mutation-error (error)
  ((builder
    :initarg :builder
    :reader bounded-sequence-builder-callback-mutation-error-builder
    :documentation "The builder whose callback attempted mutation.")
   (operation
    :initarg :operation
    :reader bounded-sequence-builder-callback-mutation-error-operation
    :documentation "The attempted mutating operation."))
  (:report
   (lambda (condition stream)
     (format stream "Cannot perform ~S while a builder weight callback is active."
             (bounded-sequence-builder-callback-mutation-error-operation
              condition))))
  (:documentation "A weight callback attempted to mutate its builder."))


;;;; -- Builder --

(defstruct (bounded-sequence-builder
            (:constructor %make-bounded-sequence-builder)
            (:conc-name %bounded-sequence-builder-))
  "An adjustable sequence accumulator with fixed count and optional weight limits."
  (storage (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (maximum-count 0 :type (integer 0 *))
  (maximum-weight nil :type (or null (integer 0 *)))
  (weight-function nil :type (or null function))
  (total-weight 0 :type (integer 0 *))
  (element-type t)
  (overflowed-p nil :type boolean)
  (weight-callback-active-p nil :type boolean))

(defun make-bounded-sequence-builder
    (maximum-count &key
                     (element-type t)
                     (initial-capacity (min 64 maximum-count))
                     maximum-weight
                     weight-function)
  "Create an empty sequence builder with fixed count and optional weight limits.

ELEMENT-TYPE is passed to MAKE-ARRAY. INITIAL-CAPACITY controls allocation but
is clamped to MAXIMUM-COUNT. WEIGHT-FUNCTION must return a non-negative integer
for each element when MAXIMUM-WEIGHT is supplied."
  (check-type maximum-count (integer 0 *))
  (check-type initial-capacity (integer 0 *))
  (check-type maximum-weight (or null (integer 0 *)))
  (check-type weight-function (or null function symbol))
  (when (and (not (null maximum-weight))
             (null weight-function))
    (error "MAXIMUM-WEIGHT requires WEIGHT-FUNCTION."))
  (let ((capacity (min initial-capacity maximum-count)))
    (%make-bounded-sequence-builder
     :storage         (make-array capacity
                                  :element-type element-type
                                  :adjustable t
                                  :fill-pointer 0)
     :maximum-count   maximum-count
     :maximum-weight  maximum-weight
     :weight-function (and weight-function
                           (coerce weight-function 'function))
     :element-type    element-type)))

(defun bounded-sequence-builder-count (builder)
  "Return the number of elements currently accumulated in BUILDER."
  (fill-pointer (%bounded-sequence-builder-storage builder)))

(defun bounded-sequence-builder-remaining-capacity (builder)
  "Return the number of elements BUILDER can still accept."
  (- (%bounded-sequence-builder-maximum-count builder)
     (bounded-sequence-builder-count builder)))

(defun bounded-sequence-builder-maximum-count (builder)
  "Return BUILDER's fixed maximum element count."
  (%bounded-sequence-builder-maximum-count builder))

(defun bounded-sequence-builder-total-weight (builder)
  "Return the total weight of BUILDER's accumulated elements."
  (%bounded-sequence-builder-total-weight builder))

(defun bounded-sequence-builder-maximum-weight (builder)
  "Return BUILDER's maximum weight, or NIL when no weight limit applies."
  (%bounded-sequence-builder-maximum-weight builder))

(defun bounded-sequence-builder-remaining-weight (builder)
  "Return BUILDER's remaining weight capacity, or NIL when unbounded by weight."
  (let ((maximum-weight (%bounded-sequence-builder-maximum-weight builder)))
    (unless (null maximum-weight)
      (- maximum-weight
         (%bounded-sequence-builder-total-weight builder)))))

(defun bounded-sequence-builder-weight-function (builder)
  "Return BUILDER's element weight function, or NIL."
  (%bounded-sequence-builder-weight-function builder))

(defun bounded-sequence-builder-element-type (builder)
  "Return the upgraded array element type used by BUILDER."
  (array-element-type (%bounded-sequence-builder-storage builder)))

(defun bounded-sequence-builder-overflowed-p (builder)
  "Return true when an append has exceeded either of BUILDER's budgets.

The state remains true until BOUNDED-SEQUENCE-BUILDER-CLEAR or
BOUNDED-SEQUENCE-BUILDER-FINISH resets the builder."
  (%bounded-sequence-builder-overflowed-p builder))

(defun bounded-sequence-builder-capacity (builder)
  "Return BUILDER's current internal allocation capacity."
  (array-total-size (%bounded-sequence-builder-storage builder)))


(defun bounded-sequence-builder-append (builder element)
  "Append ELEMENT to BUILDER and return BUILDER.

Signal BOUNDED-SEQUENCE-BUILDER-OVERFLOW without changing accumulated contents
when ELEMENT cannot fit under both budgets."
  (bounded-sequence-builder--ensure-mutable
   builder 'bounded-sequence-builder-append)
  (let ((weight (bounded-sequence-builder--element-weight builder element)))
    (unless (bounded-sequence-builder--fits-p builder 1 weight)
      (bounded-sequence-builder--signal-overflow builder 1 weight))
    (bounded-sequence-builder--append-element builder element weight))
  builder)

(defun bounded-sequence-builder-try-append (builder element)
  "Append ELEMENT to BUILDER when both budgets permit it.

Return true on success. Otherwise leave contents unchanged, mark BUILDER
overflowed, and return NIL without signaling BOUNDED-SEQUENCE-BUILDER-OVERFLOW."
  (bounded-sequence-builder--ensure-mutable
   builder 'bounded-sequence-builder-try-append)
  (let ((weight (bounded-sequence-builder--element-weight builder element)))
    (unless (bounded-sequence-builder--fits-p builder 1 weight)
      (setf (%bounded-sequence-builder-overflowed-p builder) t)
      (return-from bounded-sequence-builder-try-append nil))
    (bounded-sequence-builder--append-element builder element weight)
    t))

(defun bounded-sequence-builder-try-append-sequence
    (builder sequence &key (start 0) end)
  "Append the START to END range of SEQUENCE when both budgets permit it.

Return true on success. Otherwise leave contents unchanged, mark BUILDER
overflowed, and return NIL without signaling BOUNDED-SEQUENCE-BUILDER-OVERFLOW."
  (bounded-sequence-builder--ensure-mutable
   builder 'bounded-sequence-builder-try-append-sequence)
  (multiple-value-bind (snapshot count)
      (bounded-sequence-builder--range-snapshot sequence start end)
    (let ((weight (bounded-sequence-builder--range-weight
                   builder snapshot 0 count)))
      (unless (bounded-sequence-builder--fits-p builder count weight)
        (setf (%bounded-sequence-builder-overflowed-p builder) t)
        (return-from bounded-sequence-builder-try-append-sequence nil))
      (bounded-sequence-builder--append-range
       builder snapshot 0 count weight)
      t)))

(defun bounded-sequence-builder-append-sequence (builder sequence
                                                 &key (start 0) end)
  "Append the START to END range of SEQUENCE atomically to BUILDER.

END defaults to the sequence length. Return the number appended. If the whole
range cannot fit under both budgets, mark BUILDER overflowed and signal
BOUNDED-SEQUENCE-BUILDER-OVERFLOW without changing accumulated contents."
  (bounded-sequence-builder--ensure-mutable
   builder 'bounded-sequence-builder-append-sequence)
  (multiple-value-bind (snapshot count)
      (bounded-sequence-builder--range-snapshot sequence start end)
    (let ((weight (bounded-sequence-builder--range-weight
                   builder snapshot 0 count)))
      (unless (bounded-sequence-builder--fits-p builder count weight)
        (bounded-sequence-builder--signal-overflow builder count weight))
      (bounded-sequence-builder--append-range
       builder snapshot 0 count weight)
      count)))

(defun bounded-sequence-builder-append-sequence-truncating
    (builder sequence &key (start 0) end)
  "Append the longest START to END prefix that fits both BUILDER budgets.

Return the number appended and, as a second value, true when the complete range
fit. If elements are omitted, mark BUILDER overflowed. This operation never
signals BOUNDED-SEQUENCE-BUILDER-OVERFLOW."
  (bounded-sequence-builder--ensure-mutable
   builder 'bounded-sequence-builder-append-sequence-truncating)
  (multiple-value-bind (snapshot count)
      (bounded-sequence-builder--range-snapshot sequence start end)
    (multiple-value-bind (append-end weight)
        (bounded-sequence-builder--fitting-range-end builder snapshot 0 count)
      (let ((complete-p (= append-end count)))
        (bounded-sequence-builder--append-range
         builder snapshot 0 append-end weight)
        (unless complete-p
          (setf (%bounded-sequence-builder-overflowed-p builder) t))
        (values append-end complete-p)))))

(defun bounded-sequence-builder-snapshot (builder)
  "Return a fresh vector containing BUILDER's accumulated elements.

The vector uses BUILDER's upgraded element type and shares no storage with the
builder. Elements themselves are not copied."
  (let* ((count (bounded-sequence-builder-count builder))
         (snapshot (make-array count
                               :element-type
                               (bounded-sequence-builder-element-type
                                builder))))
    (replace snapshot (%bounded-sequence-builder-storage builder)
             :end2 count)
    snapshot))

(defun bounded-sequence-builder-finish (builder)
  "Return a detached snapshot of BUILDER, then clear BUILDER for reuse."
  (bounded-sequence-builder--ensure-mutable
   builder 'bounded-sequence-builder-finish)
  (let ((result (bounded-sequence-builder-snapshot builder)))
    (bounded-sequence-builder-clear builder)
    result))

(defun bounded-sequence-builder-clear (builder)
  "Remove all elements and reset BUILDER's weight and overflow state.

Retain the current internal allocation capacity for reuse and return BUILDER."
  (bounded-sequence-builder--ensure-mutable
   builder 'bounded-sequence-builder-clear)
  (let ((capacity
          (array-total-size (%bounded-sequence-builder-storage builder))))
    (setf (%bounded-sequence-builder-storage builder)
          (make-array capacity
                      :element-type
                      (%bounded-sequence-builder-element-type builder)
                      :adjustable t
                      :fill-pointer 0)
          (%bounded-sequence-builder-total-weight builder) 0
          (%bounded-sequence-builder-overflowed-p builder) nil))
  builder)


;;;; -- Internal Mechanics --

(defun bounded-sequence-builder--ensure-mutable (builder operation)
  "Signal when OPERATION attempts mutation during BUILDER's weight callback."
  (when (%bounded-sequence-builder-weight-callback-active-p builder)
    (error 'bounded-sequence-builder-callback-mutation-error
           :builder builder :operation operation)))


(defun bounded-sequence-builder--range-snapshot (sequence start end)
  "Validate and detach a sequence range, returning its snapshot and length."
  (check-type start (integer 0 *))
  (check-type end (or null (integer 0 *)))
  (let* ((length (length sequence))
         (end (or end length)))
    (unless (<= start end length)
      (error 'type-error
             :datum         (list start end)
             :expected-type `(cons (integer 0 ,length)
                                   (cons (integer ,start ,length) null))))
    (values (subseq sequence start end) (- end start))))

(defun bounded-sequence-builder--check-element (builder element)
  "Signal TYPE-ERROR unless ELEMENT satisfies BUILDER's requested element type."
  (let ((element-type (%bounded-sequence-builder-element-type builder)))
    (unless (typep element element-type)
      (error 'type-error :datum element :expected-type element-type))))

(defun bounded-sequence-builder--element-weight (builder element)
  "Validate ELEMENT and return its non-negative integer weight."
  (bounded-sequence-builder--check-element builder element)
  (let ((weight
          (if (%bounded-sequence-builder-weight-function builder)
              (progn
                (setf (%bounded-sequence-builder-weight-callback-active-p
                       builder)
                      t)
                (unwind-protect
                     (funcall
                      (%bounded-sequence-builder-weight-function builder)
                      element)
                  (setf (%bounded-sequence-builder-weight-callback-active-p
                         builder)
                        nil)))
              0)))
    (unless (typep weight '(integer 0 *))
      (error 'bounded-sequence-builder-weight-error
             :builder builder :element element :weight weight))
    weight))

(defun bounded-sequence-builder--range-weight (builder sequence start end)
  "Validate a sequence range and return its total weight."
  (loop for index from start below end
        sum (bounded-sequence-builder--element-weight
             builder (elt sequence index))))

(defun bounded-sequence-builder--fits-p (builder count weight)
  "Return true when COUNT elements of WEIGHT fit both BUILDER budgets."
  (and (<= count (bounded-sequence-builder-remaining-capacity builder))
       (let ((remaining-weight
               (bounded-sequence-builder-remaining-weight builder)))
         (or (null remaining-weight)
             (<= weight remaining-weight)))))

(defun bounded-sequence-builder--signal-overflow (builder count weight)
  "Mark BUILDER overflowed and signal an exact append failure."
  (setf (%bounded-sequence-builder-overflowed-p builder) t)
  (error 'bounded-sequence-builder-overflow
         :builder            builder
         :requested-count    count
         :requested-weight   weight
         :remaining-capacity
         (bounded-sequence-builder-remaining-capacity builder)
         :remaining-weight
         (bounded-sequence-builder-remaining-weight builder)))

(defun bounded-sequence-builder--ensure-capacity (builder required-count)
  "Grow BUILDER's storage to REQUIRED-COUNT without exceeding its count limit."
  (let* ((storage (%bounded-sequence-builder-storage builder))
         (capacity (array-total-size storage)))
    (when (> required-count capacity)
      (let ((new-capacity
              (min (%bounded-sequence-builder-maximum-count builder)
                   (max required-count 1 (* 2 capacity)))))
        (setf (%bounded-sequence-builder-storage builder)
              (adjust-array storage new-capacity
                            :fill-pointer
                            (bounded-sequence-builder-count builder)))))))

(defun bounded-sequence-builder--append-element (builder element weight)
  "Append one validated, known-to-fit ELEMENT of WEIGHT."
  (bounded-sequence-builder--ensure-capacity
   builder (1+ (bounded-sequence-builder-count builder)))
  (vector-push element (%bounded-sequence-builder-storage builder))
  (incf (%bounded-sequence-builder-total-weight builder) weight))

(defun bounded-sequence-builder--append-range
    (builder sequence start end weight)
  "Append a validated, known-to-fit sequence range of total WEIGHT."
  (let* ((count (- end start))
         (old-count (bounded-sequence-builder-count builder))
         (new-count (+ old-count count)))
    (bounded-sequence-builder--ensure-capacity builder new-count)
    (let ((storage (%bounded-sequence-builder-storage builder)))
      (handler-case
          (progn
            (setf (fill-pointer storage) new-count)
            (replace storage sequence
                     :start1 old-count
                     :end1   new-count
                     :start2 start
                     :end2   end))
        (error (condition)
          (setf (fill-pointer storage) old-count)
          (error condition))))
    (incf (%bounded-sequence-builder-total-weight builder) weight)))

(defun bounded-sequence-builder--fitting-range-end
    (builder sequence start end)
  "Return the exclusive end and weight of the longest fitting sequence prefix."
  (loop with index = start
        with count-limit = (bounded-sequence-builder-remaining-capacity builder)
        with weight-limit = (bounded-sequence-builder-remaining-weight builder)
        with count = 0
        with weight = 0
        while (< index end)
        do
           (when (= count count-limit)
             (return (values index weight)))
           (let* ((element (elt sequence index))
                  (element-weight
                    (bounded-sequence-builder--element-weight builder element)))
             (when (and weight-limit
                        (> (+ weight element-weight) weight-limit))
               (return (values index weight)))
             (incf index)
             (incf count)
             (incf weight element-weight))
        finally (return (values index weight))))
