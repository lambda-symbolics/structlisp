(in-package #:structlisp)

;;;; -- Shared Conditions --

(define-condition missing-weight-function-error (error)
  ((maximum-weight
    :initarg       :maximum-weight
    :reader        missing-weight-function-error-maximum-weight
    :documentation "The requested weight limit."))
  (:report
   (lambda (condition stream)
     (format stream "A maximum weight of ~D requires a weight function."
             (missing-weight-function-error-maximum-weight condition))))
  (:documentation "A constructor received a weight limit without a weight function to measure it."))
