;;;; bounded-sequence-builder-test.lisp

(in-package #:structlisp)


;;;; -- Budgets and Append Semantics --

(define-test test-bounded-sequence-builder-count-budgets
  (dolist (maximum-count '(0 1 3))
    (let ((builder (make-bounded-sequence-builder maximum-count)))
      (test-equal 0 (bounded-sequence-builder-count builder))
      (test-equal maximum-count
                  (bounded-sequence-builder-remaining-capacity builder))
      (test-equal maximum-count
                  (bounded-sequence-builder-maximum-count builder))
      (when (zerop maximum-count)
        (assert (null
                 (bounded-sequence-builder-try-append builder 'overflow))))))
  (let ((builder (make-bounded-sequence-builder 2)))
    (assert (bounded-sequence-builder-try-append builder nil))
    (bounded-sequence-builder-append builder 'value)
    (test-equal #(nil value)
                (bounded-sequence-builder-snapshot builder))
    (test-equal 2 (bounded-sequence-builder-count builder))
    (test-equal 0 (bounded-sequence-builder-remaining-capacity builder))
    (assert (null (bounded-sequence-builder-try-append builder 'overflow)))
    (assert (bounded-sequence-builder-overflowed-p builder))
    (test-equal #(nil value)
                (bounded-sequence-builder-snapshot builder))))

(define-test test-bounded-sequence-builder-weight-budgets
  (let ((builder (make-bounded-sequence-builder
                  6 :maximum-weight 5 :weight-function #'length)))
    (test-equal 5 (bounded-sequence-builder-remaining-weight builder))
    (bounded-sequence-builder-append builder "aa")
    (bounded-sequence-builder-append-sequence builder '("b" "cc"))
    (test-equal 5 (bounded-sequence-builder-total-weight builder))
    (test-equal 0 (bounded-sequence-builder-remaining-weight builder))
    (assert (bounded-sequence-builder-try-append builder ""))
    (test-equal 5 (bounded-sequence-builder-total-weight builder))
    (test-equal #("aa" "b" "cc" "")
                (bounded-sequence-builder-snapshot builder)))
  (let ((builder (make-bounded-sequence-builder
                  5 :maximum-weight 5 :weight-function #'length)))
    (multiple-value-bind (count complete-p)
        (bounded-sequence-builder-append-sequence-truncating
         builder '("aa" "bbb" "c"))
      (test-equal 2 count)
      (assert (null complete-p)))
    (test-equal 5 (bounded-sequence-builder-total-weight builder))
    (test-equal #("aa" "bbb")
                (bounded-sequence-builder-snapshot builder))))

(define-test test-bounded-sequence-builder-zero-weight-budget
  (handler-case
      (progn
        (make-bounded-sequence-builder 2 :maximum-weight 0)
        (error "Expected missing weight function failure."))
    (error ()))
  (let ((builder (make-bounded-sequence-builder
                  3 :maximum-weight 0 :weight-function #'length)))
    (test-equal 0 (bounded-sequence-builder-maximum-weight builder))
    (test-equal 0 (bounded-sequence-builder-remaining-weight builder))
    (assert (bounded-sequence-builder-try-append builder ""))
    (handler-case
        (progn
          (bounded-sequence-builder-append builder "x")
          (error "Expected BOUNDED-SEQUENCE-BUILDER-OVERFLOW."))
      (bounded-sequence-builder-overflow (condition)
        (test-equal 0
                    (bounded-sequence-builder-overflow-remaining-weight
                     condition))))
    (multiple-value-bind (count complete-p)
        (bounded-sequence-builder-append-sequence-truncating
         builder '("" "y" ""))
      (test-equal 1 count)
      (assert (null complete-p)))
    (test-equal #("" "")
                (bounded-sequence-builder-snapshot builder))
    (test-equal 0 (bounded-sequence-builder-total-weight builder))))

(define-test test-bounded-sequence-builder-exact-fills
  (let ((characters (make-bounded-sequence-builder
                     4 :element-type 'character :initial-capacity 1)))
    (test-equal 4
                (bounded-sequence-builder-append-sequence
                 characters "xabcx" :start 1 :end 5))
    (test-equal "abcx" (bounded-sequence-builder-snapshot characters))
    (test-equal 4 (bounded-sequence-builder-count characters))
    (test-equal 0
                (bounded-sequence-builder-remaining-capacity characters))
    (assert (null (bounded-sequence-builder-overflowed-p characters))))
  (let ((builder (make-bounded-sequence-builder
                  5 :maximum-weight 3 :weight-function #'length)))
    (test-equal 2
                (bounded-sequence-builder-append-sequence
                 builder '("a" "bb")))
    (test-equal 3 (bounded-sequence-builder-total-weight builder))
    (test-equal 0 (bounded-sequence-builder-remaining-weight builder))
    (assert (null (bounded-sequence-builder-overflowed-p builder)))))

(define-test test-bounded-sequence-builder-exact-overflow
  (let ((builder (make-bounded-sequence-builder
                  3 :maximum-weight 4 :weight-function #'length)))
    (bounded-sequence-builder-append builder "aa")
    (handler-case
        (progn
          (bounded-sequence-builder-append-sequence builder '("b" "cc"))
          (error "Expected BOUNDED-SEQUENCE-BUILDER-OVERFLOW."))
      (bounded-sequence-builder-overflow (condition)
        (assert (eq builder
                    (bounded-sequence-builder-overflow-builder condition)))
        (test-equal 2
                    (bounded-sequence-builder-overflow-requested-count
                     condition))
        (test-equal 3
                    (bounded-sequence-builder-overflow-requested-weight
                     condition))
        (test-equal 2
                    (bounded-sequence-builder-overflow-remaining-capacity
                     condition))
        (test-equal 2
                    (bounded-sequence-builder-overflow-remaining-weight
                     condition))))
    (assert (bounded-sequence-builder-overflowed-p builder))
    (test-equal #("aa") (bounded-sequence-builder-snapshot builder))
    (test-equal 2 (bounded-sequence-builder-total-weight builder)))
  (let ((builder (make-bounded-sequence-builder 0)))
    (handler-case
        (progn
          (bounded-sequence-builder-append builder 'value)
          (error "Expected BOUNDED-SEQUENCE-BUILDER-OVERFLOW."))
      (bounded-sequence-builder-overflow (condition)
        (test-equal 1
                    (bounded-sequence-builder-overflow-requested-count
                     condition))
        (test-equal 0
                    (bounded-sequence-builder-overflow-remaining-capacity
                     condition))
        (assert (null
                 (bounded-sequence-builder-overflow-remaining-weight
                  condition)))))
    (test-equal #() (bounded-sequence-builder-snapshot builder))))

(define-test test-bounded-sequence-builder-ranges-and-truncation
  (let ((builder (make-bounded-sequence-builder 5)))
    (test-equal 3
                (bounded-sequence-builder-append-sequence
                 builder #(ignored a b c ignored) :start 1 :end 4))
    (multiple-value-bind (count complete-p)
        (bounded-sequence-builder-append-sequence-truncating
         builder '(d e f) :start 0 :end 3)
      (test-equal 2 count)
      (assert (null complete-p)))
    (test-equal #(a b c d e)
                (bounded-sequence-builder-snapshot builder))
    (assert (bounded-sequence-builder-overflowed-p builder)))
  (let ((builder (make-bounded-sequence-builder 2)))
    (multiple-value-bind (count complete-p)
        (bounded-sequence-builder-append-sequence-truncating
         builder #(outside x outside) :start 1 :end 2)
      (test-equal 1 count)
      (assert complete-p))
    (test-equal 0
                (bounded-sequence-builder-append-sequence
                 builder #() :start 0 :end 0))
    (multiple-value-bind (count complete-p)
        (bounded-sequence-builder-append-sequence-truncating
         builder #() :start 0 :end 0)
      (test-equal 0 count)
      (assert complete-p))
    (assert (null (bounded-sequence-builder-overflowed-p builder)))))


;;;; -- Element Types and Storage --

(define-test test-bounded-sequence-builder-element-types
  (let ((characters (make-bounded-sequence-builder
                     5 :element-type 'character :initial-capacity 0))
        (octets (make-bounded-sequence-builder
                 4 :element-type '(unsigned-byte 8) :initial-capacity 1)))
    (bounded-sequence-builder-append-sequence characters "abc")
    (bounded-sequence-builder-append characters #\d)
    (test-equal "abcd" (bounded-sequence-builder-snapshot characters))
    (assert (stringp (bounded-sequence-builder-snapshot characters)))
    (bounded-sequence-builder-append-sequence octets #(0 1 254 255))
    (let ((snapshot (bounded-sequence-builder-snapshot octets)))
      (test-equal #(0 1 254 255) snapshot)
      (test-equal (array-element-type snapshot)
                  (bounded-sequence-builder-element-type octets)))
    (handler-case
        (progn
          (bounded-sequence-builder-append
           (make-bounded-sequence-builder 1
                                          :element-type '(unsigned-byte 8))
           256)
          (error "Expected TYPE-ERROR."))
      (type-error ())))
  (let ((builder (make-bounded-sequence-builder
                  4 :element-type '(or null symbol))))
    (bounded-sequence-builder-append-sequence builder '(nil one nil two))
    (test-equal #(nil one nil two)
                (bounded-sequence-builder-snapshot builder))))

(define-test test-bounded-sequence-builder-typed-finish
  (let ((characters (make-bounded-sequence-builder
                     3 :element-type 'character))
        (octets (make-bounded-sequence-builder
                 3 :element-type '(unsigned-byte 8))))
    (bounded-sequence-builder-append-sequence characters "abc")
    (bounded-sequence-builder-append-sequence octets #(1 2 3))
    (let ((string (bounded-sequence-builder-finish characters))
          (vector (bounded-sequence-builder-finish octets)))
      (assert (stringp string))
      (test-equal "abc" string)
      (test-equal #(1 2 3) vector)
      (test-equal (bounded-sequence-builder-element-type octets)
                  (array-element-type vector))
      (bounded-sequence-builder-append characters #\z)
      (bounded-sequence-builder-append octets 9)
      (test-equal "abc" string)
      (test-equal #(1 2 3) vector))))

(define-test test-bounded-sequence-builder-adjustment-and-reset
  (let ((builder (make-bounded-sequence-builder
                  10 :initial-capacity 20
                  :maximum-weight 20
                  :weight-function (lambda (element)
                                     (declare (ignore element))
                                     2))))
    (test-equal 10 (bounded-sequence-builder-capacity builder))
    (bounded-sequence-builder-clear builder)
    (let ((growing (make-bounded-sequence-builder 10 :initial-capacity 0)))
      (test-equal 0 (bounded-sequence-builder-capacity growing))
      (loop for element across #(a b c d e f g)
            do (bounded-sequence-builder-append growing element))
      (assert (<= 7 (bounded-sequence-builder-capacity growing) 10))
      (bounded-sequence-builder-append-sequence-truncating
       growing #(h i j k))
      (assert (bounded-sequence-builder-overflowed-p growing))
      (let ((capacity (bounded-sequence-builder-capacity growing)))
        (assert (eq growing (bounded-sequence-builder-clear growing)))
        (test-equal 0 (bounded-sequence-builder-count growing))
        (test-equal 0 (bounded-sequence-builder-total-weight growing))
        (test-equal 10
                    (bounded-sequence-builder-remaining-capacity growing))
        (test-equal capacity (bounded-sequence-builder-capacity growing))
        (assert (null (bounded-sequence-builder-overflowed-p growing)))))))


;;;; -- Snapshots, Finish, and Failures --

(define-test test-bounded-sequence-builder-detached-results
  (let ((builder (make-bounded-sequence-builder 4)))
    (bounded-sequence-builder-append-sequence builder #(a b))
    (let ((snapshot (bounded-sequence-builder-snapshot builder)))
      (setf (aref snapshot 0) 'changed)
      (bounded-sequence-builder-append builder 'c)
      (test-equal #(a b c) (bounded-sequence-builder-snapshot builder))
      (test-equal #(changed b) snapshot))
    (bounded-sequence-builder-append-sequence-truncating builder #(d e))
    (assert (bounded-sequence-builder-overflowed-p builder))
    (let ((finished (bounded-sequence-builder-finish builder)))
      (test-equal #(a b c d) finished)
      (test-equal 0 (bounded-sequence-builder-count builder))
      (assert (null (bounded-sequence-builder-overflowed-p builder)))
      (setf (aref finished 1) 'changed)
      (bounded-sequence-builder-append builder 'new)
      (test-equal #(new) (bounded-sequence-builder-snapshot builder))
      (test-equal #(a changed c d) finished))))

(define-test test-bounded-sequence-builder-clear-detaches-storage
  (let* ((value (list 'retained-only-by-old-storage))
         (builder (make-bounded-sequence-builder 4))
         (storage nil))
    (bounded-sequence-builder-append builder value)
    (setf storage (%bounded-sequence-builder-storage builder))
    (bounded-sequence-builder-clear builder)
    (assert (not (eq storage
                     (%bounded-sequence-builder-storage builder))))
    (test-equal 4 (bounded-sequence-builder-capacity builder))
    (test-equal #() (bounded-sequence-builder-snapshot builder))))

(define-test test-bounded-sequence-builder-reentrant-weight-callback
  (let ((builder nil)
        (attempt-mutation-p t))
    (setf builder
          (make-bounded-sequence-builder
           2
           :maximum-weight 2
           :weight-function
           (lambda (element)
             (declare (ignore element))
             (when attempt-mutation-p
               (setf attempt-mutation-p nil)
               (bounded-sequence-builder-append builder 'side))
             1)))
    (handler-case
        (progn
          (bounded-sequence-builder-append builder 'outer)
          (error "Expected callback mutation failure."))
      (bounded-sequence-builder-callback-mutation-error (condition)
        (assert (eq builder
                    (bounded-sequence-builder-callback-mutation-error-builder
                     condition)))
        (test-equal 'bounded-sequence-builder-append
                    (bounded-sequence-builder-callback-mutation-error-operation
                     condition))))
    (test-equal #() (bounded-sequence-builder-snapshot builder))
    (bounded-sequence-builder-append builder 'after)
    (test-equal #(after) (bounded-sequence-builder-snapshot builder))))

(define-test test-bounded-sequence-builder-atomic-failures
  (let ((builder (make-bounded-sequence-builder
                  4 :element-type '(unsigned-byte 8))))
    (bounded-sequence-builder-append builder 1)
    (handler-case
        (progn
          (bounded-sequence-builder-append-sequence builder #(2 bad 3))
          (error "Expected TYPE-ERROR."))
      (type-error ()))
    (test-equal #(1) (bounded-sequence-builder-snapshot builder))
    (handler-case
        (progn
          (bounded-sequence-builder-append-sequence-truncating
           builder #(2 bad 3))
          (error "Expected TYPE-ERROR."))
      (type-error ()))
    (test-equal #(1) (bounded-sequence-builder-snapshot builder)))
  (let ((builder (make-bounded-sequence-builder
                  3 :element-type '(unsigned-byte 8))))
    (multiple-value-bind (count complete-p)
        (bounded-sequence-builder-append-sequence-truncating
         builder #(1 2 3 bad))
      (test-equal 3 count)
      (assert (null complete-p)))
    (test-equal #(1 2 3)
                (bounded-sequence-builder-snapshot builder))))

(define-test test-bounded-sequence-builder-invalid-weights-and-ranges
  (let ((builder (make-bounded-sequence-builder
                  3 :maximum-weight 4
                  :weight-function (lambda (element)
                                     (if (eq element 'bad) -1 1)))))
    (bounded-sequence-builder-append builder 'good)
    (handler-case
        (progn
          (bounded-sequence-builder-append-sequence builder '(good bad))
          (error "Expected BOUNDED-SEQUENCE-BUILDER-WEIGHT-ERROR."))
      (bounded-sequence-builder-weight-error (condition)
        (assert (eq builder
                    (bounded-sequence-builder-weight-error-builder condition)))
        (test-equal 'bad
                    (bounded-sequence-builder-weight-error-element condition))
        (test-equal -1
                    (bounded-sequence-builder-weight-error-weight condition))))
    (test-equal #(good) (bounded-sequence-builder-snapshot builder))
    (test-equal 1 (bounded-sequence-builder-total-weight builder)))
  (let ((builder (make-bounded-sequence-builder 3)))
    (dolist (range '((2 1) (0 4)))
      (handler-case
          (progn
            (bounded-sequence-builder-append-sequence
             builder #(a b c) :start (first range) :end (second range))
            (error "Expected TYPE-ERROR."))
        (type-error ())))
    (test-equal #() (bounded-sequence-builder-snapshot builder))))
