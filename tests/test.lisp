;;;; test.lisp

(in-package #:structlisp)

(defvar *tests* nil
  "Registered Structlisp tests.")

(defmacro define-test (name &body body)
  "Define and register a test named NAME."
  `(progn
     (defun ,name ()
       ,@body)
     (pushnew ',name *tests*)))

(defun test-equal (expected actual)
  (assert (equalp expected actual) ()
          "Expected ~S, got ~S." expected actual))

(define-test test-deque-end-operations
  (let ((deque (make-deque :initial-capacity 1)))
    (deque-push-back deque 2)
    (deque-push-front deque 1)
    (deque-push-back deque 3)
    (test-equal #(1 2 3) (deque->vector deque))
    (multiple-value-bind (element present-p) (deque-pop-front deque)
      (test-equal 1 element)
      (assert present-p))
    (multiple-value-bind (element present-p) (deque-pop-back deque)
      (test-equal 3 element)
      (assert present-p))
    (test-equal #(2) (deque->vector deque))))

(define-test test-deque-indexed-operations
  (let ((deque (make-deque :initial-contents '(a b d))))
    (deque-insert deque 2 'c)
    (test-equal #(a b c d) (deque->vector deque))
    (test-equal 'b (deque-remove-at deque 1))
    (setf (deque-ref deque 1) 'see)
    (test-equal #(a see d) (deque->vector deque))))

(define-test test-deque-split
  (let* ((deque (make-deque :initial-contents '(0 1 2 3)))
         (suffix (deque-split-at deque 2)))
    (test-equal #(0 1) (deque->vector deque))
    (test-equal #(2 3) (deque->vector suffix))))

(define-test test-deque-budgets
  (let ((deque (make-deque :maximum-count 3
                           :maximum-weight 5
                           :weight-function #'length)))
    (deque-push-back deque "aa")
    (deque-push-back deque "b")
    (multiple-value-bind (element evicted)
        (deque-push-back deque "ccc")
      (test-equal "ccc" element)
      (test-equal #("aa") evicted))
    (test-equal 4 (deque-total-weight deque))
    (test-equal #("b" "ccc") (deque->vector deque))
    (multiple-value-bind (element evicted)
        (deque-push-back deque "123456")
      (test-equal "123456" element)
      (test-equal #("b" "ccc" "123456") evicted))
    (assert (deque-empty-p deque))))

(define-test test-deque-wrap-and-shift
  (let ((deque (make-deque :initial-capacity 4
                           :initial-contents '(0 1 2 3))))
    (deque-pop-front deque)
    (deque-pop-front deque)
    (deque-push-back deque 4)
    (deque-push-back deque 5)
    (deque-insert deque 1 'x)
    (test-equal #(2 x 3 4 5) (deque->vector deque))
    (test-equal '4 (deque-remove-at deque 3))
    (test-equal #(2 x 3 5) (deque->vector deque))))

(define-test test-deque-failures
  (let ((deque (make-deque)))
    (assert (null (nth-value 1 (deque-pop-front deque :empty))))
    (handler-case
        (progn
          (deque-ref deque 0)
          (error "Expected DEQUE-INDEX-ERROR."))
      (deque-index-error ()))
    (let ((bad (make-deque :weight-function (lambda (element)
                                              (declare (ignore element))
                                              -1))))
      (handler-case
          (progn
            (deque-push-back bad 'x)
            (error "Expected DEQUE-WEIGHT-ERROR."))
        (deque-weight-error ())))))


(define-test test-priority-queue-stability
  (let ((queue (make-priority-queue)))
    (priority-queue-push queue 'late 5)
    (priority-queue-push queue 'first 1)
    (priority-queue-push queue 'second 1)
    (test-equal 'first (priority-queue-pop queue))
    (test-equal 'second (priority-queue-pop queue))
    (test-equal 'late (priority-queue-pop queue))
    (assert (priority-queue-empty-p queue))))

(define-test test-priority-queue-cancel-and-change
  (let ((queue (make-priority-queue :key-function #'first :key-test 'equal)))
    (priority-queue-push queue '("a" value-a) 10)
    (priority-queue-push queue '("b" value-b) 20)
    (multiple-value-bind (item changed-p)
        (priority-queue-change-priority queue "b" 1)
      (test-equal '("b" value-b) item)
      (assert changed-p))
    (test-equal '("b" value-b) (priority-queue-pop queue))
    (multiple-value-bind (item priority removed-p)
        (priority-queue-cancel queue "a")
      (test-equal '("a" value-a) item)
      (test-equal 10 priority)
      (assert removed-p))))

(define-test test-priority-queue-duplicate-key
  (let ((queue (make-priority-queue)))
    (priority-queue-push queue 'a 1 :key nil)
    (handler-case
        (progn
          (priority-queue-push queue 'b 2 :key nil)
          (error "Expected PRIORITY-QUEUE-DUPLICATE-KEY."))
      (priority-queue-duplicate-key ()))))

(define-test test-top-k
  (test-equal #(1 2 3) (top-k '(8 2 5 1 3 9) 3))
  (test-equal #((d . 1) (b . 2))
              (top-k '((a . 4) (b . 2) (c . 3) (d . 1))
                     2
                     :key #'rest)))

(define-test test-sorted-string-index-prefixes
  (let ((index (make-sorted-string-index
                :initial-contents '("Beta" "alpine" "alpha" "alphabet")
                :normalizer #'string-downcase)))
    (test-equal #("alpha" "alphabet" "alpine" "Beta")
                (sorted-string-index->vector index))
    (multiple-value-bind (start end)
        (sorted-string-index-prefix-range index "ALP")
      (test-equal 0 start)
      (test-equal 3 end))
    (test-equal #("alpha" "alphabet")
                (sorted-string-index-prefix-items index "alph"))))

(define-test test-sorted-string-index-mutation
  (let ((index (make-sorted-string-index :initial-contents '("b" "d"))))
    (test-equal 1 (sorted-string-index-insert index "c"))
    (test-equal 0 (sorted-string-index-insert index "a"))
    (test-equal #("a" "b" "c" "d")
                (sorted-string-index->vector index))
    (multiple-value-bind (item removed-p)
        (sorted-string-index-remove index "c" :test #'string=)
      (test-equal "c" item)
      (assert removed-p))
    (test-equal #("a" "b" "d")
                (sorted-string-index->vector index))))

(defun run-tests ()
  "Run the complete Structlisp test suite and return true on success."
  (let ((failures nil))
    (dolist (test (reverse *tests*))
      (handler-case
          (funcall test)
        (error (condition)
          (push (cons test condition) failures))))
    (when failures
      (dolist (failure (reverse failures))
        (format *error-output* "~&~S failed: ~A~%"
                (first failure) (rest failure)))
      (return-from run-tests nil))
    t))
