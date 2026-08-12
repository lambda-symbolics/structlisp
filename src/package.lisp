;;;; package.lisp

(defpackage #:structlisp
  (:use #:cl)
  (:export
   ;; Deque
   #:deque
   #:make-deque
   #:deque-count
   #:deque-empty-p
   #:deque-capacity
   #:deque-total-weight
   #:deque-maximum-count
   #:deque-maximum-weight
   #:deque-weight-function
   #:deque-eviction-end
   #:deque-ref
   #:deque-front
   #:deque-back
   #:deque-push-front
   #:deque-push-back
   #:deque-pop-front
   #:deque-pop-back
   #:deque-insert
   #:deque-remove-at
   #:deque-split-at
   #:deque-clear
   #:deque->vector
   #:deque-error
   #:deque-index-error
   #:deque-index-error-index
   #:deque-index-error-minimum
   #:deque-index-error-maximum
   #:deque-empty-error
   #:deque-weight-error
   #:deque-weight-error-element
   #:deque-weight-error-weight
   ;; Tests
   #:run-tests))

(in-package #:structlisp)
