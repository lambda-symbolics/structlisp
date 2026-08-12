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
   ;; Priority queue
   #:priority-queue
   #:make-priority-queue
   #:priority-queue-count
   #:priority-queue-empty-p
   #:priority-queue-push
   #:priority-queue-peek
   #:priority-queue-pop
   #:priority-queue-cancel
   #:priority-queue-change-priority
   #:priority-queue-clear
   #:top-k
   #:priority-queue-error
   #:priority-queue-duplicate-key
   #:priority-queue-duplicate-key-key
   ;; Tests
   #:run-tests))

(in-package #:structlisp)
