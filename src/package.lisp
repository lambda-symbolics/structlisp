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
   ;; Sorted string index
   #:sorted-string-index
   #:make-sorted-string-index
   #:sorted-string-index-count
   #:sorted-string-index-empty-p
   #:sorted-string-index-ref
   #:sorted-string-index-key-ref
   #:sorted-string-index-lower-bound
   #:sorted-string-index-upper-bound
   #:sorted-string-index-equal-range
   #:sorted-string-index-prefix-range
   #:sorted-string-index-prefix-items
   #:sorted-string-index-insert
   #:sorted-string-index-remove-at
   #:sorted-string-index-remove
   #:sorted-string-index->vector
   #:sorted-string-index-error
   #:sorted-string-index-key-error
   #:sorted-string-index-key-error-item
   #:sorted-string-index-key-error-key
   ;; Ordered map
   #:ordered-map
   #:make-ordered-map
   #:ordered-map-count
   #:ordered-map-empty-p
   #:ordered-map-get
   #:ordered-map-set
   #:ordered-map-delete
   #:ordered-map-first
   #:ordered-map-last
   #:ordered-map-pop-first
   #:ordered-map-pop-last
   #:ordered-map-move-to-front
   #:ordered-map-move-to-back
   #:ordered-map-map
   #:ordered-map-keys
   #:ordered-map-values
   #:ordered-map->alist
   #:ordered-map-clear
   ;; Tests
   #:run-tests))

(in-package #:structlisp)
