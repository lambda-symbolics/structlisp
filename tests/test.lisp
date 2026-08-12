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


(define-test test-deque-budget-edge-cases
  (let ((back-evicting (make-deque :maximum-count 2
                                    :eviction-end :back
                                    :initial-contents '(a b c))))
    (test-equal #(a b) (deque->vector back-evicting))
    (multiple-value-bind (element evicted)
        (deque-push-front back-evicting 'z)
      (test-equal 'z element)
      (test-equal #(b) evicted))
    (test-equal #(z a) (deque->vector back-evicting)))
  (let ((empty (make-deque :maximum-count 0
                           :initial-contents '(a b))))
    (assert (deque-empty-p empty)))
  (let ((weighted (make-deque :maximum-weight 4
                              :weight-function #'length
                              :initial-contents '("a" "bb"))))
    (setf (deque-ref weighted 0) "aa")
    (test-equal 4 (deque-total-weight weighted))
    (handler-case
        (progn
          (setf (deque-ref weighted 0) "toolong")
          (error "Expected overweight replacement failure."))
      (error ()))
    (test-equal #("aa" "bb") (deque->vector weighted))))

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


(define-test test-sorted-string-index-copies-keys
  (let* ((middle (copy-seq "b"))
         (index (make-sorted-string-index
                 :initial-contents (list "a" middle "c"))))
    (setf (char middle 0) #\z)
    (multiple-value-bind (start end)
        (sorted-string-index-prefix-range index "b")
      (test-equal 1 start)
      (test-equal 2 end))
    (let ((key-copy (sorted-string-index-key-ref index 1)))
      (setf (char key-copy 0) #\x))
    (multiple-value-bind (start end)
        (sorted-string-index-prefix-range index "b")
      (test-equal 1 start)
      (test-equal 2 end))
    (multiple-value-bind (item removed-p)
        (sorted-string-index-remove index middle :test #'eq)
      (assert (eq item middle))
      (assert removed-p))))


(define-test test-ordered-map-order
  (let ((map (make-ordered-map :test 'equal
                               :initial-contents '(("a" . 1) ("b" . 2)))))
    (ordered-map-set map "c" 3)
    (ordered-map-set map "b" 20)
    (test-equal '( ("a" . 1) ("b" . 20) ("c" . 3))
                (ordered-map->alist map))
    (ordered-map-move-to-front map "c")
    (test-equal #("c" "a" "b") (ordered-map-keys map))
    (ordered-map-move-to-back map "a")
    (test-equal #("c" "b" "a") (ordered-map-keys map))))

(define-test test-ordered-map-removal
  (let ((map (make-ordered-map :initial-contents '((a . 1) (b . 2)))))
    (multiple-value-bind (key value present-p)
        (ordered-map-pop-first map)
      (test-equal 'a key)
      (test-equal 1 value)
      (assert present-p))
    (multiple-value-bind (value present-p)
        (ordered-map-delete map 'b)
      (test-equal 2 value)
      (assert present-p))
    (assert (ordered-map-empty-p map))))


(define-test test-ordered-map-snapshot-traversal
  (let ((map (make-ordered-map :initial-contents '((a . 1) (b . 2) (c . 3))))
        (visited nil))
    (ordered-map-map
     (lambda (key value)
       (push (cons key value) visited)
       (when (eq key 'a)
         (ordered-map-clear map)))
     map)
    (test-equal '((a . 1) (b . 2) (c . 3)) (nreverse visited))
    (assert (ordered-map-empty-p map))))


(define-test test-lru-cache-recency-and-count
  (let ((cache (make-lru-cache :maximum-count 2)))
    (lru-cache-put cache 'a 1)
    (lru-cache-put cache 'b 2)
    (lru-cache-get cache 'a)
    (multiple-value-bind (value evicted)
        (lru-cache-put cache 'c 3)
      (test-equal 3 value)
      (test-equal #((b . 2)) evicted))
    (test-equal #(a c) (lru-cache-keys cache))))

(define-test test-lru-cache-weight
  (let* ((evicted nil)
         (cache (make-lru-cache
                 :maximum-weight 4
                 :weight-function (lambda (key value)
                                    (declare (ignore key))
                                    (length value))
                 :eviction-function (lambda (key value)
                                      (push (cons key value) evicted)))))
    (lru-cache-put cache 'a "aa")
    (lru-cache-put cache 'b "bbb")
    (test-equal #(b) (lru-cache-keys cache))
    (test-equal 3 (lru-cache-total-weight cache))
    (test-equal '((a . "aa")) evicted)))


(define-test test-lru-cache-budget-edge-cases
  (let ((cache (make-lru-cache
                :maximum-count 2
                :maximum-weight 4
                :weight-function (lambda (key value)
                                   (declare (ignore key))
                                   (length value)))))
    (lru-cache-put cache 'a "a")
    (lru-cache-put cache 'b "bb")
    (multiple-value-bind (value evicted)
        (lru-cache-put cache 'a "toolong")
      (test-equal "toolong" value)
      (test-equal #((b . "bb") (a . "toolong")) evicted))
    (assert (lru-cache-empty-p cache)))
  (let ((cache (make-lru-cache :maximum-count 0)))
    (multiple-value-bind (value evicted)
        (lru-cache-put cache 'a nil)
      (assert (null value))
      (test-equal #((a)) evicted))
    (assert (lru-cache-empty-p cache)))
  (let ((calls 0)
        (cache (make-memo-cache :maximum-count 0)))
    (loop repeat 2
          do (multiple-value-bind (value hit-p)
                 (memo-cache-get cache 'a
                                 (lambda ()
                                   (incf calls)))
               (assert (null hit-p))
               (test-equal calls value)))
    (test-equal 2 calls)))


(define-test test-lru-cache-snapshot-traversal
  (let ((cache (make-lru-cache))
        (visited nil))
    (lru-cache-put cache 'a 1)
    (lru-cache-put cache 'b 2)
    (lru-cache-map
     (lambda (key value)
       (push (cons key value) visited)
       (when (eq key 'a)
         (lru-cache-clear cache)))
     cache)
    (test-equal '((a . 1) (b . 2)) (nreverse visited))
    (assert (lru-cache-empty-p cache))))

(define-test test-memo-cache
  (let ((calls 0)
        (cache (make-memo-cache :maximum-count 2)))
    (multiple-value-bind (value hit-p)
        (memo-cache-get cache 'answer
                        (lambda ()
                          (incf calls)
                          42))
      (test-equal 42 value)
      (assert (null hit-p)))
    (multiple-value-bind (value hit-p)
        (memo-cache-get cache 'answer
                        (lambda ()
                          (incf calls)
                          99))
      (test-equal 42 value)
      (assert hit-p))
    (test-equal 1 calls)))


(defun test-interval-pairs (intervals)
  (map 'vector
       (lambda (interval)
         (cons (integer-interval-start interval)
               (integer-interval-end interval)))
       intervals))

(defun test-interval-map-triples (entries)
  (map 'vector
       (lambda (entry)
         (list (integer-interval-entry-start entry)
               (integer-interval-entry-end entry)
               (integer-interval-entry-value entry)))
       entries))

(define-test test-integer-interval-set
  (let ((set (make-integer-interval-set)))
    (integer-interval-set-add set 5 10)
    (integer-interval-set-add set 1 3)
    (integer-interval-set-add set 3 5)
    (test-equal #((1 . 10))
                (test-interval-pairs (integer-interval-set->vector set)))
    (integer-interval-set-remove set 4 7)
    (test-equal #((1 . 4) (7 . 10))
                (test-interval-pairs (integer-interval-set->vector set)))
    (assert (integer-interval-set-contains-p set 3))
    (assert (null (integer-interval-set-contains-p set 4)))
    (assert (integer-interval-set-covers-p set 7 10))
    (assert (integer-interval-set-intersects-p set 9 12))))

(define-test test-integer-interval-set-operations
  (let* ((left (make-integer-interval-set :intervals '((1 . 5) (8 . 10))))
         (right (make-integer-interval-set :intervals '((3 . 9))))
         (union (integer-interval-set-union left right))
         (intersection (integer-interval-set-intersection left right))
         (difference (integer-interval-set-difference left right)))
    (test-equal #((1 . 10))
                (test-interval-pairs
                 (integer-interval-set->vector union)))
    (test-equal #((3 . 5) (8 . 9))
                (test-interval-pairs
                 (integer-interval-set->vector intersection)))
    (test-equal #((1 . 3) (9 . 10))
                (test-interval-pairs
                 (integer-interval-set->vector difference)))))

(define-test test-integer-interval-map
  (let ((map (make-integer-interval-map)))
    (integer-interval-map-set map 0 10 'base)
    (integer-interval-map-set map 3 7 'middle)
    (test-equal #((0 3 base) (3 7 middle) (7 10 base))
                (test-interval-map-triples
                 (integer-interval-map->vector map)))
    (multiple-value-bind (value start end present-p)
        (integer-interval-map-get map 5)
      (test-equal 'middle value)
      (test-equal 3 start)
      (test-equal 7 end)
      (assert present-p))
    (integer-interval-map-delete map 4 8)
    (test-equal #((0 3 base) (3 4 middle) (8 10 base))
                (test-interval-map-triples
                 (integer-interval-map->vector map)))))


(define-test test-integer-interval-map-linear-rewrites
  (let ((map (make-integer-interval-map
              :entries '((0 2 a) (2 4 a) (6 8 b)))))
    (test-equal #((0 4 a) (6 8 b))
                (test-interval-map-triples
                 (integer-interval-map->vector map)))
    (integer-interval-map-set map 4 6 'a)
    (test-equal #((0 6 a) (6 8 b))
                (test-interval-map-triples
                 (integer-interval-map->vector map)))
    (integer-interval-map-delete map 1 7)
    (test-equal #((0 1 a) (7 8 b))
                (test-interval-map-triples
                 (integer-interval-map->vector map)))))


(define-test test-monotone-integer-index-search
  (let ((index (make-monotone-integer-index
                :element-bits 32
                :initial-contents '(1 3 3 8 13))))
    (test-equal 1 (monotone-integer-index-lower-bound index 3))
    (test-equal 3 (monotone-integer-index-upper-bound index 3))
    (multiple-value-bind (position present-p)
        (monotone-integer-index-find index 8)
      (test-equal 3 position)
      (assert present-p))
    (multiple-value-bind (value position present-p)
        (monotone-integer-index-at-or-before index 7)
      (test-equal 3 value)
      (test-equal 2 position)
      (assert present-p))
    (multiple-value-bind (value position present-p)
        (monotone-integer-index-at-or-after index 4)
      (test-equal 8 value)
      (test-equal 3 position)
      (assert present-p))))

(define-test test-monotone-integer-index-failures
  (let ((index (make-monotone-integer-index :element-bits 8
                                             :initial-contents '(1 2))))
    (handler-case
        (progn
          (monotone-integer-index-append index 0)
          (error "Expected MONOTONE-INTEGER-INDEX-ORDER-ERROR."))
      (monotone-integer-index-order-error ()))
    (handler-case
        (progn
          (monotone-integer-index-append index 256)
          (error "Expected MONOTONE-INTEGER-INDEX-VALUE-ERROR."))
      (monotone-integer-index-value-error ()))))

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
