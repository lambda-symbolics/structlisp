;;;; structlisp.asd

(in-package #:asdf-user)

(defsystem "structlisp"
  :description "General-purpose data structures for interactive Common Lisp systems."
  :author "Lukáš Hozda"
  :license "COLL-Attribution"
  :version "0.1.0"
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "deque")
               (:file "priority-queue")
               (:file "sorted-string-index")
               (:file "ordered-map")
               (:file "lru-cache"))
  :in-order-to ((test-op (test-op "structlisp/tests"))))

(defsystem "structlisp/tests"
  :description "Tests for Structlisp."
  :depends-on ("structlisp")
  :pathname "tests"
  :serial t
  :components ((:file "test"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:structlisp '#:run-tests)
               (error "Structlisp tests failed."))))
