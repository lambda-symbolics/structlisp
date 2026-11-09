(in-package #:structlisp)

(define-test test-layered-registry-replacement-and-removal
  (let* ((registrations '((a :base 1) (b :user 2) (a :user 3)))
         (original      (copy-tree registrations)))
    (dolist (case '(((a :base 4) :base ((a :base 4) (b :user 2) (a :user 3)))
                    ((a :user 4) :user ((a :base 1) (b :user 2) (a :user 4)))
                    ((c :base 4) :base ((a :base 1) (b :user 2) (a :user 3)
                                        (c :base 4)))))
      (test-equal
       (third case)
       (layered-registry-replace registrations (first case)
                                 :key-function    #'first
                                 :source-function #'second
                                 :key-test #'eq :source (second case))))
    (test-equal '((a :base 1) (b :user 2))
                (layered-registry-remove registrations 'a
                                         :key-function    #'first
                                         :source-function #'second
                                         :key-test #'eq :source :user))
    (test-equal '((a :base 1))
                (layered-registry-remove-source registrations :user #'second))
    (test-equal registrations
                (layered-registry-remove-source registrations :absent #'second))
    (test-equal original registrations)))

(define-test test-layered-registry-effective-order
  (let ((registrations '(("a" 1 first) ("b" 2 first) ("a" 3 higher)
                         ("c" 0 first) ("b" 2 later) ("a" 2 lower))))
    (test-equal '(("a" 3 higher) ("b" 2 later) ("c" 0 first))
                (layered-registry-effective registrations #'first #'second)))
  (assert (null (layered-registry-effective nil #'first #'second))))

(define-test test-layered-registry-restore-position
  (let* ((registrations '((a current) (b kept) (a duplicate) (c kept)))
         (original      (copy-tree registrations))
         (registration  (list 'a (list 'saved))))
    (dolist (case '((0 ((a (saved)) (b kept) (c kept)))
                    (1 ((b kept) (a (saved)) (c kept)))
                    (9 ((b kept) (c kept) (a (saved))))))
      (let* ((snapshot (list :position (first case) :registration registration))
             (restored (layered-registry-restore-position
                        registrations 'a snapshot
                        :key-function #'first :key-test #'eq
                        :copy-function #'copy-tree))
             (entry    (find 'a restored :key #'first)))
        (test-equal (second case) restored)
        (assert (not (eq registration entry)))
        (setf (first (second entry)) 'changed)
        (test-equal '(a (saved)) registration)))
    (test-equal '((b kept) (c kept))
                (layered-registry-restore-position
                 registrations 'a nil
                 :key-function #'first :key-test #'eq
                 :copy-function (lambda (entry)
                                  (declare (ignore entry))
                                  (error "Deletion must not copy."))))
    (test-equal original registrations)))

(define-test test-registry-lifecycle-callbacks
  (let* ((events nil)
         (registry
           (make-registry-lifecycle
            :snapshot-function      (lambda ()
                                      (push :snapshot events)
                                      (values nil :snapshot-result))
            :restore-function       (lambda (snapshot)
                                      (push (list :restore snapshot) events)
                                      (values :restored snapshot))
            :remove-source-function (lambda (source)
                                      (push (list :remove source) events)
                                      (values :removed source)))))
    (test-equal '(nil :snapshot-result)
                (multiple-value-list (registry-snapshot registry)))
    (test-equal '(:restored nil)
                (multiple-value-list (registry-restore registry nil)))
    (test-equal '(:removed :user)
                (multiple-value-list (registry-remove-source registry :user)))
    (test-equal '(:snapshot (:restore nil) (:remove :user)) (reverse events))))

(define-test test-registry-lifecycle-callback-validation
  (let ((valid (list :snapshot-function      (constantly nil)
                     :restore-function       #'identity
                     :remove-source-function #'identity)))
    (dolist (key '(:snapshot-function :restore-function :remove-source-function))
      (dolist (value '(nil identity 17))
        (let ((arguments (copy-list valid)))
          (setf (getf arguments key) value)
          (assert (handler-case
                      (progn (apply #'make-registry-lifecycle arguments) nil)
                    (type-error () t)))))
      (let ((arguments (copy-list valid)))
        (remf arguments key)
        (assert (handler-case
                    (progn (apply #'make-registry-lifecycle arguments) nil)
                  (type-error () t)))))))

(define-test test-registry-lifecycle-failure-propagation
  (dolist (operation '(registry-snapshot registry-restore registry-remove-source))
    (let* ((failure (make-condition 'simple-error :format-control "Callback failure."))
           (calls   0)
           (registry
             (make-registry-lifecycle
              :snapshot-function      (lambda () (incf calls) (error failure))
              :restore-function       (lambda (snapshot)
                                        (declare (ignore snapshot))
                                        (incf calls) (error failure))
              :remove-source-function (lambda (source)
                                        (declare (ignore source))
                                        (incf calls) (error failure)))))
      (assert (eq failure
                  (handler-case
                      (if (eq operation 'registry-snapshot)
                          (funcall operation registry)
                          (funcall operation registry nil))
                    (simple-error (condition) condition))))
      (test-equal 1 calls))))
