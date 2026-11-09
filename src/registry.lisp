(in-package #:structlisp)

;;;; -- Layered Registry Mechanics --

(defun layered-registry-replace
    (registrations replacement &key key-function source-function key-test source)
  "Return REGISTRATIONS with REPLACEMENT replacing its source/key layer.
The replacement keeps the existing position; otherwise it is appended."
  (let ((position
          (position-if
           (lambda (registration)
             (and (funcall key-test
                           (funcall key-function registration)
                           (funcall key-function replacement))
                  (eq source (funcall source-function registration))))
           registrations)))
    (if position
        (append (subseq registrations 0 position)
                (list replacement)
                (nthcdr (1+ position) registrations))
        (append registrations (list replacement)))))

(defun layered-registry-remove
    (registrations key &key key-function source-function key-test source)
  "Return REGISTRATIONS without the SOURCE layer identified by KEY."
  (remove-if
   (lambda (registration)
     (and (funcall key-test key (funcall key-function registration))
          (eq source (funcall source-function registration))))
   registrations))

(defun layered-registry-remove-source (registrations source source-function)
  "Return REGISTRATIONS without any layer attributed to SOURCE."
  (remove source registrations :key source-function :test #'eq))

(defun layered-registry-effective (registrations key-function source-rank-function)
  "Return effective layers, preserving first-key order and highest rank.
Later layers win when their source ranks tie."
  (let ((order   nil)
        (seen    (make-hash-table :test #'equal))
        (winners (make-hash-table :test #'equal)))
    (dolist (registration registrations)
      (let ((key (funcall key-function registration)))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push key order))
        (let ((winner (gethash key winners)))
          (when (or (null winner)
                    (>= (funcall source-rank-function registration)
                        (funcall source-rank-function winner)))
            (setf (gethash key winners) registration)))))
    (mapcar (lambda (key) (gethash key winners)) (nreverse order))))

(defun layered-registry-restore-position
    (registrations key snapshot &key key-function key-test copy-function)
  "Restore KEY from SNAPSHOT at its recorded position, or remove it.
COPY-FUNCTION detaches the restored registration as required by the caller."
  (let ((remaining
          (remove key registrations :key key-function :test key-test)))
    (if snapshot
        (let* ((position (min (getf snapshot :position) (length remaining)))
               (registration
                 (funcall copy-function (getf snapshot :registration))))
          (append (subseq remaining 0 position)
                  (list registration)
                  (nthcdr position remaining)))
        remaining)))


;;;; -- Registry Lifecycle --

(defclass registry-lifecycle ()
  ((snapshot-function
    :initarg       :snapshot-function
    :initform      nil
    :reader        registry-lifecycle--snapshot-function
    :documentation "Zero-argument snapshot callback.")
   (restore-function
    :initarg       :restore-function
    :initform      nil
    :reader        registry-lifecycle--restore-function
    :documentation "Callback accepting a snapshot, including NIL.")
   (remove-source-function
    :initarg       :remove-source-function
    :initform      nil
    :reader        registry-lifecycle--remove-source-function
    :documentation "Callback accepting the source to remove."))
  (:documentation "A registry lifecycle implemented by three function callbacks."))

(defgeneric registry-snapshot (registry)
  (:documentation "Return a snapshot of REGISTRY.
Snapshot representation, detachment and return values are the implementation's
responsibility. A NIL snapshot is valid."))

(defgeneric registry-restore (registry snapshot)
  (:documentation "Restore REGISTRY from SNAPSHOT, which may be NIL.
The implementation determines return values and any copying required."))

(defgeneric registry-remove-source (registry source)
  (:documentation "Remove SOURCE's registrations from REGISTRY.
The implementation determines return values."))

(defmethod initialize-instance :after ((registry registry-lifecycle) &key)
  "Require all lifecycle callbacks to be functions."
  (check-type (slot-value registry 'snapshot-function) function)
  (check-type (slot-value registry 'restore-function) function)
  (check-type (slot-value registry 'remove-source-function) function))

(defmethod registry-snapshot ((registry registry-lifecycle))
  "Return all values from REGISTRY's snapshot callback."
  (funcall (registry-lifecycle--snapshot-function registry)))

(defmethod registry-restore ((registry registry-lifecycle) snapshot)
  "Pass SNAPSHOT to REGISTRY's restore callback and return its values."
  (funcall (registry-lifecycle--restore-function registry) snapshot))

(defmethod registry-remove-source ((registry registry-lifecycle) source)
  "Pass SOURCE to REGISTRY's removal callback and return its values."
  (funcall (registry-lifecycle--remove-source-function registry) source))

(defun make-registry-lifecycle
    (&key snapshot-function restore-function remove-source-function)
  "Create a lifecycle adapter with three required function callbacks.
SNAPSHOT-FUNCTION takes no arguments; RESTORE-FUNCTION takes a snapshot;
REMOVE-SOURCE-FUNCTION takes a source. Callbacks own snapshot detachment and
return values. All values and signaled conditions pass through unchanged.
Callers own locking, validation, derived state and transaction policy."
  (make-instance 'registry-lifecycle
                 :snapshot-function      snapshot-function
                 :restore-function       restore-function
                 :remove-source-function remove-source-function))
