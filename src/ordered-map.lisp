;;;; ordered-map.lisp

(in-package #:structlisp)


;;;; -- Ordered map --

(defstruct (ordered-map-node
            (:constructor ordered-map--make-node))
  key
  value
  previous
  next)

(defstruct (ordered-map
            (:constructor ordered-map--make))
  "A hash table whose entries retain insertion order."
  (table (make-hash-table) :type hash-table)
  first-node
  last-node)

(defun make-ordered-map (&key (test 'eql) initial-contents)
  "Create an insertion-ordered map using hash-table TEST.

INITIAL-CONTENTS is a sequence of conses. Updating an existing key preserves
its position."
  (let ((map (ordered-map--make :table (make-hash-table :test test))))
    (map nil
         (lambda (entry)
           (ordered-map-set map (first entry) (rest entry)))
         initial-contents)
    map))

(defun ordered-map-count (map)
  "Return the number of entries in MAP."
  (hash-table-count (ordered-map-table map)))

(defun ordered-map-empty-p (map)
  "Return true when MAP contains no entries."
  (zerop (ordered-map-count map)))

(defun ordered-map-get (map key &optional default)
  "Return KEY's value and true, or DEFAULT and NIL when absent."
  (multiple-value-bind (node present-p)
      (gethash key (ordered-map-table map))
    (if present-p
        (values (ordered-map-node-value node) t)
        (values default nil))))

(defun (setf ordered-map-get) (value map key &optional default)
  "Set KEY to VALUE in MAP and return VALUE.

DEFAULT is accepted for SETF protocol compatibility and ignored."
  (declare (ignore default))
  (ordered-map-set map key value))

(defun ordered-map-set (map key value)
  "Associate KEY with VALUE in MAP and return VALUE.

A new key is appended at the back. Updating an existing key does not change
its insertion position."
  (multiple-value-bind (node present-p)
      (gethash key (ordered-map-table map))
    (if present-p
        (setf (ordered-map-node-value node) value)
        (let ((new-node (ordered-map--make-node
                         :key      key
                         :value    value
                         :previous (ordered-map-last-node map))))
          (if (ordered-map-last-node map)
              (setf (ordered-map-node-next (ordered-map-last-node map))
                    new-node)
              (setf (ordered-map-first-node map) new-node))
          (setf (ordered-map-last-node map) new-node
                (gethash key (ordered-map-table map)) new-node)))
    value))

(defun ordered-map-delete (map key &optional default)
  "Remove KEY and return its value and true, or DEFAULT and NIL when absent."
  (multiple-value-bind (node present-p)
      (gethash key (ordered-map-table map))
    (if (not present-p)
        (values default nil)
        (progn
          (ordered-map--unlink-node map node)
          (remhash key (ordered-map-table map))
          (values (ordered-map-node-value node) t)))))

(defun ordered-map-delete-if (predicate map)
  "Delete entries satisfying PREDICATE and return MAP.

PREDICATE receives each key and value from a snapshot in insertion order."
  (ordered-map-map
   (lambda (key value)
     (when (funcall predicate key value)
       (ordered-map-delete map key)))
   map)
  map)

(defun ordered-map-first (map &optional default)
  "Return the first key, value, and true, or DEFAULT, DEFAULT, and NIL."
  (let ((node (ordered-map-first-node map)))
    (if node
        (values (ordered-map-node-key node)
                (ordered-map-node-value node)
                t)
        (values default default nil))))

(defun ordered-map-last (map &optional default)
  "Return the last key, value, and true, or DEFAULT, DEFAULT, and NIL."
  (let ((node (ordered-map-last-node map)))
    (if node
        (values (ordered-map-node-key node)
                (ordered-map-node-value node)
                t)
        (values default default nil))))

(defun ordered-map-pop-first (map &optional default)
  "Remove and return the first key, value, and true.

When MAP is empty, return DEFAULT, DEFAULT, and NIL."
  (let ((node (ordered-map-first-node map)))
    (if node
        (progn
          (ordered-map--unlink-node map node)
          (remhash (ordered-map-node-key node) (ordered-map-table map))
          (values (ordered-map-node-key node)
                  (ordered-map-node-value node)
                  t))
        (values default default nil))))

(defun ordered-map-pop-last (map &optional default)
  "Remove and return the last key, value, and true.

When MAP is empty, return DEFAULT, DEFAULT, and NIL."
  (let ((node (ordered-map-last-node map)))
    (if node
        (progn
          (ordered-map--unlink-node map node)
          (remhash (ordered-map-node-key node) (ordered-map-table map))
          (values (ordered-map-node-key node)
                  (ordered-map-node-value node)
                  t))
        (values default default nil))))

(defun ordered-map-move-to-front (map key)
  "Move KEY to the front and return its value and true.

Return NIL and NIL when KEY is absent."
  (multiple-value-bind (node present-p)
      (gethash key (ordered-map-table map))
    (if (not present-p)
        (values nil nil)
        (progn
          (unless (eq node (ordered-map-first-node map))
            (ordered-map--unlink-node map node)
            (setf (ordered-map-node-next node) (ordered-map-first-node map)
                  (ordered-map-node-previous node) nil
                  (ordered-map-node-previous (ordered-map-first-node map)) node
                  (ordered-map-first-node map) node))
          (values (ordered-map-node-value node) t)))))

(defun ordered-map-move-to-back (map key)
  "Move KEY to the back and return its value and true.

Return NIL and NIL when KEY is absent."
  (multiple-value-bind (node present-p)
      (gethash key (ordered-map-table map))
    (if (not present-p)
        (values nil nil)
        (progn
          (unless (eq node (ordered-map-last-node map))
            (ordered-map--unlink-node map node)
            (setf (ordered-map-node-previous node) (ordered-map-last-node map)
                  (ordered-map-node-next node) nil
                  (ordered-map-node-next (ordered-map-last-node map)) node
                  (ordered-map-last-node map) node))
          (values (ordered-map-node-value node) t)))))

(defun ordered-map-map (function map)
  "Call FUNCTION with each key and value in MAP's order, then return MAP.

Traversal uses a key/value snapshot taken before the first call. Mutating MAP
from FUNCTION does not alter which entries are visited or their values."
  (let ((entries (make-array (ordered-map-count map)))
        (position 0))
    (loop for node = (ordered-map-first-node map)
                    then (ordered-map-node-next node)
          while node
          do (setf (aref entries position)
                   (cons (ordered-map-node-key node)
                         (ordered-map-node-value node)))
             (incf position))
    (loop for entry across entries
          do (funcall function (first entry) (rest entry))))
  map)

(defun ordered-map-keys (map)
  "Return a fresh vector of MAP's keys in order."
  (let ((keys (make-array (ordered-map-count map)))
        (position 0))
    (ordered-map-map
     (lambda (key value)
       (declare (ignore value))
       (setf (aref keys position) key)
       (incf position))
     map)
    keys))

(defun ordered-map-values (map)
  "Return a fresh vector of MAP's values in order."
  (let ((values (make-array (ordered-map-count map)))
        (position 0))
    (ordered-map-map
     (lambda (key value)
       (declare (ignore key))
       (setf (aref values position) value)
       (incf position))
     map)
    values))

(defun ordered-map->alist (map)
  "Return a fresh association list of MAP's entries in order."
  (let ((entries nil))
    (ordered-map-map
     (lambda (key value)
       (push (cons key value) entries))
     map)
    (nreverse entries)))

(defun ordered-map-clear (map)
  "Remove all entries from MAP and return MAP."
  (clrhash (ordered-map-table map))
  (setf (ordered-map-first-node map) nil
        (ordered-map-last-node map) nil)
  map)


;;;; -- Internal mechanics --

(defun ordered-map--unlink-node (map node)
  (let ((previous (ordered-map-node-previous node))
        (next (ordered-map-node-next node)))
    (if previous
        (setf (ordered-map-node-next previous) next)
        (setf (ordered-map-first-node map) next))
    (if next
        (setf (ordered-map-node-previous next) previous)
        (setf (ordered-map-last-node map) previous))
    (setf (ordered-map-node-previous node) nil
          (ordered-map-node-next node) nil)
    node))
