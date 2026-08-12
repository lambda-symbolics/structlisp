;;;; interval.lisp

(in-package #:structlisp)


;;;; -- Conditions and records --

(define-condition integer-interval-error (error)
  ((start
    :initarg :start
    :reader integer-interval-error-start
    :documentation "The invalid interval start.")
   (end
    :initarg :end
    :reader integer-interval-error-end
    :documentation "The invalid interval end."))
  (:report
   (lambda (condition stream)
     (format stream "Invalid half-open integer interval [~D, ~D); start exceeds end."
             (integer-interval-error-start condition)
             (integer-interval-error-end condition))))
  (:documentation "An integer interval had its start greater than its end."))

(defstruct (integer-interval
            (:constructor integer-interval--make (start end)))
  "A half-open integer interval [START, END)."
  (start 0 :type integer :read-only t)
  (end 0 :type integer :read-only t))

(defstruct (integer-interval-entry
            (:constructor integer-interval-entry--make (start end value)))
  "A half-open integer interval associated with a value."
  (start 0 :type integer :read-only t)
  (end 0 :type integer :read-only t)
  (value nil :read-only t))

(defun make-integer-interval (start end)
  "Create the half-open integer interval [START, END)."
  (integer-interval--validate start end)
  (integer-interval--make start end))


;;;; -- Interval set --

(defstruct (integer-interval-set
            (:constructor integer-interval-set--make))
  "A canonical sorted union of half-open integer intervals."
  (intervals #() :type simple-vector))

(defun make-integer-interval-set (&key intervals)
  "Create an interval set from a sequence of intervals or (START . END) conses."
  (let ((set (integer-interval-set--make)))
    (map nil
         (lambda (interval)
           (multiple-value-bind (start end)
               (integer-interval--bounds interval)
             (integer-interval-set-add set start end)))
         intervals)
    set))

(defun integer-interval-set-count (set)
  "Return the number of disjoint canonical intervals in SET."
  (length (integer-interval-set-intervals set)))

(defun integer-interval-set-empty-p (set)
  "Return true when SET contains no integers."
  (zerop (integer-interval-set-count set)))

(defun integer-interval-set-add (set start end)
  "Add [START, END) to SET, merging overlap and adjacency, and return SET."
  (integer-interval--validate start end)
  (when (= start end)
    (return-from integer-interval-set-add set))
  (let ((result nil)
        (new-start start)
        (new-end end)
        (inserted-p nil))
    (loop for interval across (integer-interval-set-intervals set)
          for interval-start = (integer-interval-start interval)
          for interval-end = (integer-interval-end interval)
          do (cond
               ((< interval-end new-start)
                (push interval result))
               ((< new-end interval-start)
                (unless inserted-p
                  (push (integer-interval--make new-start new-end) result)
                  (setf inserted-p t))
                (push interval result))
               (t
                (setf new-start (min new-start interval-start)
                      new-end (max new-end interval-end)))))
    (unless inserted-p
      (push (integer-interval--make new-start new-end) result))
    (setf (integer-interval-set-intervals set)
          (coerce (nreverse result) 'simple-vector))
    set))

(defun integer-interval-set-remove (set start end)
  "Remove [START, END) from SET, splitting intervals as needed, and return SET."
  (integer-interval--validate start end)
  (when (= start end)
    (return-from integer-interval-set-remove set))
  (let ((result nil))
    (loop for interval across (integer-interval-set-intervals set)
          for interval-start = (integer-interval-start interval)
          for interval-end = (integer-interval-end interval)
          do (cond
               ((or (<= interval-end start) (>= interval-start end))
                (push interval result))
               (t
                (when (< interval-start start)
                  (push (integer-interval--make interval-start start) result))
                (when (> interval-end end)
                  (push (integer-interval--make end interval-end) result)))))
    (setf (integer-interval-set-intervals set)
          (coerce (nreverse result) 'simple-vector))
    set))

(defun integer-interval-set-contains-p (set point)
  "Return true when integer POINT belongs to SET."
  (let ((position (integer-interval-set--candidate-position set point)))
    (and position
         (let ((interval (aref (integer-interval-set-intervals set) position)))
           (and (<= (integer-interval-start interval) point)
                (< point (integer-interval-end interval)))))))

(defun integer-interval-set-covers-p (set start end)
  "Return true when every integer in [START, END) belongs to SET."
  (integer-interval--validate start end)
  (when (= start end)
    (return-from integer-interval-set-covers-p t))
  (let ((position (integer-interval-set--candidate-position set start)))
    (and position
         (let ((interval (aref (integer-interval-set-intervals set) position)))
           (and (<= (integer-interval-start interval) start)
                (>= (integer-interval-end interval) end))))))

(defun integer-interval-set-intersects-p (set start end)
  "Return true when SET intersects non-empty [START, END)."
  (integer-interval--validate start end)
  (when (= start end)
    (return-from integer-interval-set-intersects-p nil))
  (let ((position (integer-interval-set--first-ending-after set start)))
    (and (< position (integer-interval-set-count set))
         (< (integer-interval-start
             (aref (integer-interval-set-intervals set) position))
            end))))

(defun integer-interval-set-union (left right)
  "Return a fresh interval set containing the union of LEFT and RIGHT."
  (let ((result (make-integer-interval-set
                 :intervals (integer-interval-set-intervals left))))
    (loop for interval across (integer-interval-set-intervals right)
          do (integer-interval-set-add
              result
              (integer-interval-start interval)
              (integer-interval-end interval)))
    result))

(defun integer-interval-set-intersection (left right)
  "Return a fresh interval set containing the intersection of LEFT and RIGHT."
  (let ((result (integer-interval-set--make))
        (intervals nil)
        (left-position 0)
        (right-position 0))
    (loop while (and (< left-position (integer-interval-set-count left))
                     (< right-position (integer-interval-set-count right)))
          for left-interval = (aref (integer-interval-set-intervals left)
                                    left-position)
          for right-interval = (aref (integer-interval-set-intervals right)
                                     right-position)
          for start = (max (integer-interval-start left-interval)
                           (integer-interval-start right-interval))
          for end = (min (integer-interval-end left-interval)
                         (integer-interval-end right-interval))
          do (when (< start end)
               (push (integer-interval--make start end) intervals))
             (if (< (integer-interval-end left-interval)
                    (integer-interval-end right-interval))
                 (incf left-position)
                 (incf right-position)))
    (setf (integer-interval-set-intervals result)
          (coerce (nreverse intervals) 'simple-vector))
    result))

(defun integer-interval-set-difference (left right)
  "Return a fresh interval set containing LEFT minus RIGHT."
  (let ((result (make-integer-interval-set
                 :intervals (integer-interval-set-intervals left))))
    (loop for interval across (integer-interval-set-intervals right)
          do (integer-interval-set-remove
              result
              (integer-interval-start interval)
              (integer-interval-end interval)))
    result))

(defun integer-interval-set->vector (set)
  "Return a fresh vector of SET's canonical intervals."
  (copy-seq (integer-interval-set-intervals set)))

(defun integer-interval-set-clear (set)
  "Remove every interval from SET and return SET."
  (setf (integer-interval-set-intervals set) #())
  set)


;;;; -- Interval map --

(defstruct (integer-interval-map
            (:constructor integer-interval-map--make))
  "A sorted map from disjoint half-open integer intervals to values."
  (entries #() :type simple-vector)
  (value-test #'eql :type function))

(defun make-integer-interval-map (&key entries (value-test #'eql))
  "Create an interval map from (START END VALUE) entries.

Later entries overwrite earlier overlapping entries. Adjacent entries with
values equal under VALUE-TEST are merged."
  (let ((map (integer-interval-map--make
              :value-test (coerce value-test 'function))))
    (map nil
         (lambda (entry)
           (integer-interval-map-set map
                                     (first entry)
                                     (second entry)
                                     (third entry)))
         entries)
    map))

(defun integer-interval-map-count (map)
  "Return the number of canonical mapped intervals in MAP."
  (length (integer-interval-map-entries map)))

(defun integer-interval-map-empty-p (map)
  "Return true when MAP has no mapped integers."
  (zerop (integer-interval-map-count map)))

(defun integer-interval-map-get (map point &optional default)
  "Return POINT's value, interval start, interval end, and true.

When POINT is unmapped, return DEFAULT, NIL, NIL, and NIL."
  (let ((position (integer-interval-map--candidate-position map point)))
    (if position
        (let ((entry (aref (integer-interval-map-entries map) position)))
          (if (and (<= (integer-interval-entry-start entry) point)
                   (< point (integer-interval-entry-end entry)))
              (values (integer-interval-entry-value entry)
                      (integer-interval-entry-start entry)
                      (integer-interval-entry-end entry)
                      t)
              (values default nil nil nil)))
        (values default nil nil nil))))

(defun integer-interval-map-set (map start end value)
  "Set every integer in [START, END) to VALUE and return MAP."
  (integer-interval--validate start end)
  (when (= start end)
    (return-from integer-interval-map-set map))
  (let ((result nil))
    (loop for entry across (integer-interval-map-entries map)
          for entry-start = (integer-interval-entry-start entry)
          for entry-end = (integer-interval-entry-end entry)
          do (cond
               ((or (<= entry-end start) (>= entry-start end))
                (push entry result))
               (t
                (when (< entry-start start)
                  (push (integer-interval-entry--make
                         entry-start start (integer-interval-entry-value entry))
                        result))
                (when (> entry-end end)
                  (push (integer-interval-entry--make
                         end entry-end (integer-interval-entry-value entry))
                        result)))))
    (push (integer-interval-entry--make start end value) result)
    (setf (integer-interval-map-entries map)
          (integer-interval-map--canonicalize map result))
    map))

(defun integer-interval-map-delete (map start end)
  "Remove mappings in [START, END), splitting entries as needed, and return MAP."
  (integer-interval--validate start end)
  (when (= start end)
    (return-from integer-interval-map-delete map))
  (let ((result nil))
    (loop for entry across (integer-interval-map-entries map)
          for entry-start = (integer-interval-entry-start entry)
          for entry-end = (integer-interval-entry-end entry)
          do (cond
               ((or (<= entry-end start) (>= entry-start end))
                (push entry result))
               (t
                (when (< entry-start start)
                  (push (integer-interval-entry--make
                         entry-start start (integer-interval-entry-value entry))
                        result))
                (when (> entry-end end)
                  (push (integer-interval-entry--make
                         end entry-end (integer-interval-entry-value entry))
                        result)))))
    (setf (integer-interval-map-entries map)
          (integer-interval-map--canonicalize map result))
    map))

(defun integer-interval-map-overlaps (map start end)
  "Return a fresh vector of MAP entries clipped to [START, END)."
  (integer-interval--validate start end)
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (unless (= start end)
      (loop for position from (integer-interval-map--first-ending-after map start)
            below (integer-interval-map-count map)
            for entry = (aref (integer-interval-map-entries map) position)
            while (< (integer-interval-entry-start entry) end)
            do (vector-push-extend
                (integer-interval-entry--make
                 (max start (integer-interval-entry-start entry))
                 (min end (integer-interval-entry-end entry))
                 (integer-interval-entry-value entry))
                result)))
    (coerce result 'simple-vector)))

(defun integer-interval-map-map (function map)
  "Call FUNCTION with start, end, and value for each entry, then return MAP."
  (loop for entry across (integer-interval-map-entries map)
        do (funcall function
                    (integer-interval-entry-start entry)
                    (integer-interval-entry-end entry)
                    (integer-interval-entry-value entry)))
  map)

(defun integer-interval-map->vector (map)
  "Return a fresh vector of MAP's canonical entries."
  (copy-seq (integer-interval-map-entries map)))

(defun integer-interval-map-clear (map)
  "Remove every mapping from MAP and return MAP."
  (setf (integer-interval-map-entries map) #())
  map)


;;;; -- Internal mechanics --

(defun integer-interval--validate (start end)
  (check-type start integer)
  (check-type end integer)
  (when (> start end)
    (error 'integer-interval-error :start start :end end)))

(defun integer-interval--bounds (interval)
  (etypecase interval
    (integer-interval
     (values (integer-interval-start interval)
             (integer-interval-end interval)))
    (cons
     (values (first interval) (rest interval)))))

(defun integer-interval-set--candidate-position (set point)
  (let ((low 0)
        (high (integer-interval-set-count set)))
    (loop while (< low high)
          for middle = (floor (+ low high) 2)
          if (<= (integer-interval-start
                  (aref (integer-interval-set-intervals set) middle))
                 point)
            do (setf low (1+ middle))
          else
            do (setf high middle))
    (when (> low 0)
      (1- low))))

(defun integer-interval-set--first-ending-after (set point)
  (let ((low 0)
        (high (integer-interval-set-count set)))
    (loop while (< low high)
          for middle = (floor (+ low high) 2)
          if (<= (integer-interval-end
                  (aref (integer-interval-set-intervals set) middle))
                 point)
            do (setf low (1+ middle))
          else
            do (setf high middle))
    low))

(defun integer-interval-map--candidate-position (map point)
  (let ((low 0)
        (high (integer-interval-map-count map)))
    (loop while (< low high)
          for middle = (floor (+ low high) 2)
          if (<= (integer-interval-entry-start
                  (aref (integer-interval-map-entries map) middle))
                 point)
            do (setf low (1+ middle))
          else
            do (setf high middle))
    (when (> low 0)
      (1- low))))

(defun integer-interval-map--first-ending-after (map point)
  (let ((low 0)
        (high (integer-interval-map-count map)))
    (loop while (< low high)
          for middle = (floor (+ low high) 2)
          if (<= (integer-interval-entry-end
                  (aref (integer-interval-map-entries map) middle))
                 point)
            do (setf low (1+ middle))
          else
            do (setf high middle))
    low))

(defun integer-interval-map--canonicalize (map entries)
  (let ((sorted (stable-sort entries #'< :key #'integer-interval-entry-start))
        (result nil))
    (dolist (entry sorted)
      (let ((previous (first result)))
        (if (and previous
                 (= (integer-interval-entry-end previous)
                    (integer-interval-entry-start entry))
                 (funcall (integer-interval-map-value-test map)
                          (integer-interval-entry-value previous)
                          (integer-interval-entry-value entry)))
            (setf (first result)
                  (integer-interval-entry--make
                   (integer-interval-entry-start previous)
                   (integer-interval-entry-end entry)
                   (integer-interval-entry-value previous)))
            (push entry result))))
    (coerce (nreverse result) 'simple-vector)))
