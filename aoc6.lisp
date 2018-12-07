(defpackage :aoc6
  (:use :common-lisp :cl-ppcre :ioutil))

(in-package :aoc6)

(defvar *points* nil)

(defvar +input-file+ "/home/tjs/git/aoc2018/6.input")

(defstruct point
  (id nil :type (or null integer))
  (x nil :type integer)
  (y nil :type integer))

(defun max-x (points)
  (+ 1 (apply #'max (mapcar #'point-x points))))

(defun max-y (points)
  (+ 1 (apply #'max (mapcar #'point-y points))))

(defun read-points (file)
  (let* ((pattern "(\\d+), (\\d+)")
         (scanner (cl-ppcre:create-scanner pattern)))
    (loop
      :for id :from 100
      :for line in (ioutil:snarf-file file)
      :collect (ppcre:register-groups-bind
                   ((#'parse-integer x y))
                   (scanner line)
                 (make-point :id id :x x :y y)))))

(defvar *owners* nil)
(defvar *distances* nil)

(defun init ()
  (init-from +input-file+))

(defun init-from (file)
  (multiple-value-bind (p o d)
      (just-init file)
    (setf *points* p)
    (setf *owners* o)
    (setf *distances* d)
    (values o p)))

(defun just-init (file)
  (let* ((points (read-points file))
         (xx (max-x points))
         (yy (max-y points))
         (inf 999)               ; silly effective infinity
         (owners (make-array `(,xx ,yy)
                             :element-type '(integer)))
         (distances (make-array `(,xx ,yy)
                                :initial-element inf
                                :element-type '(integer))))
    (set-distances points owners distances)
    (values points owners distances)))

(defun taxi-dist (a b)
  (check-type a point)
  (check-type b point)
  (+ (abs (- (point-x a) (point-x b)))
     (abs (- (point-y a) (point-y b)))))
     

(defun set-distances (points owners distances)
  (check-type points list)
  (check-type owners (array * *))
  (check-type distances (array * *))
  (let ((xx (array-dimension owners 0))
        (yy (array-dimension owners 1)))

    ;; not very efficient.  We could probably do better
    ;; by spreading out in a box around the point, and checking
    ;; for better neighbors as we go; if we encounter one, we can
    ;; simply ignore it.
    (loop :for point in points :do
      (loop :for x :below xx :do
        (loop :for y :below yy :do
          (let ((new-dist (taxi-dist point
                                     (make-point :x x :y y)))
                (cur-dist (aref distances x y)))
            (when (= new-dist cur-dist)
              ;; tie!
              (setf (aref owners x y) nil)
              (setf (aref distances x y) new-dist))
            (when (< new-dist cur-dist)
              (setf (aref owners x y) (point-id point))
              (setf (aref distances x y) new-dist))))))))

(defun compute-areas (owners)
  (check-type owners (array * *))

  (let ((xx (array-dimension owners 0))
        (yy (array-dimension owners 1))
        (areas (make-hash-table))
        (infinite-areas (make-hash-table)))

    (loop :for x :below xx :do
      (loop :for y :below yy :do
        (cond ((or (eq x 0)
                   (eq y 0)
                   (eq x (- xx 1))
                   (eq y (- yy 1)))
               (setf (gethash (aref owners x y) infinite-areas) t))
              (t (incf (gethash (aref owners x y) areas 0))))))

    (maphash (lambda (k v)
               (declare (ignore v))
               (remhash k areas))
             infinite-areas)

    (loop :for v :being :the :hash-values :in areas
          :collect v)))

(defun answer-one ()
  (let ((owners (init)))
    (apply #'max (compute-areas owners))))

(defun sum-distance-to-all-points (points)
  (let* ((xx (max-x points))
         (yy (max-y points))
         (r (make-array `(,(max-x points) ,(max-y points))
                        :initial-element 0
                        :element-type '(integer))))

    (loop :for point :in points :do
      (loop :for x :below xx :do
        (loop :for y :below yy :do
          (incf (aref r x y)
                (taxi-dist point (make-point :x x :y y))))))
    r))

(defun region-size (dists ok-dist)
  (let ((xx (array-dimension dists 0))
        (yy (array-dimension dists 1))
        (r 0))
    (loop :for x :below xx :do
      (loop :for y :below yy :do
        (when (< (aref dists x y) ok-dist)
          (incf r))))
    r))

(defun answer-two ()
  (multiple-value-bind (owners points)
      (init)
    (declare (ignore owners))
    (let ((dists (sum-distance-to-all-points points)))
      (region-size dists 10000))))
