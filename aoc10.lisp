(defpackage :aoc10
  (:use :common-lisp))

(in-package :aoc10)

(defvar +input-file+ #P"/home/tjs/git/aoc2018/10.input")

(defvar *points*)

(defun read-input () (read-input-from +input-file+))

(defstruct point
  (x nil :type integer :read-only t)
  (y nil :type integer :read-only t)
  (dx nil :type integer :read-only t)
  (dy nil :type integer :read-only t))

(defun read-input-from (file)
  ;; position=<-32391,  32789> velocity=< 3, -3>
  (let* ((pattern "position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>")
         (scanner (cl-ppcre:create-scanner pattern)))
    (loop
      :for line in (ioutil:snarf-file file)
      :collect (ppcre:register-groups-bind ((#'parse-integer x y xd yd))
                   (scanner line)
                 (make-point :x x :y y :dx xd :dy yd)))))

(defun move-point-once (p)
  (check-type p point)
  (make-point :x (+ (point-x p) (point-dx p))
              :y (+ (point-y p) (point-dy p))
              :dx (point-dx p)
              :dy (point-dy p)))

(defun iterate-once (points)
  (mapcar #'move-point-once points))

(defun iterate-n (n points)
  (loop :while (> n 0)
        :do (decf n)
        :do (setf points (iterate-once points)))
  points)

(defun min-x (points)
  (apply #'min (mapcar #'point-x points)))

(defun min-y (points)
  (apply #'min (mapcar #'point-y points)))

(defun max-x (points)
  (apply #'max (mapcar #'point-x points)))

(defun max-y (points)
  (apply #'max (mapcar #'point-y points)))

(defun iterate-until-positive (points)
  (let ((n 0))
    (loop :while (or (< (min-x points) 0)
                     (< (min-y points) 0))
          :do (incf n)
          :do (setf points (iterate-once points)))
    (values points n)))

(defun to-array (points)
  (let ((a (make-array `(,(+ 1 (max-x points)) , (+ 1 (max-y points))) :element-type 'integer)))
    (loop :for p :in points
          :do (setf (aref a (point-x p) (point-y p)) 1))
    a))

(defun render-array (a)
  (loop :for y from 0 below (array-dimension a 1) :do
    (loop :for x from 0 below (array-dimension a 0) :do
      (princ (cond
               ((= (aref a x y) 0) ".")
               ((= (aref a x y) 1) "#")
               ((= (aref a x y) 2) "2") ;sentry
               ((= (aref a x y) 3) "3") ;sentry
               ((= (aref a x y) 4) "4") ;sentry
               (t "_"))))
    (princ #\Newline)))

;; (render-array (to-array (iterate-n 10867 *points*)))
