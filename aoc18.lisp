(defpackage :aoc18
  (:use :common-lisp :alexandria))

(in-package :aoc18)

(defparameter +input-file+ #P"/home/tjs/git/aoc2018/18.input")
(defparameter +input-file-test+ #P"/home/tjs/git/aoc2018/18-test.input")

(defun scan-by-line-from-stream (file)
  (let* ((lines (ioutil:snarf-file file))
         (width (loop :for line :in lines
                      :maximizing (length line))))
    (loop
      :with r = (make-array `(,(length lines) ,width)
                            :element-type 'character
                            :initial-element #\Space)
      :for y :from 0
      :for line :in lines :do
        (loop :for x :from 0 :below (length line) :do
          (setf (aref r y x) (aref line x)))
      :finally (return r))))

(defvar *grid*)
(defvar *minutes*)
(defvar *states*)

(defun reset ()
  (setf *minutes* 0)
  (setf *states*  (make-hash-table
                   :hash-function #'resource-value
                   :test 'equalp)))

(defun init-grid ()
  (reset)
  (with-open-file (s +input-file+)
    (setf *grid* (scan-by-line-from-stream s))))

(defun init-grid-test ()
  (reset)
  (with-open-file (s +input-file-test+)
    (setf *grid* (scan-by-line-from-stream s))))

(defun print-grid ()
  (let ((grid *grid*))
    (loop :for y :from 0 :below (array-dimension grid 0) :do
      (loop :for x :from 0 :below (array-dimension grid 1)
            :do (princ (aref grid y x))
            :finally (terpri)))))

(defun maybe-square (y x)
  (if (not (and (<= 0 y (1- (array-dimension *grid* 0)))
                (<= 0 x (1- (array-dimension *grid* 1)))))
      nil
      (aref *grid* y x)))

(defun adjacent-squares (y_0 x_0)
  (let ((y+1 (+ y_0 1))
        (y-1 (- y_0 1))
        (x+1 (+ x_0 1))
        (x-1 (- x_0 1)))
    (delete-if #'null (list (maybe-square y+1 x+1)
                            (maybe-square y+1 x_0)
                            (maybe-square y+1 x-1)
                            (maybe-square y_0 x+1)
                            (maybe-square y_0 x-1)
                            (maybe-square y-1 x+1)
                            (maybe-square y-1 x_0)
                            (maybe-square y-1 x-1)))))

(defstruct counts
  trees
  lumberyards)

(defconstant +open+ #\.)
(defconstant +tree+ #\|)
(defconstant +lumberyard+ #\#)

(defun count-square (y x)
  (let ((adj (adjacent-squares y x)))
    (make-counts :lumberyards (count +lumberyard+ adj)
                 :trees (count +tree+ adj))))

(defun new-grid-value (sq c)
  (check-type sq character)
  (check-type c counts)
  (switch (sq :test 'eql)
    (+open+ (if (>= (counts-trees c) 3) +tree+ +open+))
    (+tree+ (if (>= (counts-lumberyards c) 3) +lumberyard+ +tree+))
    (+lumberyard+ (if (and (>= (counts-trees c) 1)
                           (>= (counts-lumberyards c) 1))
                      +lumberyard+
                      +open+))))

(defun one-minute ()
  (let ((new (make-array (array-dimensions *grid*))))
    (loop :for y :from 0 :below (array-dimension *grid* 0) :do
      (loop :for x :from 0 :below (array-dimension *grid* 1) :do
        (setf (aref new y x)
              (new-grid-value (aref *grid* y x)
                              (count-square y x)))))
    (incf *minutes*)
    (setf *grid* new)
    (setf (gethash *grid* *states*)
          (push *minutes* (gethash *grid* *states*)))))
                                              
(defun resource-value ()
  (grid-resource-value *grid*))

(defun grid-resource-value (grid)
  (let ((trees 0)
        (lumberyards 0))
    (loop :for y :from 0 :below (array-dimension *grid* 0) :do
      (loop :for x :from 0 :below (array-dimension *grid* 1) :do
        (switch ((aref *grid* y x))
          (#\# (incf lumberyards))
          (#\| (incf trees)))))
    (* trees lumberyards)))

;; this helps find a cycle:
;; (loop :until (> (length (gethash *grid* *states*)) 1) :do (one-minute))
;; I learned my cycle was 28.

;; this is dumb (we could use math), but gets us to just under the limit:
;; 
;; AOC18> (loop :while (< (+ *minutes* 28) 1000000000) :do (incf *minutes* 28))
;; NIL

;; now we can step forward in time slightly

;; AOC18> *minutes*
;; 999999997 (30 bits, #x3B9AC9FD)
;; AOC18> (one-minute)
;; (999999998 466)
;; AOC18> (one-minute)
;; (999999999 467)
;; AOC18> (one-minute)
;; (1000000000 468)

;; AOC18> (resource-value)
;; 200364 (18 bits, #x30EAC)
