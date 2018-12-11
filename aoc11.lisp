(defpackage :aoc11
  (:use :common-lisp))

(in-package :aoc11)

(defvar *input* 9005)

(defun power-level-of-cell (x y)
  (let* ((rack-id (+ x 10))
         (power-level-starts-at (* rack-id y))
         (add-sn (+ power-level-starts-at *input*))
         (times-rack-id (* add-sn rack-id)))
    ;; (format t "~a~a~%~a~%~a~%~a~%" *input* rack-id power-level-starts-at add-sn times-rack-id)
    (multiple-value-bind (_ rem) (truncate times-rack-id 1000)
      (declare (ignore _))
      (let ((middle-digit (truncate rem 100)))
        ;; (format t "truncated remainder ~a; truncating again ~a~%" rem middle-digit)
        (- middle-digit 5)))))

(defun all-3x3 ()
  (let ((a (make-array '(301 301)))    ; we will not use left col or top row
        (3x3 (make-array '(299 299)))) ; we will not use left col or top row
    (loop :for x :from 1 :to 300 :do
      (loop :for y :from 1 :to 300 :do
        (setf (aref a x y) (power-level-of-cell x y))))

    (loop :for x :from 1 :to 298 :do
      (loop :for y :from 1 :to 298 :do
        (loop :for i :from 0 below 3 :do
          (loop :for j :from 0 below 3 :do
            (incf (aref 3x3 x y)
                  (aref a (+ x i) (+ y j)))))))

    (let ((max (aref 3x3 1 1))
          (at (cons 1 1)))
      (loop :for x :from 1 :to 298 :do
        (loop :for y :from 1 :to 298 :do
          (let ((looking-at (aref 3x3 x y)))
            (when (> looking-at max)
              (format t "~a better than ~a at ~a,~a~%" looking-at max x y)
              (setf max looking-at)
              (setf at (cons x y))))))

      at)))


;; I forgot to commit the working code I had that was O(N**3), and that's what
;; I actually used to submit my answer.

;; Memoizing was obviously required, and when someone hinted the algorithm, I
;; implemented it.  This took a lot longer than I want to admit.  One sneaky bug
;; is that if you triple-add the corner (I had an incf where I needed a decf)
;; the example inputs pass, but mine fails; it is off by *one column*.
;;
;; This code combines 0-based and 1-based arrays so I find it extra difficult
;; to reason about.  As above, we waste a row and column to try and simplify
;; the references, but we have to treat the offsets carefully.
;;
;; Takes around 5 seconds of runtime.  (The O(N**3) version took well over 5
;; minutes, although I only had the patience to run it once.)

(defun all-n-by-n ()
  (let* ((gmax 300)                     ; documented grid size
         (agmax (+ 1 gmax))             ; array grid max, fudged for 1-base
         (max -999999999)               ; impossibly low score
         at                             ; answer
         (local-scores (make-array `(,agmax agmax))))    ; we will not use left col or top row

    (loop :for x :from 1 :to gmax :do
      (loop :for y :from 1 :to gmax :do
        (setf (aref local-scores x y) (power-level-of-cell x y))))

    (loop :for grid-offset :from 0 to (- gmax 1)
          :for far-edge := (- gmax grid-offset)
          :with scores := (make-array `(,agmax ,agmax)) :do
            (progn
              (loop :for x :from 1 :to far-edge :do
                (loop :for y :from 1 :to far-edge :do
                  (loop :for i :from 0 :to grid-offset :do
                    (incf (aref scores x y)
                          (aref local-scores (+ x grid-offset) (+ y i))))
                  (loop :for i :from 0 :to (- grid-offset 1) :do
                    (incf (aref scores x y)
                          (aref local-scores (+ x i) (+ y grid-offset))))))

              (loop :for x :from 1 :to far-edge :do
                (loop :for y :from 1 :to far-edge :do
                  (let ((looking-at (aref scores x y)))
                    (when (> looking-at max)
                      ;; (format t "~a better than ~a at ~a,~a,~a~%" looking-at max x y (+ 1 grid-offset))
                      (setf max looking-at)
                      (setf at (list x y (+ 1 grid-offset)))))))))

    (values at max)))
