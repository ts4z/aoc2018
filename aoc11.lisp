(defpackage :aoc11
  (:use :common-lisp))

(in-package :aoc11)

(defvar *input* 9005)

(defun power-level-of-cell (x y)
  (declare (type fixnum x y))
  (let* ((rack-id (+ x 10))
         (power-level-starts-at (* rack-id y))
         (add-sn (+ power-level-starts-at *input*))
         (times-rack-id (* add-sn rack-id))
         (rem (mod times-rack-id 1000))
         (middle-digit (truncate rem 100)))
    ;; this declaration shrinks the assembled code from 295 to 188
    (declare (type fixnum rack-id power-level-starts-at add-sn
                   times-rack-id rem middle-digit))
    (- middle-digit 5)))

(defun power-level-of-cell-w-input (input x y)
  (declare (type fixnum x y))
  (let* ((rack-id (+ x 10))
         (power-level-starts-at (* rack-id y))
         (add-sn (+ power-level-starts-at input))
         (times-rack-id (* add-sn rack-id))
         (rem (mod times-rack-id 1000))
         (middle-digit (truncate rem 100)))
    ;; this declaration shrinks the assembled code from 295 to 188
    (declare (type fixnum rack-id power-level-starts-at add-sn
                   times-rack-id rem middle-digit))
    (- middle-digit 5)))

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


;; I forgot to commit the working code I had that was O(N^5)?, and that's what
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
;; Takes around 5 seconds of runtime.  (The O(N^5) version took well over 5
;; minutes, although I only had the patience to run it once.)  I think this is
;; O(N^4)

(defun all-n-by-n ()
  (let* ((gmax 300)                     ; documented grid size
         (agmax (+ 1 gmax))             ; array grid max, fudged for 1-base
         (max -999999999)               ; impossibly low score
         at                             ; answer
         (local-scores (make-array `(,agmax agmax) :element-type 'fixnum)))    ; we will not use left col or top row
    (macrolet
        ((local-score (x y) `(aref local-scores ,x ,y)))
      
      (loop :for x :from 1 :to gmax :do
        (loop :for y :from 1 :to gmax :do
          (setf (local-score x y) (power-level-of-cell x y))))
      
      (loop :for grid-offset :from 0 :below gmax
            :for far-edge := (- gmax grid-offset)
            :with scores := (make-array `(,agmax ,agmax) :element-type 'fixnum) :do
              (progn
                (loop :for x :from 1 :to far-edge :do
                  (loop :for y :from 1 :to far-edge :do
                    (incf (aref scores x y)
                          (+ (local-score (+ x grid-offset) (+ y grid-offset))
                             (loop :for i :from 0 :below grid-offset
                                   :sum (local-score (+ x i) (+ y grid-offset))
                                   :sum (local-score (+ x grid-offset) (+ y i)))))))

              (loop :for x :from 1 :to far-edge :do
                (loop :for y :from 1 :to far-edge :do
                  (let ((looking-at (aref scores x y)))
                    (when (> looking-at max)
                      ;; (format t "~a better than ~a at ~a,~a,~a~%" looking-at max x y (+ 1 grid-offset))
                      (setf max looking-at)
                      (setf at (list x y (+ 1 grid-offset)))))))))

      (values at max))))

;; After someone clued me in to memoizing properly, I ran into this thread:
;; https://en.wikipedia.org/wiki/Summed-area_table and for my own edification I
;; wanted to do this implementation.  It took me a couple tries to get it right
;; because I did not read that web page very carefully.
;;
;; This version takes about 0.1s in CPU time.
(defun better (input)
  (declare (optimize speed))
  (let* ((gmax 300)                     ; documented grid size
         (agmax (+ 1 gmax))             ; array grid max, fudged for 1-base
         (max -999999999)               ; impossibly low score
         at                             ; answer
         ;; fudge the partials array so that we have an extra 0 col and row.
         ;; we do peek at that data in the code below and it is convenient that
         ;; it is zero; that way we don't have to bounds check further.
         (partials (make-array `(,agmax agmax) :element-type 'fixnum)))

    (macrolet
        ((partial (x y)
           `(aref partials ,x ,y)))
      ;; build grid of partial sums
      (loop :for x :from 1 :to gmax :do
        (loop :for y :from 1 :to gmax :do
          (setf (partial x y)
                (+ (partial x (1- y))
                   (partial (1- x) y)
                   (- (partial (1- x) (1- y)))
                   (power-level-of-cell-w-input input x y)))))

      ;; iterate partial sums, looking for max
      ;; we can take the sum of the whole grid anchored top left,
      ;; then remove the parts left and above what we are interested in;
      ;; we then re-add the part that is both left AND above as we ahve
      ;; removed it twice.
      ;;
      ;; This is O(N^3), but like the above solutions, as N becomes large,
      ;; one of the factors becomes small.
      (loop :for grid-offset :from 0 :to gmax :do
        (loop :for x :from 1 :to (- gmax grid-offset)  :do
          (loop :for y :from 1 :to (- gmax grid-offset)
                :for x-1 = (- x 1)
                :for y-1 = (- y 1)
                :for x+g = (+ x grid-offset)
                :for y+g = (+ y grid-offset)
                :for p = (+ (partial x+g y+g)
                            (partial x-1 y-1)
                            (- (+ (partial x+g y-1)
                                  (partial x-1 y+g))))
                :do (when (> p max)
                      (setf max p)
                      (setf at (list x y (1+ grid-offset))))
                ))))
      
      (values at max)))
