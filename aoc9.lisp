(defpackage :aoc9
  (:use :common-lisp))

(in-package :aoc9)

;; AOC9> (time (game 473 7090400))
;; Evaluation took:
;;   0.853 seconds of real time
;;   0.852974 seconds of total run time (0.732911 user, 0.120063 system)
;;   [ Run times consist of 0.554 seconds GC time, and 0.299 seconds non-GC time. ]
;;   100.00% CPU
;;   2,813,919,165 processor cycles
;;   217,128,224 bytes consed
  
;; 3038972494 (32 bits, #xB5230A4E)

;; The big problem I had with this was printing a circular list.  The classic
;; Lisp thing to do is cdar/cddr to store the previous/next pointers.  I tried
;; this, but getting Lisp not to print cons cells in the repl is a challenge.
;; Setting *print-circle* did not help.  I gave up and made a more readable
;; object that I could avoid printing.

;; "next" is clockwise;
;; "prev" is counterclockwise.

(defstruct (pos
            (:print-function
             (lambda (pos s _)
               (declare (ignore _))
               (princ "#<POS" s)
               (loop :for i :from -8 :to 8
                     :do (format s " ~[(~:;~]~a~0@*~[)~:;~]" i
                                 (pos-points (pos-step pos i))))
               (princ ">" s))))
  (points 0 :read-only t :type integer)
  (next nil :type (or null pos))
  (prev nil :type (or null pos)))

(defun pos-step (pos n)
  (check-type n integer)
  (check-type pos pos)
  (loop :while (< n 0)
        :do (incf n)
        :do (setf pos (pos-prev pos)))
  (loop :while (> n 0)
        :do (decf n)
        :do (setf pos (pos-next pos)))
  pos)

(defun remove-marble (c)
  (let ((pv (pos-prev c))
        (nx (pos-next c)))
    (setf (pos-next pv) nx)
    (setf (pos-prev nx) pv)
    nx))

(defun insert-marble (old-c p)
  (let* ((one-before-new-node (pos-next old-c))
         (one-after-new-node  (pos-next one-before-new-node))
         (new-node (make-pos :points p
                             :prev one-before-new-node
                             :next one-after-new-node)))
    (setf (pos-next one-before-new-node) new-node)
    (setf (pos-prev one-after-new-node)  new-node)
    new-node))
    
(defun make-circle ()
  (let ((n (make-pos :points 0)))
    (setf (pos-next n) n)
    (setf (pos-prev n) n)))

(defun game (nplayers last-marble)
  (let ((scores (make-array nplayers :element-type 'integer)))
    (loop :with circle = (make-circle)
          :for current-marble :from 1 :to last-marble :do
            ;; (format t "cm=~a ~a~%" current-marble circle)
            (progn
              (if (= 0 (mod current-marble 23))
                  ;; 23
                  (let ((back7 (pos-n-prev circle 7)))
                    (incf (aref scores (mod current-marble nplayers))
                          (+ current-marble
                             (pos-points back7)))
                    (setf circle (remove-marble back7)))
                  ;; not 23
                  (setf circle (insert-marble circle current-marble)))))
    ;; (format t "scores=~a" (concatenate 'list scores))
    (apply #'max (concatenate 'list scores))))
    
