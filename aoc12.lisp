
;;; I'm keeping this because I want the full set, but this code is garbage.

;; Starting at iteration 119, we reach stability with a sequence of gliders:

;; 117 5726 190 ......#.##.#.##.#.##.#.#....#.#......#.#....#.#....#.#....#.#....#.#....#.#........#.#....#.#......#.#....#.#.......#.#...........#.#......#.#....#.#.....#.#.....#.#......#.#.......#.#....
;; 118 6066 340 ......################.#....#.#......#.#....#.#....#.#....#.#....#.#....#.#........#.#....#.#......#.#....#.#.......#.#...........#.#......#.#....#.#.....#.#.....#.#......#.#.......#.#........
;; 119 5468 -598 ....#.#..............#.#....#.#......#.#....#.#....#.#....#.#....#.#....#.#........#.#....#.#......#.#....#.#.......#.#...........#.#......#.#....#.#.....#.#.....#.#......#.#.......#.#......
;; 120 5510 42 ..........#.#..............#.#....#.#......#.#....#.#....#.#....#.#....#.#....#.#........#.#....#.#......#.#....#.#.......#.#...........#.#......#.#....#.#.....#.#.....#.#......#.#.......#.#....
;; 121 5552 42 ..........#.#..............#.#....#.#......#.#....#.#....#.#....#.#....#.#....#.#........#.#....#.#......#.#....#.#.......#.#...........#.#......#.#....#.#.....#.#.....#.#......#.#.......#.#........
;; 122 5594 42 ..........#.#..............#.#....#.#......#.#....#.#....#.#....#.#....#.#....#.#........#.#....#.#......#.#....#.#.......#.#...........#.#......#.#....#.#.....#.#.....#.#......#.#.......#.#......

;; For 50B iterations, less 120, our sum total will be 49999999880*42 +
;; whatever we were at on 119 (0-based) = 2100000000428
;;
;; I cheated a little on this.  For the first round, no problem.  I *think* I
;; had the right idea, but I had some sloppy bugs in the code to abridge the
;; window into the infinite space of the line.  (Specifically I was truncating
;; and expanding it at the wrong times, and not by enough.) But I had some
;; hints there was a glider involved, although I had most of the code to figure
;; it out.


(defpackage :aoc12
  (:use :common-lisp))

(in-package :aoc12)

(defconstant +initial-state+
  "###......#.#........##.###.####......#..#####.####..#.###..#.###.#..#..#.#..#..#.##...#..##......#.#")

(defvar *rules* (make-rules))

(defun make-rules ()
  (let ((ht (make-hash-table :test 'equalp)))
    (mapcar (lambda (rule)
              (setf (gethash (car rule) ht) (cdr rule)))
            '(
              ("..#.#" . #\.)
              ("#..##" . #\#)
              ("..###" . #\.)
              ("###.#" . #\.)
              ("#...#" . #\#)
              ("###.." . #\#)
              (".##.#" . #\#)
              ("#..#." . #\#)
              ("#.##." . #\#)
              ("####." . #\.)
              (".#.##" . #\#)
              ("...#." . #\.)
              (".#..#" . #\#)
              (".###." . #\.)
              ("##..#" . #\#)
              (".##.." . #\#)
              (".####" . #\#)
              (".#.#." . #\#)
              ("#####" . #\.)
              ("#.#.#" . #\#)
              ("...##" . #\#)
              ("..##." . #\.)
              ("....#" . #\.)
              ("##..." . #\.)
              ("##.#." . #\#)
              ("..#.." . #\#)
              ("....." . #\.)
              ("##.##" . #\.)
              ("#.###" . #\.)
              ("#.#.." . #\.)
              (".#..." . #\#)
              ("#...." . #\.)
              ;; tests
              ;; ("...##" . #\#)
              ;; ("..#.." . #\#)
              ;; (".#..." . #\#)
              ;; (".#.#." . #\#)
              ;; (".#.##" . #\#)
              ;; (".##.." . #\#)
              ;; (".####" . #\#)
              ;; ("#.#.#" . #\#)
              ;; ("#.###" . #\#)
              ;; ("##.#." . #\#)
              ;; ("##.##" . #\#)
              ;; ("###.." . #\#)
              ;; ("###.#" . #\#)
              ;; ("####." . #\#)
              ))
    ht))

(defun dumb-step-once (last-gen)
  (let* ((slen (length last-gen))
         (new-state
           (make-array `(,slen) :element-type 'character :initial-element #\.)))
    (loop :for i :from 2 :to (- slen 3)
          :for state-here = (subseq last-gen (- i 2) (+ i 3)) :do
            (setf (aref new-state i) 
                  (gethash state-here *rules*)))
    new-state))

(defun expand-prefix (offset s)
  (cond
    ((equalp "....." (subseq s 0 5))
     (list (+ offset 1) (subseq s 1)))
    ((or (eq (aref s 0) #\#)
         (eq (aref s 1) #\#)
         (eq (aref s 2) #\#)
         (eq (aref s 3) #\#)
         (eq (aref s 4) #\#))
     (list (- offset 5) (concatenate 'string "....." s)))
    (t (list offset s))))

(defun expand-suffix (s)
  (let ((len (length s)))
    (cond
      ((equalp "....." (subseq s (- len 5) len))
       (subseq s 0 (- len 1)))
      ((or (eq (aref s (- len 1)) #\#)
           (eq (aref s (- len 2)) #\#)
           (eq (aref s (- len 3)) #\#)
           (eq (aref s (- len 4)) #\#)
           (eq (aref s (- len 5)) #\#))
       (concatenate 'string s "....."))
      (t s))))

(defun adjust (old-offset old-state)
  (expand-prefix old-offset (expand-suffix old-state)))
       
(defun step-once (dirty-last-offset dirty-last-state)
  (destructuring-bind (offset last-state)
      (adjust dirty-last-offset dirty-last-state)
    (loop :with new-state = (make-string (length last-state) :initial-element #\.)
          :for i :from 2 :below (- (length last-state) 2)
          :for state-here = (subseq last-state (- i 2) (+ i 3))
          :do (setf (aref new-state i)
                    (gethash state-here *rules* #\.))
          :finally (return (values offset new-state)))))

;; used this to explore the glider when it became stable
(defun step-n (state &key n (offset 0))
  (loop :with prev-score = 0
        :with new-score = 0
        :for i :from 0 :below n
        :do (setf (values offset state) (step-once offset state))
        :do (setf prev-score new-score)
        :do (setf new-score (score offset state))
        :do (format t "~a ~a ~a ~a~%" i new-score (- new-score prev-score) state)
        :finally (return (list offset state))))

;; part of my part 1 soln
(defun step-20 ()
  ;; -25 offset baked in
  (loop :with state = ".........................###......#.#........##.###.####......#..#####.####..#.###..#.###.#..#..#.#..#..#.##...#..##......#.#........................."
        :repeat 20
        :do (setf state (dumb-step-once state))
        :finally (return state)))

(defun smart-step-20 ()
  ;; -25 offset baked in
  (loop :with state = ".........................###......#.#........##.###.####......#..#####.####..#.###..#.###.#..#..#.#..#..#.##...#..##......#.#........................."
        :repeat 20
        :with offset = -25
        :for i :from 0
        :do (setf (values offset state) (step-once offset state))
        :do (format t "~a ~a ~a~%" i offset state)
        :finally (return (values offset state))))

;; part of part 2 soln
(defun score-20 ()
  (let ((final-state (step-20)))
    (score -25 (step-20))))

(defun score (offset state)
  (loop :for i :from offset
        :for j :from 0 :below (length state)
        :summing (if (eq (aref state j) #\#) i 0)))
