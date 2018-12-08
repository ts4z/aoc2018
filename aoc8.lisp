;;; This is all lists, because it reads better that way.  Arrays could have
;;; slightly higher performance if the trees are very bushy (in the input data,
;;; they are not).

;; AOC8> (time (part-one))
;; Evaluation took:
;;   0.009 seconds of real time
;;   0.008619 seconds of total run time (0.008588 user, 0.000031 system)
;;   100.00% CPU
;;   29,234,958 processor cycles
;;   393,200 bytes consed
  
;; 47464 (16 bits, #xB968)

;; AOC8> (time (part-two))
;; Evaluation took:
;;   0.008 seconds of real time
;;   0.008320 seconds of total run time (0.008320 user, 0.000000 system)
;;   100.00% CPU
;;   27,445,800 processor cycles
;;   425,984 bytes consed
  
;; 23054 (15 bits, #x5A0E)

;; A little examination suggests that more or less all of the above time is
;; I/O.

(defpackage :aoc8
  (:use :common-lisp))

(in-package :aoc8)

(defparameter *input-file* "/home/tjs/git/aoc2018/8.input")

(defstruct node
  (children nil)
  (metadata nil))

(defun read-node (in)
  (let* ((nc (read in))
         (nm (read in))
         ;; children could be an array for faster access later.
         (children (loop :repeat nc :collect (read-node in)))
         (metadata (loop :repeat nm :collect (read in))))
    (make-node :children children :metadata metadata)))

(defun read-input-from (file)
  (with-open-file (in file)
    (read-node in)))

(defun read-input ()
  (read-input-from *input-file*))

;; as per maphash
(defun mapnode (fn n)
  (check-type n node)
  (funcall fn n)
  (mapc (lambda (ch) (mapnode fn ch)) (node-children n)))

(defun total-metadata (tree)
  (let ((ttl 0))
    (mapnode (lambda (n)
               (mapc (lambda (v) (incf ttl v))
                       (node-metadata n)))
             tree)
    ttl))

(defun part-one ()
  (total-metadata (read-input)))

(defun value-of-node (n)
  (apply #'+ (if (null (node-children n))
                 (node-metadata n)
                 (mapcar (lambda (md)
                           (cond
                             ((= md 0) 0)
                             ((> md (length (node-children n))) 0)
                             (t (value-of-node (elt (node-children n) (1- md))))))
                         (node-metadata n)))))

(defun part-two ()
  (value-of-node (read-input)))
