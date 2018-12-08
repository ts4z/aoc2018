;;; This is all lists, because it reads better that way.  Arrays could have
;;; slightly higher performance if the trees are very bushy (in the input data,
;;; they are not).

;; AOC8> (time (part-two))
;; Evaluation took:
;;   0.008 seconds of real time
;;   0.008182 seconds of total run time (0.008117 user, 0.000065 system)
;;   100.00% CPU
;;   26,990,901 processor cycles
;;   688,096 bytes consed
  
;; 47464 (16 bits, #xB968)

(defpackage :aoc8
  (:use :common-lisp :cl-ppcre :ioutil))

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
  (map 'nil (lambda (child)
              (mapnode fn child))
       (node-children n)))

(defun total-metadata (tree)
  (let ((ttl 0))
    (mapnode (lambda (n)
               (mapcar (lambda (v) (incf ttl v))
                       (node-metadata n)))
             tree)
    ttl))

(defun part-one ()
  (total-metadata (read-input)))

(defun value-of-node (n)
  (apply #'+ (if (null (node-children n))
                 (node-metadata n)
                 (loop :with children = (node-children n)
                       :for md :in (node-metadata n)
                       :collect (cond
                                  ((= md 0) 0)
                                  ((> md (length children)) 0)
                                  (t (value-of-node (elt children (1- md)))))))))

(defun part-two ()
  (total-metadata (read-input)))
