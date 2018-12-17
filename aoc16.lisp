(ql:quickload "alexandria")
(ql:quickload "cl-ppcre")

(defpackage :aoc16
  (:use :common-lisp :alexandria :cl-ppcre))

(in-package :aoc16)

(defstruct test-case
  before
  op
  after)

(defparameter *registers* (make-array 4 :element-type 'integer))

(defmacro reg (r)
  `(aref *registers* ,r))

(defun set-reg (x y)
  (setf (reg x) y))

(defun set-registers (rs)
  (loop :for i :from 0 :below (array-dimension *registers* 0)
        :for v across rs
        :do (setf (reg i) v)))

(defmacro reg-reg-op (name fn)
  `(defun ,name (a b c)
     (setf (reg c) (,fn (reg a) (reg b)))))

(defmacro reg-imm-op (name fn)
  `(defun ,name (a b c)
     (setf (reg c) (,fn (reg a) b))))

(reg-imm-op i-addi +)
(reg-imm-op i-muli *)
(reg-imm-op i-banr logand)
(reg-imm-op i-borr logior)

(reg-reg-op i-addr +)
(reg-reg-op i-mulr *)
(reg-reg-op i-banr logand)
(reg-reg-op i-borr logior)

(defun i-setr (a b c)  (declare (ignore b)) (setf (reg c) (reg a)))
(defun i-seti (a b c)  (declare (ignore b)) (setf (reg c) a))
(defun i-gtir (a b c)  (setf (reg c) (if (> a (reg b)) 1 0)))
(defun i-gtri (a b c)  (setf (reg c) (if (> (reg a) b) 1 0)))
(defun i-gtrr (a b c)  (setf (reg c) (if (> (reg a) (reg b)) 1 0)))
(defun i-eqir (a b c)  (setf (reg c) (if (= a (reg b)) 1 0)))
(defun i-eqri (a b c)  (setf (reg c) (if (= (reg a) b) 1 0)))
(defun i-eqrr (a b c)  (setf (reg c) (if (= (reg a) (reg b)) 1 0)))

(defun ops ()
  (list #'i-addi #'i-muli #'i-banr #'i-borr #'i-addr #'i-mulr #'i-banr #'i-borr
        #'i-setr #'i-seti #'i-gtri #'i-gtir #'i-gtrr #'i-eqri #'i-eqir #'i-eqrr))

(defconstant +input-file+ #P"/home/tjs/git/aoc2018/16-tc.input")

(defparameter *before-after-scanner*
  (cl-ppcre:create-scanner
   "(Before|After): ? \\[(\\d+), (\\d+), (\\d+), (\\d+)\\]"))
  
(defun parse-before-after (which s)
  (cl-ppcre:register-groups-bind (tag (#'parse-integer r1 r2 r3 r4))
      (*before-after-scanner* s)
    (when (string-equal which tag)
      (make-array '(4) :element-type 'integer
                       :initial-contents (list r1 r2 r3 r4)))))

(defvar *code-scanner*
  (cl-ppcre:create-scanner "(\\d+) (\\d+) (\\d+) (\\d+)"))

(defun parse-code (s)
    (cl-ppcre:register-groups-bind ((#'parse-integer op a b c))
        (*code-scanner* s)
      (list op a b c)))

(defun read-line-or-dont (stream)
  (read-line stream nil))

(defun read-record (in)
  (let (before code after blank)
    (if (and (setf before (parse-before-after "Before" (read-line-or-dont in)))
             (prog1 t (format t "code: ~a~%" before))
             (setf code   (parse-code (read-line-or-dont in)))
             (prog1 t (format t "after: ~a~%" code))
             (setf after  (parse-before-after "After" (read-line-or-dont in)))
             (prog1 t (format t "blank: ~a~%" after))
             (setf blank  (read-line-or-dont in)))
        (make-test-case :before before :op code :after after))))

(defun read-input-file (&optional name)
  (with-open-file (in (or name +input-file+))
    (loop :for tc = (read-record in)
          :while tc
          :do (format t "~a~%" tc)
          :collect tc)))

(defvar *test-cases*)

(defun read-test-cases ()
  (setf *test-cases* (read-input-file)))


(defun test-case-ok (tc)
  (equalp *registers* (test-case-after tc)))

(defun apply-op-to-args (&key tc op)
  (check-type tc test-case)
  (check-type op function)

  (set-registers (test-case-before tc))
  (apply op (cdr (test-case-op tc)))
  (test-case-ok tc))

(defun part-one ()
  (read-test-cases)

  (let ((all-ops (ops)))
    (count-if (lambda (n) (>= n 10))
              (mapcar (lambda (test-case)
                        (count t (mapcar (lambda (op)
                                           (apply-op-to-args :tc test-case :op op))
                                         all-ops)))
                      *test-cases*))))

