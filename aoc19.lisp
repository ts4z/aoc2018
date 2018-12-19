(ql:quickload "alexandria")
(ql:quickload "cl-ppcre")

(defpackage :aoc19
  (:use :common-lisp :alexandria :cl-ppcre))

(in-package :aoc19)

(defstruct test-case
  before
  op
  after)

(defvar *registers*)

(defconstant +nregisters+ 6)

(defun zero-registers ()
  (setf *registers* (make-array +nregisters+ :element-type 'integer)))

;; must be set according to input
(defvar *ip-register* 4)

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
(reg-imm-op i-bani logand)
(reg-imm-op i-bori logior)

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

(defparameter *instructions*
      (alist-hash-table `(
                      ("addi" . ,#'i-addi)
                      ("addr" . ,#'i-addr)
                      ("bani" . ,#'i-bani)
                      ("banr" . ,#'i-banr)
                      ("bori" . ,#'i-bori)
                      ("borr" . ,#'i-borr)
                      ("eqir" . ,#'i-eqir)
                      ("eqri" . ,#'i-eqri)
                      ("eqrr" . ,#'i-eqrr)
                      ("gtir" . ,#'i-gtir)
                      ("gtri" . ,#'i-gtri)
                      ("gtrr" . ,#'i-gtrr)
                      ("muli" . ,#'i-muli)
                      ("mulr" . ,#'i-mulr)
                      ("seti" . ,#'i-seti)
                      ("setr" . ,#'i-setr)
                      )
                    :test 'equalp
                    ))

(defvar *program*)

(defun program-is-input ()
  (setf *ip-register* 4)
  (let ((program `(
                   (,#'i-addi 4 16 4)   ;0    goto 17
                   (,#'i-seti 1 8 1)    ;     r[1] = 1
                   (,#'i-seti 1 3 5)    ;2    r[5] = 1
                   (,#'i-mulr 1 5 3)    ;3    r[3] = r[5] * r[1]
                   (,#'i-eqrr 3 2 3)    ;     if r[2] == r[3]...
                   (,#'i-addr 3 4 4)    ;5    ... goto 7
                   (,#'i-addi 4 1 4)    ;6    ... else goto 8
                   (,#'i-addr 1 0 0)    ;     r[0] += r[1]       <-- # times r[2] == r[3]
                   (,#'i-addi 5 1 5)    ;8    r[5] += 1
                   (,#'i-gtrr 5 2 3)    ;     if r[5] > r[2]...
                   (,#'i-addr 4 3 4)    ;10   ...goto 12
                   (,#'i-seti 2 2 4)    ;     ... else goto 3
                   (,#'i-addi 1 1 1)    ;12   r[1] += 1
                   (,#'i-gtrr 1 2 3)    ;     r[3] = r[1] > r[2] ...
                   (,#'i-addr 3 4 4)    ;     ... so if r[1] > r[2], goto 16
                   (,#'i-seti 1 4 4)    ;15   goto 1
                   (,#'i-mulr 4 4 4)    ;16   r[4] *= r[4], 16*16, so we're done.
                   (,#'i-addi 2 2 2)    ;17   r[2] += 2     (so probably r[2] = 2)
                   (,#'i-mulr 2 2 2)    ;     r[2] *= r[2]  r[2] = 4
                   (,#'i-mulr 4 2 2)    ;     r[2] *= 19    (r[4] must be 19)  r[2]=76
                   (,#'i-muli 2 11 2)   ;20   r[2] *= 11      =836
                   (,#'i-addi 3 6 3)    ;     r[3] += 6     r[3] = 6
                   (,#'i-mulr 3 4 3)    ;     r[3] *= 22    =132
                   (,#'i-addi 3 8 3)    ;     r[3] += 8     =140
                   (,#'i-addr 2 3 2)    ;     r[2] += r[3]  =976
                   (,#'i-addr 4 0 4)    ;25   r[4] += r[0]  (skip next 0 or 1, depending on init?)
                   (,#'i-seti 0 1 4)    ;     goto 1
                   (,#'i-setr 4 4 3)    ;     r[3] = 27     (r[4] must be 27)
                   (,#'i-mulr 3 4 3)    ;28   r[3] *= r[4]  so r[3] = 756
                   (,#'i-addr 4 3 3)    ;     r[3] += 29    so r[3] = 785
                   (,#'i-mulr 4 3 3)    ;30   r[3] *= 30    so r[3] = 23550
                   (,#'i-muli 3 14 3)   ;     r[3] *= 14    r[3] = 329700
                   (,#'i-mulr 3 4 3)    ;     r[3] *= 32    r[4] = 10550400
                   (,#'i-addr 2 3 2)    ;     r[2] += r[3]  r[2] = 10551376
                   (,#'i-seti 0 4 0)    ;     r[0] = 0
                   (,#'i-seti 0 7 4)))) ;35   goto 1
    (setf *program*
          (make-array (length program) :initial-contents program))))

(defun program-is-test ()
  (setf *ip-register* 0)
  (setf *program*
        #((#'seti 5 0 1)
          (#'seti 6 0 2)
          (#'addi 0 1 0)
          (#'addr 1 2 3)
          (#'setr 1 0 0)
          (#'seti 8 0 4)
          (#'seti 9 0 5))))

(defmacro ip ()
  `(reg *ip-register*))

(defun one-instruction-save-registers () ;useful in simulating example
  (destructuring-bind (op a b c)
      (aref *program* (ip))
    (funcall op a b c)
    (prog1 (copy-array *registers*)
      (incf (ip)))))

(defun one-instruction ()
  (destructuring-bind (op a b c)
      (aref *program* (ip))
    (funcall op a b c)
    (incf (ip))))

(defun opr ()
  (let ((ip-before (ip))
        (before (copy-array *registers*))
        (inst (aref *program* (ip)))
        (after (one-instruction-save-registers)))
    (format t "ip=~a ~a ~a ~a~%" ip-before before inst after)))

(defun pr ()
  (format t "ip=~a ~a ~a~%" (ip) (aref *program* (ip)) *registers*))

;; implementing gdb in lisp?
(defun n (&optional howmany)
  (if (null howmany) (setf howmany 1))
  (loop :while (ip-ok) :repeat howmany :do (one-instruction))
  (pr))

(defun ip-ok ()
  (and (<= 0 (ip)) (< (ip) (length *program*))))

(defun reset ()
  (zero-registers))

(defun run-1 ()                         ;part one: => 1922
  (reset)
  (loop :while (ip-ok)
        :do (one-instruction))
  (reg 0))

(defun setup-2 ()
  (reset)
  (setf (aref *registers* 0) 1))

(defun run-2 ()                         ;part two; runs more or less forever
  (setup-2)
  (loop :while (and (<= 0 (ip)) (< (ip) (length *program*)))
        :do (one-instruction))
  (reg 0))

;; in progress:
;; AOC19> *registers*
;; #(372 217 10551376 0 5 3402676)

;; this didn't work so good (listen to the fans spin!), so I eventually went
;; back and decoded the part 1 program and discovered that it's computing sum
;; of factors.  then I used wolfram alpha to get the factors for the large
;; intermediate number.
