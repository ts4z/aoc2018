(ql:quickload "alexandria")
(ql:quickload "cl-ppcre")

(defpackage :aoc16
  (:use :common-lisp :alexandria :cl-ppcre))

(in-package :aoc16)

(defstruct test-case
  before
  op
  after)

(defparameter *registers*)

(defun zero-registers ()
  (setf *registers* (make-array 4 :element-type 'integer)))

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

(defun ops ()
  (list #'i-addi #'i-muli #'i-bani #'i-bori #'i-addr #'i-mulr #'i-banr #'i-borr
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

(defun read-test-case-input-file (&optional name)
  (with-open-file (in (or name +input-file+))
    (loop :for tc = (read-record in)
          :while tc
          :do (format t "~a~%" tc)
          :collect tc)))

(defvar *test-cases*)

(defun read-test-cases ()
  (setf *test-cases* (read-test-case-input-file)))

(defconstant +code-input-file+ #P"/home/tjs/git/aoc2018/16-code.input")

(defun read-input-code (&optional name)
  (setf *input-code* 
        (with-open-file (in (or name +code-input-file+))
          (loop :for line = (read-line in nil)
                :while line
                :collect (cl-ppcre:register-groups-bind ((#'parse-integer op a b c))
                             (*code-scanner* line)
                           (list op a b c))))))

(defvar *input-code*)
     

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

;; this is updated by 
(defun unassigned-slots ()
  (remove-if-not (lambda (i) (eq #'i-noop (inst i)))
                 (loop :for i :from 0 :below 16 :collect i)))


(defun i-noop (a b c) (declare (ignore a b c)))

;; couldn't get this to work in an array literal; dunno why
;;
;; to compute this, I called
;;   (mapcar (lambda (x) (cons x (length (does-it-blend x)))) (unassigned-slots))
;; across, picked whichever one was cdr 1, and added it to the array.
;; this could have been made better but I didn't care about automating it
;; more, I just wanted the answer, and I was a little frustrated as to why
;; I couldn't make this a jump table.  (I ended up with symbols in the table instead
;; of closures and worked around it.)
(defun inst (n)
  (switch (n)
    (0 #'i-muli)
    (1 #'i-bani)
    (2 #'i-addi)
    (3 #'i-seti)

    (4 #'i-eqrr)
    (5 #'i-eqir)
    (6 #'i-setr)
    (7 #'i-bori)

    (8 #'i-gtri)
    (9 #'i-eqri)
    (10 #'i-gtir)
    (11 #'i-borr)

    (12 #'i-addr)
    (13 #'i-gtrr)
    (14 #'i-mulr)
    (15 #'i-banr)))


(defun unassigned-ops ()
  (loop :with unassigned
        :for op :in (ops)
        :for found-it = (loop :with found-it = nil
                              :for i :from 0 :below 16
                              :do (setf found-it (or found-it (and (eql (inst i) op)
                                                                   op)))
                              :finally (return found-it))
        :do (unless found-it
              (push op unassigned))
        :finally (return unassigned)))

  
(defun does-it-blend (i)
  (remove-if-not
   (lambda (res) (= 1 (car res)))
   (let* ((limited-tests (remove-if-not (lambda (tc) (= (car (test-case-op tc)) i)) *test-cases*))
          (nt (length limited-tests)))
     (when (> nt 0)
       (mapcar (lambda (op)
                 (let ((nok (count t (mapcar (lambda (tc)
                                               (apply-op-to-args :op op :tc tc))
                                             limited-tests))))
                   (format t "op ~a passed ~a/~a (~a)~%" op nok nt (float (/ nok nt)))
                   (cons (/ nok nt) op)))
               (unassigned-ops))))))

(defun slots-across-unassigned ()
  (loop :for slot :in (unassigned-slots) :do
        (does-it-blend slot)))
   
(defun interpreter ()
  (loop :for (op a b c) in *input-code*
        :do (funcall (inst op) a b c))
  (reg 0))
