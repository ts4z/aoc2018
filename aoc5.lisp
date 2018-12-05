;;;; day 5

;;; I tried doing this in Perl. I actually thought about doing it in C, because
;;; the array syntax has always read better to me there.  But I wimped out and
;;; did it in Lisp again because repls are addictive.

;;; I got a little smarter and broke this down a little bit.  collapse-length
;;; and collapse came from attempting to optimize.  It does not appear to make
;;; much difference (this is plenty fast).  One accidental optimization is in
;;; mostly-answer-two, where I accidentally pre-collapsed the input.  This does
;;; help a fair amount.

(defpackage :aoc5
  (:use :common-lisp))

(in-package :aoc5)

(defun switch-case (ch)
  "Return the opposite-case version of CH."
  (check-type ch character)
  (cond ((lower-case-p ch)   (char-upcase ch))
        ((upper-case-p ch) (char-downcase ch))
        (t ch)))

(defun matched-pair-p (c1 c2)
  ;; (format t "matched pair? ~a~a~%" c1 c2)
  (eq c1 (switch-case c2)))

(defun collapse-internal (input)
  (check-type input string)
  (let* ((output (make-array (length input)))
         (j 0))                         ; output position
    (loop :with i = 0                   ; input position
          :while (< i (length input))
          :do (progn
                (setf (aref output j) (aref input i))
                (incf i)
                (incf j)
                (loop :while (>= j 2)
                      :while (matched-pair-p (aref output (- j 1))
                                             (aref output (- j 2)))
                      ;; :do (format t "matchy")
                      :do (decf j 2))))
    (values output j)))

(defun collapse (input)
  (multiple-value-bind (output j)
      (collapse-internal input)
    ;; coerce to string:
    ;; (concatenate 'string (adjust-array output j))))
    ;; don't bother, just tweak the array
    (adjust-array output j)))

(defun collapse-length (input)
  (multiple-value-bind (output j)
      (collapse-internal input)
    j))

(defun answer-one ()    
  (with-open-file (in "/home/tjs/git/aoc2018/5.input")
    (collapse-length (read-line in))))

(defun recognize-char-predicate (code)
  "Return a predicate that returns true iff the input character is the same as
CODE."
  (check-type code integer)
  (let ((target (code-char code)))
    (lambda (ch)
      (or (eq ch target)
          (eq ch (switch-case target))))))
      

(defun consider-removals (s)
  (loop :for i :from (char-code #\A) :to (char-code #\Z)
        :collect (cons (code-char i)
                       (collapse-length (remove-if (recognize-char-predicate i) s)))))
                       
(defun mostly-answer-two ()
  "Mostly answer day 5 part 2, but return all the data, not just the answer."
  (with-open-file (in "/home/tjs/git/aoc2018/5.input")
    ;; we collapse up front, so there's less work to do on each pass.
    ;; was accidental, but saves work on every pass.
    (consider-removals (collapse (read-line in)))))

(defun answer-two ()
  "Just return the answer for day 5 part 2."
  (cdar (sort (mostly-answer-two) #'< :key #'cdr)))
