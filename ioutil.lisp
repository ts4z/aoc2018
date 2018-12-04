;;;; trivial io utilities for trivial problems.

(defpackage :ioutil
  (:use :common-lisp :cl-ppcre)
  (:export :do-or-nil :read-input
           :snarf-file :ppcre-register-groups-bind))

(in-package :ioutil)

(defmacro do-or-nil (form)
  "Do, or do not. And if you do not, return nil."
  `(handler-case ,form
     (t () nil)))

(defun read-line-or-nil (stream)
  "Read a line from STREAM. If anything goes wrong, return nil."
  (check-type stream stream)
  (do-or-nil (read-line stream)))

(defun snarf-file (name)
  "Read filename into a list of lines."
  (with-open-file (input name)
    (loop for line = (do-or-nil (read-line input))
          while line
          collect line)))

(defun read-input (filename handle-line)
  "Read FILENAME as a stream and for each line, call HANDLE-LINE.  I/O errors
on the underlying stream will stop the read of the file."
  (check-type filename string)
  (check-type handle-line function)
  (with-open-file (in filename)
    (loop for line = (read-line-or-nil in)
          while line
          collect (funcall handle-line line))))

;; It could be cool if this could take type specs for the variables in e and
;; apply those to the macro; it would save the trouble of, say, calling
;; `parse-integer' repeatedly.
(defmacro ppcre-register-groups-bind (e f &body c)
  "A wrapper around the cl-pprcce macro of the similar name.  This asserts (via
`check-type') all arguments in the vars list E are of type string.  Usage is
identical to cl-ppcre:register-groups-bind."
  `(cl-ppcre:register-groups-bind ,e ,f
     ,@(mapcar (lambda (x) `(check-type ,x string)) e)
     ,@c))
