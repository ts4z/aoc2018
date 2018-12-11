;; template code; 30% nonsense

(defpackage :aoc99
  (:use :common-lisp))

(in-package :aoc99)

(defvar +input-file+ #P"/home/tjs/git/aoc2018/11.input")

(defvar *input*)

(defun scan-by-line ()
  (setf *input* (scan-by-line-from (file))))

(defvar *line-pattern*
  ".")

(defun scan-by-line-from (file)
  ;; position=<-32391,  32789> velocity=< 3, -3>
  (let* ((pattern *line-pattern*)
         (scanner (cl-ppcre:create-scanner pattern)))
    (loop
      :for line in (ioutil:snarf-file file)
      :collect (ppcre:register-groups-bind ((#'parse-integer x y xd yd))
                   (scanner line)
                 (cons 'and-a-one 'and-a-two)))))

(defun read-one (file)
  (with-open-file (in file)
    (read-node in)))

(defun read-node (stream)
  nil)
