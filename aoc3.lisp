(defpackage :aoc3
  (:use :common-lisp :cl-ppcre :ioutil))

(in-package :aoc3)

(defvar *input-file* "3.input")

(defvar *line-format* 
  "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)")

(defvar *scanner*
  (cl-ppcre:create-scanner *line-format*))

(defun max-x-dim (claims)
  (apply #'max 0 (mapcar #'claim-to-x claims)))

(defun max-y-dim (claims)
  (apply #'max 0 (mapcar #'claim-to-y claims)))

(defstruct claim
  (id nil :read-only true :type integer)
  (from-x nil :read-only true :type integer)
  (to-x nil :read-only true :type integer)
  (from-y nil :read-only true :type integer)
  (to-y nil :read-only true :type integer))

(defun scan-line (s)
  (ioutil:ppcre-register-groups-bind
      (id-s from-x-s from-y-s x-width-s y-width-s)
      (*scanner* s :sharedp t)
    (let ((from-x (parse-integer from-x-s))
          (from-y (parse-integer from-y-s))
          (x-width (parse-integer x-width-s))
          (y-width (parse-integer y-width-s)))
      (make-claim :id (parse-integer id-s)
                  :from-x from-x
                  :to-x (+ from-x x-width)
                  :from-y from-y
                  :to-y (+ from-y y-width)))))

(defun read-claims ()
  (ioutil:read-input *input-file* #'scan-line))

(defvar *claims*
  (read-claims))

;; return answer to 3a
(defun count-overlapping-claims (claims)
  (let* ((r 0)
         (max-x (max-x-dim claims))
         (max-y (max-y-dim claims))
         (a (make-array (list max-x max-y))))
    (loop for claim in claims do
      (loop for x from (claim-from-x claim) to (1- (claim-to-x claim)) do
        (loop for y from (claim-from-y claim) to (1- (claim-to-y claim)) do
          (incf (aref a x y)))))
    (loop for x from 0 to (1- max-x) do
      (loop for y from 0 to (1- max-y) do
        (when (> (aref a x y) 1)
          (incf r))))
    r))

;; return answer to part 3b
(defun find-unique-claims (claims)
  (let* ((max-x (max-x-dim claims))
         (max-y (max-y-dim claims))
         (a (make-array (list max-x max-y))))

    (flet ((scan-claim-for-overlap (c)
             (check-type c claim)
             (loop
               :named x-loop
               :for x :from (claim-from-x c) :to (1- (claim-to-x c))
               :do (loop 
                     :for y :from (claim-from-y c) :to (1- (claim-to-y c))
                     :do (when (> (aref a x y) 1)
                           (return-from x-loop t))))))

      ;; build map that counts overlapping claims
      (loop for c in claims do
        (loop for x from (claim-from-x c) to (1- (claim-to-x c)) do
          (loop for y from (claim-from-y c) to (1- (claim-to-y c)) do
            (incf (aref a x y)))))

      (remove-if (lambda (c) (scan-claim-for-overlap c)) claims))))
