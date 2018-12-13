;; template code; 30% nonsense

(ql:quickload "alexandria")

(defpackage :aoc13
  (:use :common-lisp :alexandria))

(in-package :aoc13)

(defvar +exp-input-file+ #P"/home/tjs/git/aoc2018/13-exp.input")


(defvar +test-input-file+ #P"/home/tjs/git/aoc2018/13-test.input")


(defvar +input-file+ #P"/home/tjs/git/aoc2018/13.input")

(defvar *grid*)

(defvar *carts*)

(defun scan-by-line-from-stream (file)
  (let* ((lines (ioutil:snarf-file file))
         (width (loop :for line :in lines
                      :maximizing (length line))))
    (loop
      :with r = (make-array `(,(length lines) ,width)
                            :element-type 'character
                            :initial-element #\Space)
      :for y :from 0
      :for line :in lines :do
        (loop :for x :from 0 :below (length line) :do
          (setf (aref r y x) (aref line x)))
      :finally (return r))))

(defun load-input-grid ()
  (setf *grid* (scan-by-line-from-stream +input-file+))
  *grid*)

(defun load-input-grid-for-test ()
  (setf *grid* (scan-by-line-from-stream +test-input-file+))
  *grid*)

(defun load-input-grid-for-exp ()
  (setf *grid* (scan-by-line-from-stream +exp-input-file+))
  *grid*)


(defvar *car-to-track*
  (alexandria:alist-hash-table '((#\< . #\-)
                                 (#\> . #\-)
                                 (#\v . #\|)
                                 (#\^ . #\|))))

(defstruct cart
  dir
  y
  x
  (nt :left))

(define-condition cant-go-that-way (error)
  ((cart :initarg :cart :reader cart)
   (track :initarg :track :reader track))
  (:report (lambda (c s)
             (format s "can't go that way (cart ~a on track '~a')~&"
                     (cart c) (track c)))))

(define-condition crash (error)
  ((x :initarg :x :reader x)
   (y :initarg :u :reader y))
  (:report (lambda (c s)
             (format s "crash at ~a,~a~&" (x c) (y c)))))

;; next nt value
(defun nnt (nt)
  (switch (nt)
    (:left     :straight)
    (:straight :right)
    (:right    :left)))

(defun move-cart (cart)
  (let* ((dir (cart-dir cart))
         (x   (cart-x   cart))
         (y   (cart-y   cart))
         (nt  (cart-nt  cart))
         (tk  (aref *grid* y x)))
    (cond
      ((eql tk #\+)
       (switch (dir)
         (#\< (switch (nt)
                (:left     (make-cart :nt (nnt nt) :dir #\v :x x :y (1+ y)))
                (:straight (make-cart :nt (nnt nt) :dir dir :x (1- x) :y y))
                (:right    (make-cart :nt (nnt nt) :dir #\^ :x x :y (1- y)))))
         (#\> (switch (nt)
                (:left     (make-cart :nt (nnt nt) :dir #\^ :x x :y (1- y)))
                (:straight (make-cart :nt (nnt nt) :dir dir :x (1+ x) :y y))
                (:right    (make-cart :nt (nnt nt) :dir #\v :x x :y (1+ y)))))
         (#\^ (switch (nt)
                (:left     (make-cart :nt (nnt nt) :dir #\< :x (1- x) :y y))
                (:straight (make-cart :nt (nnt nt) :dir dir :x x :y (1- y)))
                (:right    (make-cart :nt (nnt nt) :dir #\> :x (1+ x) :y y))))
         (#\v (switch (nt)
                (:left     (make-cart :nt (nnt nt) :dir #\> :x (1+ x) :y y))
                (:straight (make-cart :nt (nnt nt) :dir dir :x x :y (1+ y)))
                (:right    (make-cart :nt (nnt nt) :dir #\< :x (1- x) :y y))))))
       
      ((and (eql dir #\<) (eql tk #\-)) (make-cart :nt nt :dir dir :x (1- x) :y y))
      ((and (eql dir #\<) (eql tk #\/)) (make-cart :nt nt :dir #\v :x x :y (1+ y)))
      ((and (eql dir #\<) (eql tk #\\)) (make-cart :nt nt :dir #\^ :x x :y (1- y)))
      ((and (eql dir #\>) (eql tk #\-)) (make-cart :nt nt :dir dir :x (1+ x) :y y))
      ((and (eql dir #\>) (eql tk #\/)) (make-cart :nt nt :dir #\^ :x x :y (1- y)))
      ((and (eql dir #\>) (eql tk #\\)) (make-cart :nt nt :dir #\v :x x :y (1+ y)))
      ((and (eql dir #\^) (eql tk #\/)) (make-cart :nt nt :dir #\> :x (1+ x) :y y))
      ((and (eql dir #\^) (eql tk #\\)) (make-cart :nt nt :dir #\< :x (1- x) :y y))
      ((and (eql dir #\^) (eql tk #\|)) (make-cart :nt nt :dir dir :x x :y (1- y)))
      ((and (eql dir #\v) (eql tk #\/)) (make-cart :nt nt :dir #\< :x (1- x) :y y))
      ((and (eql dir #\v) (eql tk #\\)) (make-cart :nt nt :dir #\> :x (1+ x) :y y))
      ((and (eql dir #\v) (eql tk #\|)) (make-cart :nt nt :dir dir :x x :y (1+ y)))

      (t (error 'cant-go-that-way :cart cart :track tk)))))

(defvar *to-next-cell*
  (alexandria:alist-hash-table
   '((#\< . #\-)
     (#\> . #\-)
     (#\v . #\|)
     (#\^ . #\|))))

;; modifies grid
(defun collect-carts ()
  (let (carts)
    (loop :for y :from 0 :below (array-dimension *grid* 0) :do
      (loop :for x :from 0 :below (array-dimension *grid* 1)
            :for sq = (aref *grid* y x) :do
              (let ((maybe-track (gethash sq *car-to-track*)))
                (when maybe-track
                  (push (make-cart :x x :y y :dir sq) carts)
                  (setf (aref *grid* y x) maybe-track)))))
    (setf *carts* carts)
    carts))

(defun reset-exp ()
  (load-input-grid-for-exp)
  (collect-carts))

(defun reset-test ()
  (load-input-grid-for-test)
  (collect-carts))

(defun reset ()
  (load-input-grid)
  (collect-carts))

(defun cart-order-p (c1 c2)
  (or (< (cart-y c1) (cart-y c2))
      (< (cart-x c1) (cart-x c2))))
      
(defun tick-once ()
  (let (crashes
        moved)
    (setf *carts* (sort *carts* #'cart-order-p))
    (loop :with unmoved = *carts*
          :while (and (null crashes) unmoved)
          :for moved-cart = (move-cart (car unmoved))
          :do (setf crashes (crash-check-among moved unmoved))
          :do (if crashes (return crashes))
          :do (push moved-cart moved)
          :do (setf unmoved (cdr unmoved)))
    (setf *carts* moved)
    crashes))
            
(defun crash-check ()
  (loop :with crashes = ()
        :with ht = (make-hash-table :test 'equal)
        :for cart in *carts*
        :for x = (cart-x cart)
        :for y = (cart-y cart)
        :for p = (cons y x)
        :for old-occupant = (gethash p ht)
        :do (if old-occupant
                (push `(,p ,cart ,old-occupant) crashes)
                (setf (gethash p ht) cart))
        :finally (return crashes)))

(defun crash-check-among (&rest whatever)
  (loop :with crashes = ()
        :with ht = (make-hash-table :test 'equal)
        :for cart in (apply #'append whatever)
        :for x = (cart-x cart)
        :for y = (cart-y cart)
        :for p = (cons y x)
        :for old-occupant = (gethash p ht)
        :do (if old-occupant
                (push `(,p ,cart ,old-occupant) crashes)
                (setf (gethash p ht) cart))
        :finally (return crashes)))

(defun tick-until-crash ()
  (loop :for i :from 0
        :for crashes = (tick-once)
        :while (null crashes)
        :do (format t "~%~%iteration ok: ~a~%" i)
        :do (print-grid-with-carts)
        :finally (return (list i crashes))))
            
(defun print-this-grid (grid)
  (loop :for y :from 0 :below (array-dimension grid 0) :do
    (loop :for x :from 0 :below (array-dimension grid 1)
          :do (princ (aref grid y x))
          :finally (terpri))))

(defun print-grid-with-carts ()
  (print-this-grid (put-carts-on-grid (copy-array *grid*))))

(defun put-carts-on-grid (g)
  (loop :for c :in *carts*
        :do (setf (aref g (cart-y c) (cart-x c)) (cart-dir c)))
  (loop :for crash in (crash-check)
        :do (setf (aref g (caar crash) (cdar crash)) #\X))
  g)

(defun tick-once-and-print ()
  (if (crash-check)
      "but there is a crash"
      (prog1 (tick-once)
             (print-grid-with-carts))))
