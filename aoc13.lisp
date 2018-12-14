
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
  (dir nil :type character)
  (y nil :type integer)
  (x nil :type integer)
  (nt :left :type symbol)
  (crashed-p nil :type boolean))

(define-condition cant-go-that-way (error)
  ((cart :initarg :cart :reader cart)
   (track :initarg :track :reader track))
  (:report (lambda (c s)
             (format s "can't go that way (cart ~a on track '~a')~&"
                     (cart c) (track c)))))

;; next nt value
(defun nnt (nt)
  (switch (nt)
    (:left     :straight)
    (:straight :right)
    (:right    :left)))

(defvar *location-to-cart*)             ; hash table

(defun cart-loc (c)                     ; key for that hash table
  (cons (cart-y c) (cart-x c)))

(defmacro cart-at (at)                  ; get cart at given location
  (check-type at cons)
  `(gethash ,at *location-to-cart*))

(defun cart-set (c &key nt dir x y)
  (when (not (cart-crashed-p c))

    (format t "cart ~a~%" c)
    (format t "nt=~a dir=~a x=~a y=~a~%" nt dir x y)

    (when (or x y)
      (remhash (cart-loc c) *location-to-cart*))

    (when nt
      (setf (cart-nt c) nt))
    (when dir
      (setf (cart-dir c) dir))
    (when y
      (setf (cart-y c) y))
    (when x
      (setf (cart-x c) x))

    (when (or x y)
      (let ((other-cart (cart-at (cart-loc c))))
        (if other-cart
            (progn
              (setf (cart-crashed-p c) t)
              (setf (cart-crashed-p other-cart) t)
              (remhash (cart-loc c) *location-to-cart*)
              (format t "crashed carts: ~a~%" (list c other-cart))
              (list c other-cart))        ; crashed carts
            (progn
              (setf (cart-at (cart-loc c)) c)
              nil))))))                   ; no cart crashed

(defun move-cart (c)
  (if (cart-crashed-p c)
      (assert (not (equal (cart-at (cart-loc c)) c))) ; we should be removed if we're crashed
      (assert (eq c (cart-at (cart-loc c))))) ; our object should be in the index
                          
  (let* ((dir (cart-dir c))
         (x   (cart-x   c))
         (y   (cart-y   c))
         (nt  (cart-nt  c))
         (tk  (aref *grid* y x)))
    (cond
      ((eql tk #\+)
       (switch (dir)
         (#\< (switch (nt)
                (:left     (cart-set c :nt (nnt nt) :dir #\v :y (1+ y)))
                (:straight (cart-set c :nt (nnt nt) :dir dir :x (1- x)))
                (:right    (cart-set c :nt (nnt nt) :dir #\^ :y (1- y)))))
         (#\> (switch (nt)
                (:left     (cart-set c :nt (nnt nt) :dir #\^ :y (1- y)))
                (:straight (cart-set c :nt (nnt nt) :dir dir :x (1+ x)))
                (:right    (cart-set c :nt (nnt nt) :dir #\v :y (1+ y)))))
         (#\^ (switch (nt)
                (:left     (cart-set c :nt (nnt nt) :dir #\< :x (1- x)))
                (:straight (cart-set c :nt (nnt nt) :dir dir :y (1- y)))
                (:right    (cart-set c :nt (nnt nt) :dir #\> :x (1+ x)))))
         (#\v (switch (nt)
                (:left     (cart-set c :nt (nnt nt) :dir #\> :x (1+ x)))
                (:straight (cart-set c :nt (nnt nt) :dir dir :y (1+ y)))
                (:right    (cart-set c :nt (nnt nt) :dir #\< :x (1- x)))))))
       
      ((and (eql dir #\<) (eql tk #\-)) (cart-set c          :x (1- x)))
      ((and (eql dir #\<) (eql tk #\/)) (cart-set c :dir #\v :y (1+ y)))
      ((and (eql dir #\<) (eql tk #\\)) (cart-set c :dir #\^ :y (1- y)))
      ((and (eql dir #\>) (eql tk #\-)) (cart-set c          :x (1+ x)))
      ((and (eql dir #\>) (eql tk #\/)) (cart-set c :dir #\^ :y (1- y)))
      ((and (eql dir #\>) (eql tk #\\)) (cart-set c :dir #\v :y (1+ y)))
      ((and (eql dir #\^) (eql tk #\/)) (cart-set c :dir #\> :x (1+ x)))
      ((and (eql dir #\^) (eql tk #\\)) (cart-set c :dir #\< :x (1- x)))
      ((and (eql dir #\^) (eql tk #\|)) (cart-set c           :y (1- y)))
      ((and (eql dir #\v) (eql tk #\/)) (cart-set c :dir #\< :x (1- x)))
      ((and (eql dir #\v) (eql tk #\\)) (cart-set c :dir #\> :x (1+ x)))
      ((and (eql dir #\v) (eql tk #\|)) (cart-set c          :y (1+ y)))

      (t (error 'cant-go-that-way :cart c :track tk)))))

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
    (setf *location-to-cart* (make-hash-table :test 'equal))
    (mapcar (lambda (c)
              (setf (cart-at (cart-loc c)) c))
            *carts*)))

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
  (let ((dy (- (cart-y c1) (cart-y c2))))
    (cond ((< dy 0) t)
          ((= dy 0) (< (cart-x c1) (cart-x c2)))
          (t nil))))

(defun assert-cart-<-and-not-> (c1 c2)
  (assert (cart-order-p c1 c2))
  (assert (not (cart-order-p c2 c1))))

(defun test-cart-order-p ()
  (flet ((assert-cart-<-and-not-> (c1 c2)
           (assert (cart-order-p c1 c2))
           (assert (not (cart-order-p c2 c1)))))
    (assert-cart-<-and-not-> (make-cart :x 2 :y 2 :dir #\^) (make-cart :x 1 :y 3 :dir #\^) )
    (assert-cart-<-and-not-> (make-cart :x 2 :y 3 :dir #\^) (make-cart :x 3 :y 3 :dir #\^) )))

(defun same-location-p (c1 c2)
  (and (eql (cart-x c1) (cart-x c2))
       (eql (cart-y c1) (cart-y c2))))

(defun running-carts ()
  (sort (hash-table-values *location-to-cart*)  #'cart-order-p))

(defun tick-once ()
  (loop :for cart :in (running-carts)
        ;; :do (format t "move cart: ~a~%" cart)
        :appending (move-cart cart)))

(defun tick-until-crash ()
  (loop :for i :from 0 :below 1000
        :for crashes = (tick-once)
        :while (null crashes)
        ;; :do (print-grid-with-carts)
        :finally (return (list i crashes))))

(defun tick-until-one-left ()
  (loop :for i :from 1
        :while (> (length (running-carts)) 1)
        :for crashes = (tick-once)
        ;; :do (print-grid-with-carts)
        :finally (return (list i (running-carts)))))

(defun print-this-grid (grid)
  (loop :for y :from 0 :below (array-dimension grid 0) :do
    (loop :for x :from 0 :below (array-dimension grid 1)
          :do (princ (aref grid y x))
          :finally (terpri))))

(defun print-grid-with-carts ()
  (print-this-grid (put-carts-on-grid (copy-array *grid*))))

(defun put-carts-on-grid (g)
  (loop :for c :in (running-carts)
        :do (setf (aref g (cart-y c) (cart-x c)) (cart-dir c)))
  g)

(defun tick-once-and-print ()
  (prog1 (tick-once)
    (print-grid-with-carts)))
