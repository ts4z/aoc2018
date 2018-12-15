(ql:quickload "alexandria")
(ql:quickload "queues.simple-queue")

(defpackage :aoc15
  (:use :common-lisp :alexandria :q))

(in-package :aoc15)

(defvar +exp-input-file+ #P"/home/tjs/git/aoc2018/15-exp.input")
(defvar +example-input-file+ #P"/home/tjs/git/aoc2018/15-example.input")
(defvar +input-file+ #P"/home/tjs/git/aoc2018/15.input")

(defvar *grid*)
(defvar *units-by-loc*)
(defvar *completed-rounds*)
(defvar *dead-elf* 0)
(defvar *elf-ap* 3)
(defvar *goblin-ap* 3)

(defstruct loc
  (y nil :type integer :read-only t)
  (x nil :type integer :read-only t))

(defstruct unit
  (id nil :type integer)
  (type nil :type character)
  (hp 200 :type integer)
  (ap 3 :type integer)
  (loc nil :type loc))

(defun unit-alive-p (u)
  (check-type u unit)
  (> (unit-hp u) 0))

(defun unit-dead-p (u)
  (not (unit-alive-p u)))

(defmacro loc (y x)                     ;remove this
  `(make-loc :y ,y :x ,x))

(defmacro at-location (loc)
  `(gethash ,loc *units-by-loc*))

(defun scan-by-line-from-stream (file)
  (format t "filename ~a~%" file)
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

(defmacro gridref (ll)
  `(aref *grid* (loc-y ,ll) (loc-x ,ll)))


(defun attackable-location-p (enemy cyx)
  (check-type cyx loc)
  (and (< -1 (loc-y cyx) (array-dimension *grid* 0))
       (< -1 (loc-x cyx) (array-dimension *grid* 1))
       (eql enemy (gridref cyx))
       cyx))

(defun valid-move-p (cyx)
  (check-type cyx loc)
  (if (and (< -1 (loc-y cyx) (array-dimension *grid* 0))
           (< -1 (loc-x cyx) (array-dimension *grid* 1))
           (eql #\. (gridref cyx)))
      cyx))

(defparameter *input-files* (alist-hash-table `((nil . ,+input-file+)
                                                (:example . ,+example-input-file+)
                                                (:exp . ,+exp-input-file+))))


(defun units ()
  (hash-table-values *units-by-loc*))

(defun collect-units ()
  (flet ((collect-unit (ch y x ap)
           (setf (at-location (make-loc :y y :x x))
                 (make-unit :id (1+ (hash-table-count *units-by-loc*))
                            :ap ap
                            :type ch :loc (make-loc :y y :x x)))))
    (loop :for y :from 0 :below (array-dimension *grid* 0) :do
      (loop :for x :from 0 :below (array-dimension *grid* 1) :do
        (switch ((gridref (make-loc :y y :x x)))
          (#\E (collect-unit #\E y x *elf-ap*))
          (#\G (collect-unit #\G y x *goblin-ap*)))))))

(defun reset (&optional file elf-ap)
  (setf *elf-ap* (or elf-ap 3))
  (setf *dead-elf* 0)
  (setf *completed-rounds* 0)
  (setf *units-by-loc* (make-hash-table :test 'equalp))
  (setf *grid* (scan-by-line-from-stream (gethash file *input-files*)))
  (collect-units)
  (print-grid))

(defun loc-order-p (c1 c2)
  (check-type c1 loc)
  (check-type c2 loc)
  (let ((dy (- (loc-y c1) (loc-y c2))))
    (cond ((< dy 0) t)
          ((= dy 0) (< (loc-x c1) (loc-x c2)))
          (t nil))))

(defun loc-order-p-test ()
  (flet ((c1-then-c2 (c1 c2)
           (assert (loc-order-p c1 c2))
           (assert (not (loc-order-p c2 c1)))))
    (c1-then-c2 (make-loc :y 2 :x 2) (make-loc :y 2 :x 3))
    (c1-then-c2 (make-loc :y 2 :x 3) (make-loc :y 3 :x 3))))

(defun unit-test ()
  (loc-order-p-test))

(defun print-grid ()
  (let ((grid *grid*))
    (loop :for y :from 0 :below (array-dimension grid 0) :do
      (loop :for x :from 0 :below (array-dimension grid 1)
            :do (princ (aref grid y x))
            :finally (terpri)))))

(defun units-in-turn-order ()
  (sort (hash-table-values *units-by-loc*) #'loc-order-p :key #'unit-loc))

(defun clear-grid-at (cyx)
  (check-type cyx loc)
  ;; there is a unit, and its type is the same as the grid
  (assert (eql (unit-type (at-location cyx)) (gridref cyx)))
  (remhash cyx *units-by-loc*)
  (setf (gridref cyx) #\.))

(defun unit-move (u to)
  (let ((from (unit-loc u)))
    (check-type to loc)
    (assert (equal (unit-type u) (gridref (unit-loc u))))
    (assert (eql #\. (gridref to)))

    (format t "  nc ~a ~a moves to ~a~%" (unit-id u) (unit-type u) to)

    (clear-grid-at (unit-loc u))
    (setf (at-location to) u)
    (setf (gridref to) (unit-type u))
    (setf (unit-loc u) to)

    (assert (eql (gridref from) #\.))
    (assert (eql (gridref to) (unit-type u))))
  t)

(defun above (loc)    (make-loc :y (1- (loc-y loc)) :x (loc-x loc)))
(defun below (loc)    (make-loc :y (1+ (loc-y loc)) :x (loc-x loc)))
(defun left-of (loc)  (make-loc :y (loc-y loc) :x (1- (loc-x loc))))
(defun right-of (loc) (make-loc :y (loc-y loc) :x (1+ (loc-x loc))))

(defun unit-enemy (u)
  (switch ((unit-type u))
    (#\E #\G)
    (#\G #\E)))

;; locations returned in "reading order"
(defun attackable-locations-adjacent-to (enemy-type loc)
  (remove-if #'null
             (mapcar
              (lambda (fn)
                (attackable-location-p enemy-type (funcall fn loc)))
              (list #'above #'left-of #'right-of #'below))))

;; locations returned in "reading order"
(defun movable-locations-adjacent-to (loc)
  (remove-if #'null
             (mapcar
              (lambda (fn)
                (valid-move-p (funcall fn loc)))
              (list #'above #'left-of #'right-of #'below))))

(defun contains-enemy (loc enemy)
  (if (eql (gridref loc) enemy)
      loc
      nil))

(defun attackable-unit (u)
  (check-type u unit)
  (let ((locations (attackable-locations-adjacent-to (unit-enemy u) (unit-loc u))))
    (car (stable-sort (mapcar (lambda (loc) (at-location loc)) locations)
                      #'< :key #'unit-hp))))

(defun unit-remove (u)
  (clear-grid-at (unit-loc u)))

(defun unit-attack (perp vic)
  (check-type perp unit)
  (format t "perp ~a ~a (~a) attacks ~a (~a)~%"
          (unit-id perp) (unit-type perp) (unit-loc perp) (unit-loc vic) (unit-hp vic))
  (decf (unit-hp vic) (unit-ap perp))
  (when (unit-dead-p vic)
    (format t "~a killed ~a~%" perp vic)
    (if (eql (unit-type vic) #\E)
        (incf *dead-elf*))
    (unit-remove vic))
  t)

(defun maybe-attack (u)
  (let ((victim (attackable-unit u)))
    (when victim
      (unit-attack u victim))))

(defun take-turn (u)
  ;; if there is no one to kill, return t and don't count this round.
  
  
  (or (= 0 (count-if (lambda (maybe-enemy)
                       (eql (unit-type maybe-enemy) (unit-enemy u)))
                     (units)))          ; Nobody left to kill
      (and (or (maybe-attack u)
               (and (let ((next-step-loc (pick-next-step u)))
                      (if (null next-step-loc)
                          (format t "unit stuck ~a~%" u)
                          (progn (unit-move u next-step-loc)
                                 (maybe-attack u))))))
           nil)))                  ; I took my whole turn

(defun tick ()
  (loop :with short-round-p = nil
        :for u in (units-in-turn-order) :do
          (when (unit-alive-p u)
            (when (take-turn u)
              (format t "short round~%")
              (setf short-round-p t)))
        :finally (when (not short-round-p)
                   (incf *completed-rounds*))))

(defun tick-print ()
  (if (tick)
      (format t "SHORT ROUND~%")
      (format t "round ~a done~%" *completed-rounds*))
  (print (units-in-turn-order))
  (terpri)
  (print-grid))
  

(defun goblin-p (u)
  (eql (unit-type u) #\G))

(defun elf-p (u)
  (eql (unit-type u) #\E))

(defun count-goblins ()
  (length (remove-if-not #'goblin-p (units))))

(defun count-elves ()
  (length (remove-if-not #'elf-p (units))))

(defun total-hp ()
  (apply #'+ (mapcar #'unit-hp (units))))

(defun outcome ()
  (* *completed-rounds* (total-hp)))

(defun tick-until-outcome ()
  (loop :until (or (= 0 (count-goblins))
                   (= 0 (count-elves)))
        :do (tick)
        :finally (progn
                   (format t "Combat ends after ~a full rounds~%" *completed-rounds*)
                   (format t "dead elves ~a~%" *dead-elf*)
                   (format t "outcome ~a~%" (outcome)))))

(defun pick-next-step (u)
  (let* ((enemy-type     (unit-enemy u))
         (seen-locs      (make-hash-table :test 'equalp))
         (paths-to-score (make-queue :simple-queue)))

    (macrolet ((in-hash (loc) `(gethash ,loc seen-locs)))
      (flet ((adjacent-enemy-in-p (loc) (attackable-locations-adjacent-to enemy-type loc))
             (peek-q ()    (qtop paths-to-score))
             (pop-q  ()    (qpop paths-to-score))
             (push-q (loc) (qpush paths-to-score loc)))
      
        ;; init: map the locations to themselves
        (mapc (lambda (loc)
                (push-q (list loc loc)))
              (movable-locations-adjacent-to (unit-loc u)))

        (loop :with answer = nil
              :while (and (null answer) (peek-q))
              :for loc-and-breadcrumb = (pop-q)
              :do (destructuring-bind (loc breadcrumb) loc-and-breadcrumb
                    (if (adjacent-enemy-in-p loc)
                        (setf answer breadcrumb) ; done
                        (mapc (lambda (possible)
                                (when (not (in-hash possible))
                                  (setf (in-hash possible) t)
                                  ;; map new loc back to breadcrumb;
                                  ;; we only care about the first step
                                  (push-q (list possible breadcrumb))))
                              (movable-locations-adjacent-to loc))))
              :finally (return answer))))))
