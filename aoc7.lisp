(defpackage :aoc7
  (:use :common-lisp :cl-ppcre :ioutil))

(defvar +input-file+ #P"/home/tjs/git/aoc2018/7.input")

(defvar *steps*)

(defun read-steps () (read-steps-from +input-file+))

(defun read-steps-from (file)
  (let* ((pattern "Step (.) must be finished before step (.) can begin.")
         (scanner (cl-ppcre:create-scanner pattern)))
    (loop
      :for line in (ioutil:snarf-file file)
      :collect (ppcre:register-groups-bind (a b)
                   (scanner line)
                 (cons a b)))))

(defun hash-keys (h)
  (loop :for k :being :the :hash-keys :in h :collect k))

(defun print-dag (dag)
  (maphash (lambda (k v)
             (format t "~a => ~a~%" k (hash-keys v)))
           dag))

(defun make-empty-dag ()
  (loop :with h = (make-hash-table :test 'equal)
        :for i :from (char-code #\A) :to (char-code #\Z) :do
          (setf (gethash (format nil "~a" (code-char i)) h)
                (make-hash-table :test 'equal))
        :finally (return h)))

(defun fill-dag (dag steps)
  (map 'nil (lambda (step)
              (let* ((to (car step))
                     (from (cdr step))
                     (h (gethash from dag)))
                (setf (gethash to h) t)))
       steps)
  dag)

(defun complete-step (outer step)
  (maphash (lambda (k inner)
             (declare (ignore k))
             (remhash step inner))
           outer)
  (remhash step outer)
  outer)

(defun one-ready-step (dag)
  (loop :with r = ()
        :for k :being :the :hash-keys :in dag :using (hash-value v) :do
          (when (= (hash-table-count v) 0)
            (push k r))
        :finally (return (car (sort r #'string<)))))

(defun steps-in-order (dag)
  (let ((next-step (one-ready-step dag)))
    (when next-step
      (cons next-step
            (steps-in-order (complete-step dag next-step))))))

(defun answer-one ()
  (read-steps)
  (apply #'concatenate 'string
         (steps-in-order (fill-dag (make-empty-dag) *steps*))))

(defun start-step (dag step)
  (remhash step dag))

(defun step-time (step)
  ;; assumes ASCII
  (- (char-code (aref step 0)) 4))

;; This could be better -- it is using a clock but it could just be using a PQ
;; or a sorted list.
(defun clock-watcher (steps)
  (let* ((dag (fill-dag (make-empty-dag) steps))
         (steps-to-do (hash-table-count dag))
         (steps-finishing-this-sec (make-array 1000 :initial-element nil))
         (max-time 3000)                ;sentry
         (avail-workers 5))
    (loop :for now :from 0 :below max-time
          :while (> steps-to-do 0)
          ;; :do (format t "now=~a~%" (- now 1))
          :do (progn
                (loop :for step :in (aref steps-finishing-this-sec now)
                      :do (complete-step dag step)
                      :do (decf steps-to-do)
                      :do (incf avail-workers)
                      ;; :do (format t "t=~a completing step ~a; ~a workers~%" now step avail-workers)
                      )
                (loop :while (and (> avail-workers 0)
                                  (one-ready-step dag))
                      :do
                         (let* ((step (one-ready-step dag))
                                (completes-at (+ (step-time step) now)))
                           ;; (format t "starting ~a (until ~a)~%" step completes-at)
                           (start-step dag step)
                           (push step (aref steps-finishing-this-sec completes-at))
                           (decf avail-workers))))
          :finally (return now))))

(defun answer-two ()
  (read-steps)
  (clock-watcher *steps*))
