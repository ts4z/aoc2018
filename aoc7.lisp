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

(defun step-time (step)
  ;; assumes ASCII
  (- (char-code (aref step 0)) 4))

;; This could be better -- it is using a clock but it could just be using a PQ
;; or a sorted list.
(defun clock-watcher (steps)
  (let* ((now 0)
         (dag (fill-dag (make-empty-dag) steps))
         (steps-to-do (hash-table-count dag))
         (sec-step-pairs ())
         (avail-workers 5))
    (loop :for i from 0 below 30
          :while (> steps-to-do 0)
          :do (progn
                ;; Start workers while ready work and available worker.
                (loop :while (and (> avail-workers 0) (one-ready-step dag))
                      :do
                         (let* ((step (one-ready-step dag))
                                (completes-at (+ (step-time step) now)))
                           ;; (format t "starting ~a (until ~a)~%" step completes-at)
                           (remhash step dag)
                           (push (cons completes-at step) sec-step-pairs)
                           (decf avail-workers)))

                ;; Re-sort to find next thing to finish.  Could do better here,
                ;; but with 5 workers, there can't be more than 5 items; a sort
                ;; is fine
                (setf sec-step-pairs (sort sec-step-pairs #'< :key #'car))

                ;; now, our next time to run will be the lowest time to consider
                (setf now (caar sec-step-pairs))

                (loop :while (and sec-step-pairs (= now (caar sec-step-pairs)))
                      :do (let* ((completing (pop sec-step-pairs))
                                 (step (cdr completing)))
                            (complete-step dag step)
                            (decf steps-to-do)
                            (incf avail-workers)
                            ))))
    now))

(defun answer-two ()
  (read-steps)
  (clock-watcher *steps*))
