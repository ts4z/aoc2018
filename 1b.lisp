
(defun make-producer (filename)
  (let (stream)
    (labels
        ((reopen () (setf stream (open filename)))
         (close-it ()
           (close stream)
           (setf stream nil))
         (read-one ()
           (handler-case (read stream)
             (t () (progn (close-it)
                          (reopen)
                          (read-one))))))
      (reopen)
      (values (lambda () (read-one))
              (lambda () (close-it))))))
                    

(let ((sum 0)
      (freqs (make-hash-table)))
  (multiple-value-bind (producer closer)
      (make-producer "input")
    (prog1
        (loop for x = (funcall producer) do
          (format t "read ~a~%" x)
          (incf sum x)
          (when (gethash sum freqs)
            (return sum))
          (setf (gethash sum freqs) 1))
      (funcall closer))))
