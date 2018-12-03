(defun read-intish (stream) (handler-case (read stream) (t nil)))

(with-open-file (input "input")
  (let ((n 0))
    (loop for x = (read-intish input)
          while x
          do (incf n x))
    n))
