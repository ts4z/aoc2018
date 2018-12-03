
(defmacro do-or-nil (form)
  `(handler-case ,form
     (t () nil)))

(defun snarf-file (name)
  (with-open-file (input name)
    (loop for line = (do-or-nil (read-line input))
          while line
          collect line)))

(defun process-file (filename)
  (process-lines (snarf-file filename)))

(defun process-pair (s1 s2 found-it)
  (let ((diff 0))
  (loop :for i :from 0 :to (1- (length s1))
        :do (when (not (eq (char s1 i) (char s2 i)))
              (incf diff))
        :while (<= diff 2))
    (when (= diff 1)
      (funcall found-it s1 s2))))
  

(defun process-lines (lines)
  (do ((p lines (cdr p)))
      ((null p))
    (let ((s1 (car p))
          (q (cdr p)))
      (dolist (s2 q)
        (process-pair s1 s2
                      (lambda (s1 s2)
                        (format t "CLOSE: ~a ~a~%" s1 s2)))))))
