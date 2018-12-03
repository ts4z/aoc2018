
(defmacro do-or-nil (form)
  `(handler-case ,form
     (t () nil)))

(with-open-file (input "/Users/tjs/aoc/2.input")
  (let ((having-2 0)
        (having-3 0))
    (loop
      for line = (do-or-nil (read-line input))
      while line
      do (let ((h (make-hash-table)))
           (loop for i from 0 to (1- (length line))
                 do (incf (gethash (char line i) h 0)))
           (let (has-2 has-3)
             (loop for v being the hash-values in h using (hash-key k)
                   do (if (= v 2) (setf has-2 t))
                   do (if (= v 3) (setf has-3 t)))
             (if has-2 (incf having-2))
             (if has-3 (incf having-3)))))
    (format t "having-2 ~a having-3 ~a~%" having-2 having-3)
    (* having-2 having-3)))
