(defun say-hello ()
  (print "Please, type your name:")
  (let ((name (read)))
    (princ "Nice to meet you, ")
    (princ name)))

(say-hello)
