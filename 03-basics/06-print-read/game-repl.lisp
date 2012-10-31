(defun game-repl ()
  (progn
    (princ "wizard> ")
    (let ((cmd (read)))
      (unless (eq cmd 'quit)
        (print (eval cmd))
        (game-repl)))))
