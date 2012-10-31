(defun game-repl ()
  (progn
    (princ "wizard> ")
    (let ((cmd (read)))
      (print (eval cmd))
      (game-repl))))
