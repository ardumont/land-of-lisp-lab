(defun game-repl ()
  (progn
    (princ "wizard> ")
    (print (eval (read)))
    (game-repl)))
