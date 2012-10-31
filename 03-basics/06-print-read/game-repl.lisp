(defparameter *available-cmds* '(pickup look walk inventory quit))

(defun game-repl ()
  (progn
    (princ "wizard> ")
    (let ((cmd (read)))
      (unless (eq cmd 'quit)
        (if (member (car cmd) *available-cmds*)
            (progn
              (print (eval cmd))
              (game-repl))
          `(command ,cmd unknown!))))))
