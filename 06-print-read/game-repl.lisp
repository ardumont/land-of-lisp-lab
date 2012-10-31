;; this will permit us to read a string without any parenthesis or quote
(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (d)
                     (list 'quote d)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *available-cmds* '(pickup look walk inventory quit))

(defun game-eval (cmd)
  (if (member (car cmd) *available-cmds*)
      (eval cmd)
      `(command ,cmd unknown!)))

(defun game-repl ()
  (progn
    (princ "wizard> ")
    (let ((cmd (game-read)))
      (unless (eq (car cmd) 'quit)
        (progn
          (print (game-eval cmd))
          (game-repl))))))
