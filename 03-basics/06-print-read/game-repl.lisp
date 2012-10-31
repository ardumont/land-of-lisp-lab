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

;; this will permit us to read a string without any parenthesis or quote
(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (d)
                     (list 'quote d)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))



