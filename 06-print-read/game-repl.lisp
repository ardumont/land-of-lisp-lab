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
      `(command unknown!)))

(defun game-print (msg)
  (flet ((interpose (c l)
                    (cons (car l)
                          (reduce (lambda (a e) (cons c (cons e a)))
                                  (reverse (cdr l))
                                  :initial-value ())))
         (as-str (sym)
                 (string-downcase (symbol-name sym))))
    (apply #'concatenate 'string
           (interpose " " (mapcar #'as-str msg)))))

(game-print '(this message will display as is but as a string.))
;; this message will display as is but as a string.
(game-print '(this message will display as is but as a string!?))
;; this message will display as is but as a string!?

(defun game-repl ()
  (progn
    (princ "wizard> ")
    (let ((cmd (game-read)))
      (unless (eq (car cmd) 'quit)
        (progn
          (princ (game-print (game-eval cmd)))
          (fresh-line)
          (game-repl))))))
