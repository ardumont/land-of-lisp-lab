;; There is a common 'construction' onto which lisp can work seemlessly
;; the sequence

;; for example, length can work on sequence

(length '(1 2 3))
;; 3

(length (make-array 3 :initial-contents '(1 2 3)))
;; 3

;; reduce
(defun my-sum (l) (reduce #'+ l))

(my-sum '(1 2 3))
;; 6

(my-sum (make-array 3 :initial-contents '(1 2 3)))
;; 6
