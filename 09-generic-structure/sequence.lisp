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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; map

(map 'list (lambda (x) (if (eq #\c x) #\C x))
     "this is a sequence of char to 'capitalize' on c.")
;; (#\t #\h #\i #\s #\Space #\i #\s #\Space #\a #\Space #\s #\e #\q #\u #\e #\n #\C #\e #\Space #\o #\f #\Space #\C #\h #\a #\r #\Space #\t #\o #\Space #\' #\C #\a #\p #\i #\t #\a #\l #\i #\z #\e #\' #\Space #\o #\n #\Space #\C
#\.)

(map 'string (lambda (x) (if (eq #\c x) #\C x))
     "this is a sequence of char to 'capitalize' on c.")
;; "this is a sequenCe of Char to 'Capitalize' on C."

(map 'array (lambda (x) (if (eq #\c x) #\C x))
     "this is a sequence of char to 'capitalize' on c.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; subseq

(subseq "some-random-string" 5 11)
;; "random"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sort

(sort '(10 3 4 5 500 43) #'<)
;; '(3 4 5 10 43 500)

(sort '(10 3 4 5 500 43) #'>)
;; '(500 43 10 5 4 3)

