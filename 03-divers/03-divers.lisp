;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; let

;; let form
(let ((a 10)
      (b 5))
  (+ a b))
;; 15

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flet

;; function let
(flet ((f (n) (+ n 10)))
  (f 5))
;; 15

(flet ((f (n) (+ n 10))
       (g (n) (+ n 5)))
  (g (f 5)))
;; 20

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; labels

;; to permit that g knows f inside the same let bloc
(labels ((f (n) (+ n 10))
         (g (n) (+ 5 (f n))))
  (g 5))
;; 20

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; princ

;; print the message "hello world!" and then return this message
(princ "hello world!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; code/data

(expt 2 3)
;; 8

'(expt 2 3)
;; (expt 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nil

nil
()
'()
;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cons

;; cons
(cons 'chicken '())
(cons 'chicken nil)
;; '(chicken)

'(1 2 3 4)
(cons 1 (cons 2 (cons 3 (cons 4 nil))))
;; '(1 2 3 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; car/cdr/cadr/cdar

(car '(this is a list))
;; 'this

(cdr '(this is a list))
;; '(is a list)

(car (cdr '(this is a list)))
;; 'is

(car (car '((this) is a list)))
;; 'this

(cdr (car '((this) is a list)))
;; nil

(cadr '(this is a list))
;; 'is <=> (comp car cdr)

(cdar '((this is a list)))
;; '(is a list) <=> (comp car cdr)

