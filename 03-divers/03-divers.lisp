;; let form
(let ((a 10)
      (b 5))
  (+ a b))
;; 15


;; function let
(flet ((f (n) (+ n 10)))
  (f 5))
;; 15


(flet ((f (n) (+ n 10))
       (g (n) (+ n 5)))
  (g (f 5)))
;; 20

;; to permit that g knows f inside the same let bloc
(labels ((f (n) (+ n 10))
         (g (n) (+ 5 (f n))))
  (g 5))
;; 20

;; print the message "hello world!" and then return this message
(princ "hello world!")

(expt 2 3)
;; 8

'(expt 2 3)
;; (expt 2 3)

nil
()
'()
;; nil

;; cons
(cons 'chicken '())
(cons 'chicken nil)
;; '(chicken)

'(1 2 3 4)
(cons 1 (cons 2 (cons 3 (cons 4 nil))))
;; '(1 2 3 4)
