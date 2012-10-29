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


