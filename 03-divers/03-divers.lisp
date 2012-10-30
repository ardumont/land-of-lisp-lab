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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; if

(if '()
    'this-is-true
  'this-is-false-or-the-list-is-empty)
;; 'this-is-false-or-the-list-is-empty

(if '(1)
    'this-is-true
  'this-is-false-or-the-list-is-empty)
;; 'this-is-true

(if 'some-thing
    'this-is-true
  'this-is-false)
;; 'this-is-true

(if (oddp 5)
    'odd
  'even)
;; 'odd

(if (oddp 4)
    'odd
  'even)
;; 'even

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cond

(defun who-is-this
  (person)
  (cond ((eq person 'chloe) '(this is my daughter))
        ((eq person 'theo) '(this is my son))
        ((eq person 'chris) '(this is my wife))
        (t '(this is some stranger))))

(who-is-this 'theo)
;; '(this is my son)
(who-is-this 'chloe)
;; '(this is my daughter)
(who-is-this 'chris)
;; '(this is my wife)
(who-is-this 'someone)
;; '(this is some stranger)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; case

(defun who-is-this-person
  (person)
  (case person
    ((chloe)  '(this is my daughter))
    ((theo)   '(this is my son))
    ((chris)  '(this is my wife))
    (otherwise '(this is some stranger))))

(who-is-this-person 'theo)
;; '(this is my son)
(who-is-this-person 'chloe)
;; '(this is my daughter)
(who-is-this-person 'chris)
;; '(this is my wife)
(who-is-this-person 'someone)
;; '(this is some stranger)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; conditions

(and (oddp 4) (oddp 5) (oddp 6))
;; nil

(and (oddp 3) (oddp 5) (oddp 9))
;; T

(or (oddp 4) (oddp 5) (oddp 6))
;; T

(or (oddp 4) (oddp 6) (oddp 8))
;; nil

(defparameter *it-is-even* nil)
(and (oddp 4) (setf *it-is-even* t))
; nil
*it-is-even*
;; nil

(or (oddp 4) (setf *it-is-even* t))
;; t
*it-is-even*
;; t
