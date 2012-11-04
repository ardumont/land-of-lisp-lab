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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; member

(member 1 '(0 1 2 3))
;; '(1 2 3) -> this returns the tail of the list which starts with the searched elt

(member 4 '(0 1 2 3))
;; nil

(member nil '(0 1 nil 3))
;; '(nil 3)

(member nil '(0 1 nil))
;; '(nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; find-if

(flet ((eq1 (n) (eq 1 n)))
  (find-if #'eq1 '(0 1 2 3)))
;; 1

(find-if #'evenp '(-1 1 2 3 4 5 6))
;; 2 -> return the first element which validate the #'evenp predicate

(find-if #'null '(1 2 nil 3))
;; nil
(find-if #'null '(1 2 3))
;; nil -> no way to distinguish the use cases (annoying known limit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eq (symbol equality)

(eq 'theo 'chloe)
;; nil

(eq 'theo 'theo)
;; t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; equal (for everything else)

(equal 1 1)
;; t

(equal 1 2)
;; nil

(equal '(theo and chloe are my children) '(theo and chloe are my children))
;; t

(equal '(theo and chloe are my children) '(indeed they are))
;; nil

(equal "on-string-too" "on-string-too")
;; t

(equal "on-string-too" "on-string")
;; nil

(equal 5.5 5.5)
;; t

(equal 5.5 5.6)
;; nil

(equal #\a #\a)
;; t

(equal #\a #\b)
;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eql (same as eq + deal with characters and numbers)

(eql 1 1)
;; t

(eql 1 2)
;; nil

(eql 1.1 1.1)
;; t

(eql 1.1 1.2)
;; nil

(eql #\a #\a)
;; t

(eql #\a #\b)
;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mapcar

(mapcar #'sqrt '(1 3 4 10))
;; '(1 1.7320508 2 3.1622777)

(mapcar (function sqrt) '(1 3 4 10))
;; '(1 1.7320508 2 3.1622777)

;; #' is a macro that expands into the (function form)

(mapcar #'car '((10 20 30) (40 50) ))
;; '(10 40)

(apply #'append (mapcar #'cdr ((10 20 30) (40 50))))
;; '(20 30 50)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; append

(append '(1 3 2) '(:a :b))
;; '(1 3 2 :a :b)

(apply #'append '((these are) (lists which will) (be appended into) (one)))
;; (these are lists which will be appended into one)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; apply

(apply + '(1 2 3 4 5))
;; 15

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; find

(find 'y (mapcar #'cadr '((5 x) (3 y) (7 z))))
;; y

(find 'y '((5 x) (3 y) (7 z)) :key #'cadr)
;; '(3 y)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; assoc/push

(assoc :fruit '((:fruit :apple) (:fruit :banana)))
;; '(:fruit :apple) -> only the first element of the list is given

(defparameter *fruits*  '((:fruit :apple) (:fruit :banana)))

(push '(:fruit :tomatoe) *fruits*)
;;  '((:fruit :tomatoe) (:fruit :apple) (:fruit :banana))

*fruits*
;;  '((:fruit :tomatoe) (:fruit :apple) (:fruit :banana))
;; (push new-value list)  <=> (setf list (cons new-value list))

(assoc :fruit *fruits*)
;; '(:fruit :tomatoe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; print/princ/prin1

(print "this is a string")
;; "this is a string"
;; "this is a string"

(progn (print "this")
       (print "is a string"))
;; "this"
;; "is a string"
;; "is a string"

(progn (prin1 "this")
       (prin1 "is a string"))
;; "this""is a string"
;; "is a string"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; substitute

(substitute-if -1 #'oddp '(0 1 2 3 4 5))
;; '(0 -1 2 -1 4 -1)

(substitute-if -1 #'evenp '(0 1 2 3 4 5))
;; '(-1 1 -1 3 -1 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; complement

(substitute-if -1 (complement #'evenp) '(0 1 2 3 4 5))
;; '(0 -1 2 -1 4 -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; alphanumericp

(alphanumericp #\a)
(alphanumericp #\1)
;; t

(alphanumericp #\_)
;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mapcar/maplist

(mapcar #'print '(1 2 3))
;; prints
;; 1
;; 2
;; 3
;; (1 2 3)

(maplist #'print '(1 2 3))
;; prints
;; (1 2 3)
;; (2 3)
;; (3)
;; ((1 2 3) (2 3) (3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; loop

(loop for i in '(1 2 3) do (print i))
;; prints
;;
;; 1
;; 2
;; 3
;; NIL -> loop does not retain the head

(loop for i on '(1 2 3) do (print i))
;; prints
;;
;; (1 2 3)
;; (2 3)
;; (3)
;; NIL -> loop does not retain the head

(loop for i from 1 to 3 do (* i i))
;; NIL -> does the computation but does not return it

(loop for i from 1 to 3 collect (* i i))
;; '(1 4 9) -> to return the result, use collect

(loop for i from 1 to 3 collect (list i (* i i)))
;; '((1 1) (2 4) (3 9))

(loop for i from 1 to 3 append (list i (* i i)))
;; '(1 1 2 4 3 9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; let/let*

;; (let ((x 1)
;;       (y (+ x 1)))
;;   y)
;; does not work - the let can only bind variables without dependency between them

(let* ((x 1)
       (y (+ x 1)))
  y)
;; 2 ; use let* if dependencies between variables you want to declare
