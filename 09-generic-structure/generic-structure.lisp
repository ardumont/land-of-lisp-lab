;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; array

(make-array 3)
;; #(nil nil nil)

(defparameter a (make-array 3))

(aref a 1)
;; nil

;; set 10 to the position 1 of the array a
(setf (aref a 1) 10)
;; 10

;; Now a has been mutated (gloups!) in place and hold the new value at the position 1
a
;; #(nil 10 nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; setf. generic setter

;;;;;;;;;;;;;;;;;; also works with list

(defparameter foo '(:a :b :c))

foo
;; '(:a :b :c)

(setf (second foo) :z)
; :z

foo
;; '(:a :z :c)

;;;;;;;;;;;;;;;;;; also works with hashtable

(defparameter bar (make-hash-table))

bar
;; #S(HASH-TABLE :TEST FASTHASH-EQL)

(setf (gethash 'boink bar) 5)
;; 5

bar
;; #S(HASH-TABLE :TEST FASTHASH-EQL (BOINK . 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; difference between array and list

;; array
;; constant access time to an element
;; so the updating of an element is faster too

;; list
;; linear access time to an element
