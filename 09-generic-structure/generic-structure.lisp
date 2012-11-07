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

