(substitute-if -1 #'oddp '(0 1 2 3 4 5))
;; '(0 -1 2 -1 4 -1)

(substitute-if -1 #'evenp '(0 1 2 3 4 5))
;; '(-1 1 -1 3 -1 5)

(substitute-if -1 (complement #'evenp) '(0 1 2 3 4 5))
;; '(0 -1 2 -1 4 -1)

(alphanumericp #\a)
(alphanumericp #\1)
;; t

(alphanumericp #\_)
;; nil

(prin1-to-string "some-name")

;; compute a graphviz's dot format compliant name
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(dot-name 'this_is_some_test!)
(dot-name 'this-is-some_test!)
;; "THIS_IS_SOME_TEST_"

(defparameter *max-length* 30)

(write-to-string 'this-is-way-too-much-in-regards-of-the-30-characters-limit
                 :pretty nil)

;; compute the label (and limit to 30 characters)
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (< *max-length* (length s))
            ;; +
            (concatenate 'string (subseq s 0 (- *max-length* 3)) "...")
          s))
    ""))

(dot-label nil)
;; ""

(dot-label 'this-is-ok)
;; "THIS-IS-OK"

(dot-label 'this-is-way-too-much-in-regards-of-the-30-characters-limit)
;; "THIS-IS-WAY-TOO-MUCH-IN-REG..."


