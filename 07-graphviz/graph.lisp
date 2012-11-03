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
(defun dot-name (name)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string name)))

(dot-name 'this_is_some_test!)
(dot-name 'this-is-some_test!)
;; "THIS_IS_SOME_TEST_"
