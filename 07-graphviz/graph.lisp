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

;; print the graphviz label
(defun glabel (name)
  (concatenate 'string "[label=\"" name "\"];"))

;; compute the nodes of the graph
(defun nodes->dot (nodes)
  (mapc
   (lambda (node)
     (fresh-line)
     (princ (dot-name (car node)))
     (princ (glabel (dot-label (cadr node)))))
   nodes))

(nodes->dot *nodes*)
;; prints
;; LIVING_ROOM[label="(YOU ARE IN THE LIVING-ROOM..."];
;; GARDEN[label="(YOU ARE IN A BEAUTIFUL GAR..."];
;; ATTIC[label="(YOU ARE IN THE ATTIC. THER..."];

;; compute the edges of the graph
(defun edges->dot (edges)
  (mapc
   (lambda (edge)
     (let ((node-src (car edge)))
       (mapc
        (lambda (dest)
          (fresh-line)
          (princ (dot-name node-src))
          (princ "->")
          (princ (dot-name (car dest)))
          (princ (glabel (dot-label (cdr dest)))))
        (cdr edge))))
   edges))

(edges->dot *edges*)
;; prints
;; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
;; LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
;; GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
;; ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];
