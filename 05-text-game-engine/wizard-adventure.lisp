;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NODES

;; association list which represents the possible places of the game
(defparameter *nodes*
  '((living-room (you are in the living-room. a wizard is snoring on the couch))
    (garden      (you are in a beautiful garden. there is a well in front of you))
    (attic       (you are in the attic. there is a giant welding torch in the corner))))

;; retrieve the correct item regarding the 'garden entry
(assoc 'garden *nodes*)
;; '(garden (you are in a beautiful garden. there is a well in front of you))

;; to retrieve the description
(cadr (assoc 'garden *nodes*))
;; '(you are in a beautiful garden. there is a well in front of you)

;; Now we can implement the describe-location as follows
(defun describe-location (key alist)
  (cadr (assoc key alist)))

(describe-location 'garden *nodes*)
;; '(you are in a beautiful garden. there is a well in front of you)
(describe-location 'attic *nodes*)
;;  '(you are in the attic. there is a giant welding torch in the corner)
(describe-location 'living-room *nodes*)
;; '(you are in the living-room. a wizard is snoring on the couch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EDGES

;; the paths the player can take from one place (one node) to another (node)
(defparameter *edges*
  '((living-room ((garden west door)
                  (attic upstairs ladder)))
    (garden      (living-room east door))
    (attic       (living-room downstairs ladder))))

;; function to describe the possible path

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))
;; ` as avoiding the interpretation of list
;; , to prefix a list to be evaluated inside a `expression
;; this is common-lisp's quasiquoting
;; we can see the ` as a switch that turns on the data mode
;; and the , a local switch to on for the code mode (list interpretation)

(describe-path '(living-room downstairs ladder))
;; '(there is a ladder going downstairs from here.)

(describe-path '(garden west door))
;; '(there is a door going west from here.)


