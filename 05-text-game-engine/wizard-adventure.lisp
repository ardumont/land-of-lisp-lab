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
