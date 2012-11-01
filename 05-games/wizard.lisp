;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NODES

;; association list which represents the possible places of the game
(defparameter *nodes*
  '((living-room (you are in the living-room. a wizard is snoring on the couch.))
    (garden      (you are in a beautiful garden. there is a well in front of you.))
    (attic       (you are in the attic. there is a giant welding torch in the corner.))))

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
  '((living-room (garden west door)
                 (attic upstairs ladder))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; describe-paths

(mapcar #'describe-path '((garden west door) (attic upstairs ladder)))
;; '((there is a door going west from here.) (there is a ladder going upstairs from here.))

(cdr (assoc 'living-room *edges*))
;; '((garden west door) (attic upstairs ladder))

(mapcar #'describe-path (cdr (assoc 'living-room *edges*)))
;; '((there is a door going west from here.) (there is a ladder going upstairs from here.))

(apply #'append (mapcar #'describe-path (cdr (assoc 'living-room *edges*))))
;; '(there is a door going west from here. there is a ladder going upstairs from here.)

(defun describe-paths (location edges)
  (apply #'append
         (mapcar #'describe-path (cdr (assoc location edges)))))

(describe-paths 'living-room *edges*)
;; '(there is a door going west from here. there is a ladder going upstairs from here.)

(describe-paths 'garden *edges*)
;; '(there is a door going east from here)
(describe-paths 'attic *edges*)
;; '(there is a ladder going downstairs from here)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; objects-at

;; all the objects of the game
(defparameter *objects* '(whiskey bucket frog chain))

;; the object present in a location
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (frog garden)
                                   (chain garden)))

;; given a location, list the objects present in it
(defun objects-at (loc objects object-locations)
  (flet ((eq-loc-p (object-loc)
                   (eq loc (cadr object-loc))))
    (mapcar #'car (remove-if-not #'eq-loc-p *object-locations*))))

(objects-at 'living-room *objects* *object-locations*)
;; '(whiskey bucket)

(objects-at 'garden *objects* *object-locations*)
;; '(frog chain)

(objects-at 'attic *objects* *object-locations*)
;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; describe-objects

;; goal: describe the objects visible at a given location

(defun describe-objects (loc objs obj-loc)
  (flet ((describe-obj (obj)
                         `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(describe-objects 'living-room *objects* *object-locations*)
;; '(you see a whiskey on the floor. you see a bucket on the floor.)

(describe-objects 'attic *objects* *object-locations*)
;; nil

(describe-objects 'garden *objects* *object-locations*)
;; '(you see a frog on the floor. you see a chain on the floor.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; look

;; goal: one function to describe it all

;; the current location of the player
(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(look)
(setf *location* 'attic)
(look)
(setf *location* 'living-room)
(look)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; walk

;; goal: walking in a direction through a real passage
;; side-effect: modify the current *location* variable

(defun walk (direction)
  (let ((next (find direction (cdr (assoc *location* *edges*)) :key #'cadr)))
    ;; direction ok, can we go there?
    (if next
        ;; all is ok, the player can go
        (progn (setf *location* (car next))
               (look))
      '(this direction is not possible from here.))))

(walk 'no-where)
;; '(this direction is not possible from here.)

(setf *location* 'attic)
(walk 'upstairs)
;; '(this direction is not possible from here.)

(setf *location* 'living-room)
(walk 'upstairs)
;; (you are in the attic. there is a giant welding torch in the corner. there is a ladder going downstairs from here.)
*location*
;; 'attic
(walk 'downstairs)
 ;; (you are in the living-room. a wizard is snoring loudly on the couch. there is a door going west from here. there is a ladder going upstairs from here. you see a
 ;; whiskey on the floor. you see a bucket on the floor.)
*location*
;; 'living-room
(walk 'west)
;; '(you are in a beautiful garden. there is a well in front of you. there is a door going east from here. you see a frog on the floor. you see a chain on the floor.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pickup

;; goal: pickup an object from a location

(member 'whiskey (objects-at 'living-room *objects* *object-locations*))

(defun pickup (obj)
  (cond ((member obj (objects-at *location* *objects* *object-locations*))
           ;; ok, this object is present at the player's current location
           (push (list obj 'body) *object-locations*)
           `(,obj was picked up.))
         (t `(you cannot pickup that.))))

*location*
(pickup 'whiskey)
(walk 'east)
(pickup 'whiskey)
(walk 'west)
(pickup 'frog)
(pickup 'something)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; inventory

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(inventory)
