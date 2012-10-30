;; games

(defparameter *nodes* '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden. there is a well in front of you.))
                        (attic (you are in the attic. there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(describe-location 'living-room *nodes*)

(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(describe-path '(garden west door))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(describe-paths 'living-room *edges*)

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

*nodes*
*edges*

(find 'living-room '(attic living-room garden))
;; living-room
(find 'test '(attic living-room garden))
;; nil

(find 'living-room (mapcar 'car *nodes*))
;; living-room
(find 'test (mapcar 'car *nodes*))
;; nil

(describe-paths *location* *edges*)
(find 'garden (mapcar #'car (cdr (assoc *location* *edges*))))
(find 'attic (mapcar #'car (cdr (assoc *location* *edges*))))

(defun walk (direction)
  (if (find direction (mapcar 'car *nodes*))
      ;; direction ok, can we go there?
      (if (find direction (mapcar #'car (cdr (assoc *location* *edges*))))
          ;; all is ok, the player can go
          (progn (setf *location* direction)
                 (look))
          '(this direction is not possible from here.))
      '(this direction is unknown.)))

(walk 'no-where)
;; '(this direction is unknown)

(setf *location* 'attic)
(walk 'garden)
;; '(this direction is not possible from here.)

(setf *location* 'living-room)
(walk 'attic)
;; (you are in the attic. there is a giant welding torch in the corner. there is a ladder going downstairs from here.)
*location*
;; 'attic
(walk 'living-room)
 ;; (you are in the living-room. a wizard is snoring loudly on the couch. there is a door going west from here. there is a ladder going upstairs from here. you see a
 ;; whiskey on the floor. you see a bucket on the floor.)
*location*
;; 'living-room
(walk 'garden)
;; '(you are in a beautiful garden. there is a well in front of you. there is a door going east from here. you see a frog on the floor. you see a chain on the floor.)
