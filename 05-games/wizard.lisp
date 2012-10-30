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


