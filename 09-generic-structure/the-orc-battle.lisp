(load "../00-util/functions-graph.lisp")

;; the orc battle game data functions
(defparameter
  *functions-orc-battle-edges* '((orc-battle (init-monster)
                                             (init-player)
                                             (game-loop)
                                             (player-dead)
                                             (monsters-dead))
                                 (game-loop (player-dead)
                                            (monsters-dead)
                                            (show-player)
                                            (show-monsters)
                                            (player-attack)
                                            (monster-dead)
                                            (monster-attack)
                                            (game-loop))
                                 (player-attack (monster-hit)
                                                (pick-monster)
                                                (monsters-dead)
                                                (random-monster)
                                                (randval))
                                 (random-monster (random-monster)
                                                 (monster-dead))
                                 (pick-monster (pick-monster)
                                               (monster-dead))
                                 (monster-dead (monster-health))
                                 (monsters-dead (monster-dead))
                                 (show-monsters (monster-dead)
                                                (monster-health)
                                                (monster-show))
                                 (monster-hit (monster-health)
                                              (monster-dead))
                                 (monster-show (orc-club-level))
                                 (monster-attack (randval)
                                                 (orc-club-level))))

(graph-functions "the-orc-battle.dot"  *functions-orc-battle-edges*)
(display-graph  "the-orc-battle.dot.png")

;; the existing monsters
(defparameter *monsters* nil)

;; the monsters builder (helper to build monsters)
(defparameter *monster-builders* nil)

;; the number of monsters
(defparameter *monster-num* 12)

;; Initialize the property of the player
(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30)
  '(*player-health* *player-agility* *player-strength*))

;;(init-player)

(defun player-dead-p ()
  (<= *player-health* 0))

;; (player-dead-p)

;; Display the player's metadata (health, agility and strength)
(defun show-player ()
  `(knight health ,*player-health*
           agility ,*player-agility*
           strength ,*player-strength*))

;; (show-player)

;; Compute a random between 1 and n
;; (its utility will be to ask for a random index in a list)
(defun randval (n)
  (1+ (random (max 1 n))))

;; (randval 10)

;; Is a monster m dead?
(defun monster-dead-p (m)
  (<= (monster-health m) 0))

;; Are all the monsters dead?
(defun monsters-dead-p ()
  (every #'monster-dead-p *monsters*))

;; Pick randomly an alive monster inside the *monsters* list
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*))))
        (unless (monster-dead-p m)
          m
          (random-monster)))))

;; initiate the generic structure monster
(defstruct monster (health (randval 10)))

;; initialize the *monsters* list
(defparameter *monsters* nil)

;; push some monsters to help in developing
(push (make-monster) *monsters*)

(defun show-monsters ()
  (cons 'foe
        (let ((x 0))
          (mapcar
           (lambda (m)
             (if (monster-dead-p m)
                 (list (incf x) 'dead)
               (list (incf x) 'health (monster-health m))))
           *monsters*))))

(show-monsters)
;; '(FOE (1 HEALTH 5) (2 HEALTH 3) (3 HEALTH 7) (4 DEAD))

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead-p)
    (princ "You have been killed. Game over"))
  (when (monsters-dead-p)
    (princ "You killed all the monsters. You win!")))

(defun game-loop ()
  (unless (or (player-dead-p) (monsters-dead-p))
    (show-player)
    (dotimes (i (1+ (truncate (/ (max 0 *player-agility*)))))
      (unless (monsters-dead-p)
        (player-attack)))
    (map 'list
         (lambda (m)
           (or (monster-dead-p m) (monster-attack m)))
         *monsters*)
    (game-loop)))
