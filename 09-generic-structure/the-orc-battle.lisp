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






