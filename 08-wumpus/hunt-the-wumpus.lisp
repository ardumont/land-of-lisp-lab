;; load another lisp file
(load "../07-graph/graph.lisp")

;; the nodes of the city '((1) (2)...)
(defparameter *congestion-city-nodes* nil)
;; the edges '((1 (2) (3))...
(defparameter *congestion-city-edges* nil)
;; the nodes the player visited
(defparameter *visited-nodes* nil)
;; the current position
(defparameter *player-pos* nil)
;; limit number of nodes
(defparameter *node-num* 30)
;; limit number of edges
(defparameter *edge-num* 45)
;; to determine randomly 3 nodes with glow-worm on them (player on such node -> teleportation)
(defparameter *worm-num* 3)
;; to determine randomly some node with cops on them (player on such node -> Game over)
(defparameter *cop-odds* 15)

;; compute a random number between 1 and *node-num*
;; this corresponds to the number of nodes of the graph
(defun random-node ()
  (1+ (random *node-num*)))

;; compute an edge pair a and b such as (<> a b)
(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(edge-pair '1 '1)
;; nil

(edge-pair :a :b)
;; '((:a . :b) (:b . :a))

;; create the random graph of edges
(defun make-edge-list ()
  (loop for i from 1 to *edge-num*
        append (edge-pair (random-node) (random-node))))

(make-edge-list)
;; '((1 . 26) (26 . 1) (15 . 5) (5 . 15) (26 . 5) (5 . 26) (5 . 9) (9 . 5) (1 . 16)
;;  (16 . 1) (21 . 26) (26 . 21) (21 . 12) (12 . 21) (30 . 26) (26 . 30) (5 . 23)
;;  (23 . 5) (28 . 19) (19 . 28) (13 . 16) (16 . 13) (28 . 23) (23 . 28) (3 . 26)
;;  (26 . 3) (16 . 29) (29 . 16) (28 . 10) (10 . 28) (16 . 11) (11 . 16) (26 . 14)
;;  (14 . 26) (25 . 5) (5 . 25) (23 . 8) (8 . 23) (18 . 5) (5 . 18) (23 . 18)
;;  (18 . 23) (7 . 11) (11 . 7) (1 . 5) (5 . 1) (15 . 29) (29 . 15) (5 . 27) (27 . 5)
;;  (14 . 24) (24 . 14) (15 . 8) (8 . 15) (30 . 14) (14 . 30) (6 . 26) (26 . 6)
;;  (25 . 17) (17 . 25) (20 . 7) (7 . 20) (15 . 9) (9 . 15) (26 . 14) (14 . 26)
;;  (18 . 22) (22 . 18) (20 . 9) (9 . 20) (19 . 9) (9 . 19) (2 . 12) (12 . 2)
;;  (17 . 7) (7 . 17) (2 . 11) (11 . 2) (15 . 11) (11 . 15) (22 . 9) (9 . 22)
;;  (13 . 18) (18 . 13) (5 . 9) (9 . 5) (8 . 25) (25 . 8))

;; for testing the method
(defparameter *edges-test* '((1 . 26) (26 . 1) (15 . 5) (5 . 15) (26 . 5) (5 . 26) (5 . 9) (9 . 5) (1 . 16) (16 . 1) (21 . 26) (26 . 21) (21 . 12) (12 . 21) (30 . 26) (26 . 30) (5 . 23) (23 . 5) (28 . 19) (19 . 28) (13 . 16) (16 . 13) (28 . 23) (23 . 28) (3 . 26) (26 . 3) (16 . 29) (29 . 16) (28 . 10) (10 . 28) (16 . 11) (11 . 16) (26 . 14) (14 . 26) (25 . 5) (5 . 25) (23 . 8) (8 . 23) (18 . 5) (5 . 18) (23 . 18) (18 . 23) (7 . 11) (11 . 7) (1 . 5) (5 . 1) (15 . 29) (29 . 15) (5 . 27) (27 . 5) (14 . 24) (24 . 14) (15 . 8) (8 . 15) (30 . 14) (14 . 30) (6 . 26) (26 . 6) (25 . 17) (17 . 25) (20 . 7) (7 . 20) (15 . 9) (9 . 15) (26 . 14) (14 . 26) (18 . 22) (22 . 18) (20 . 9) (9 . 20) (19 . 9) (9 . 19) (2 . 12) (12 . 2) (17 . 7) (7 . 17) (2 . 11) (11 . 2) (15 . 11) (11 . 15) (22 . 9) (9 . 22) (13 . 18) (18 . 13) (5 . 9) (9 . 5) (8 . 25) (25 . 8)))

;; Find all the edges which start from a given node
(defun direct-edges (node edges)
  (remove-if-not (lambda (edge) (eql (car edge) node)) edges))

(direct-edges 1 *edges-test*)
;; '((1 . 26) (1 . 16) (1 . 5))

;; Take a source node and an edges list of nodes to compute a list of all nodes connected
;; to that source node (even if it requires walking accross multiple nodes)
(defun get-connected (node edges)
  (let ((visited nil))
    (labels ((traverse (n)
                       (unless (member n visited)
                         (push n visited)
                         (mapc (lambda (edge) (traverse (cdr edge))) (direct-edges n edges)))))
      (traverse node)
      visited)))

(get-connected 1 *edges-test*)

;; find islands in nodes
(defun find-islands (nodes edges)
  (let ((islands nil))
    (labels ((find-island
              (nodes)
              (let* ((connected (get-connected (car nodes) edges))
                     (unconnected (set-difference nodes connected)))
                (push connected islands)
                (when unconnected
                  (find-island unconnected)))))
      (find-island nodes))
    islands))

;; all the nodes without duplicates
(defun all-nodes (edges)
  (remove-duplicates (mapcar #'car edges)))

(all-nodes *edges-test*)
;; '(21 3 28 10 16 23 1 29 27 24 30 6 26 14 20 19 12 17 7 2 15 11 22 13 18 5 9 8 25)

(find-islands (all-nodes *edges-test*) *edges-test*)
;; '((6 3 24 14 30 27 10 22 25 17 12 2 11 7 20 9 19 28 23 8 29 15 5 18 13 16 1 26 21))

;; connect islands between them
;; for this we connect the first node of the island with the first node of the second island
;; then recursively connect the other remaining islands. In the end, we obtain a connect graph
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(connect-with-bridges '((1 2 3) (4 5 6)))
;; '((1 . 4) (4 . 1))
(connect-with-bridges '(()))
;; nil
(connect-with-bridges (find-islands (all-nodes *edges-test*) *edges-test*))
;; nil

(defun connect-all-islands (nodes edges)
  (append (connect-with-bridges (find-islands nodes edges)) edges))

(connect-all-islands (all-nodes *edges-test*) *edges-test*)
;; ((1 . 26) (26 . 1) (15 . 5) (5 . 15) (26 . 5) (5 . 26) (5 . 9) (9 . 5) (1 . 16)
;; (16 . 1) (21 . 26) (26 . 21) (21 . 12) (12 . 21) (30 . 26) (26 . 30) (5 . 23)
;; (23 . 5) (28 . 19) (19 . 28) (13 . 16) (16 . 13) (28 . 23) (23 . 28) (3 . 26)
;; (26 . 3) (16 . 29) (29 . 16) (28 . 10) (10 . 28) (16 . 11) (11 . 16) (26 . 14)
;; (14 . 26) (25 . 5) (5 . 25) (23 . 8) (8 . 23) (18 . 5) (5 . 18) (23 . 18)
;; (18 . 23) (7 . 11) (11 . 7) (1 . 5) (5 . 1) (15 . 29) (29 . 15) (5 . 27) (27 . 5)
;; (14 . 24) (24 . 14) (15 . 8) (8 . 15) (30 . 14) (14 . 30) (6 . 26) (26 . 6)
;; (25 . 17) (17 . 25) (20 . 7) (7 . 20) (15 . 9) (9 . 15) (26 . 14) (14 . 26)
;; (18 . 22) (22 . 18) (20 . 9) (9 . 20) (19 . 9) (9 . 19) (2 . 12) (12 . 2)
;; (17 . 7) (7 . 17) (2 . 11) (11 . 2) (15 . 11) (11 . 15) (22 . 9) (9 . 22)
;; (13 . 18) (18 . 13) (5 . 9) (9 . 5) (8 . 25) (25 . 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; building the final edges

;; transform the edges map into an association list
(defun edges-to-alist (edges)
  (mapcar
   (lambda (node)
     (cons node
           (mapcar
            (lambda (nd)
              (list (cdr nd)))
            (remove-duplicates (direct-edges node edges)
                               :test #'equal))))
   (remove-duplicates (mapcar #'car edges))))

(edges-to-alist (make-edge-list))
;; one possible output
;; ((5 (30) (27) (24))
;;  (20 (23) (11))...)

;; add cops to some routes
(defun add-cops (a-edges cops)
  (mapcar
   (lambda (node)
     (let* ((node1 (car node))
            (node1-edges (cdr node)))
       (cons node1
             (mapcar
              (lambda (edge)
                (let ((node2 (car edge)))
                  (if (intersection (edge-pair node1 node2) cops
                                    :test #'equal)
                      (list node2 'cops)
                    edge)))
              node1-edges))))
   a-edges))

(add-cops (edges-to-alist *edges-test*) '((21 . 26)))
;; ((21 (26 COPS) (12))
;;  (3 (26))
;;  (26 (1) (5) (21 COPS) (30) (3) (6) (14)))
;; ...

;; the full map
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num* collect i))
         (edges (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (n) (zerop (random *cop-odds*))) edges)))
    (add-cops (edges-to-alist edges) cops)))

(defparameter *a-edges*
  (make-city-edges))

;; compute the list of neighbors of the node in the a-edges
(defun neighbors (node a-edges)
  (mapcar #'car (cdr (assoc node a-edges))))

;; return the list containing the neighbors (starting from b if b is a neighbor of a)
(defun within-one (a b a-edges)
  (member b (neighbors a a-edges)))

;; return a value (so true) if b is within 2 edges from a
(defun within-two (a b a-edges)
  (or (within-one a b a-edges)
      (some
       (lambda (node) (within-one node b a-edges))
       (neighbors a a-edges))))

;; create randomly the graph with the protagonist of the game
;; init:
;; - the wumpus is randomly put on node
;; - the glow-worms too (see *worm-num* for the size of gang)
;; the rest are some alerts for helping the player choosing the next node :
;; - wumpus is on the node, we tag it 'wumpus
;; - wumpus is within two nodes, we tag the node 'blood
;; - one member of the glow-worm is present, we tag it with 'glow-worm
;; - one member of the glow-worm is within one node, we tag it 'light
;; - one edge is tagged with cops, we tag the node with 'sirens
(defun make-city-nodes (a-edges)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num* collect (random-node))))
    (loop for n from 1 to *node-num*
          collect (append (list n)
                          (cond ((eql n wumpus) '(wumpus));; we found the wumpus
                                ((within-two n wumpus a-edges) '(blood!)));; we found trail of wumpus
                          (cond ((member n glow-worms) '(glow-worms))
                                ((some (lambda (worm) (within-one n worm a-edges)) glow-worms) '(light!)))
                          (when (some #'cdr (cdr (assoc n a-edges))) '(sirens!))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start a new game

;; find some empty node (recursive if the one picked is not empty)
(defun find-empty-node ()
  (let ((rn (random-node)))
    (if (cdr (assoc rn *congestion-city-nodes*))
        (find-empty-node)
      rn)))

;; draw the city map
(defun draw-city (dot-filename)
  (ugraph->png dot-filename *congestion-city-nodes* *congestion-city-edges*))

(draw-city "city.dot")
(display-graph "city.dot.png")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; one tour

;; Display the nodes with some informations
;; (* for the player's current position, ? for a one node-neighbor not yet visited)
(defun known-city-nodes ()
  (mapcar
   (lambda (node)
     (if (member node *visited-nodes*);; already visited?
         ;; retrieve the node
         (let ((n (assoc node *congestion-city-nodes*)))
           (if (eql node *player-pos*);; is the player on this node?
               ;; yes, we append the * on the nodes (we keep the previous informations)
               (append n '(*))
             ;; no, we give the node n back as is
             n))
       ;; not yet visited, we mark them as unknown
       ;; we do not give the nodes informations, it's a surprise :D
       (list node '?)))
   ;; computes the visited nodes and their one-node neighbors into a single list
   (remove-duplicates
    (append *visited-nodes*
            (mapcan
             (lambda (node)
               (neighbors node *congestion-city-edges*))
             *visited-nodes*)))))

(known-city-nodes)
;; possible output:
;; ((18 ?) (28 ?) (3 WUMPUS) (22 ?) (8 BLOOD! LIGHT!) (16 ?)
;; (11 GLOW-WORMS) (5) (24 ?) (14 LIGHT!) (9 BLOOD! GLOW-WORMS *) (20) (25)
;; (13 LIGHT!) (6 ?) (21) (7 ?))
;; which reads:
;; - the player is currently on node 9
;; - the player is within 2 nodes of the wumpus
;; - he got hit by a glow-worm
;; ...

;; FIXME I do not really get this function!
;; Now, we need to create an alist stripped of any cop sirens that we haven’t reached yet
(defun known-city-edges ()
  (mapcar
   (lambda (node)
     (cons node
           (mapcar
            (lambda (nd)
              (if (member (car nd) *visited-nodes*)
                  nd
                (list (car nd))));; we strip the cdr (which if it exists is cops)
            (cdr (assoc node *congestion-city-edges*)))))
   *visited-nodes*))

;; draw the city's nodes and edges
(defun draw-known-city (dot-filename)
  (ugraph->png dot-filename (known-city-nodes) (known-city-edges)))

(draw-known-city "known-city.dot")
(display-graph  "known-city.dot.png")

;; this function starts a new game :
;; - randomly create edges
;; - randomly create nodes
;; - randomly put the player in some node
;; - computes the position of each protagonist
;; - draw the city's graph
;; - draw the player's city's graph
(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city "city.dot")
  (draw-known-city "known-city.dot"))

(new-game)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; walk/charge/handle-direction/handle-new-place

;; Handle the player's movement to the position pos.
;; if the new position contains:
;; - cops, the game is over
;; - the wumpus and the player is charging, the player wins (wumpus is dead)
;; - the wumpus and the player is not charging, the player loses (wumpus kills him)
;; - nothing special and the player is charging, the player loses (no more bullet)
;; - glow-worms, the player is teleported randomly in a new node
(defun handle-new-place (edge-pos pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and
                    (member 'glow-worms node)
                    (not (member pos *visited-nodes*)))))
    ;; updated the visited-nodes
    (pushnew pos *visited-nodes*)
    ;; we move the player to the new pos
    (setf *player-pos* pos)
    ;; we force a new draw of the know city
    (draw-known-city "known-city.dot")
    ;; at last, we check some stuff
    (cond
     ;; cops
     ((member 'cops edge-pos) (princ "You ran into cops! Game over!"))
     ;; wumpus?
     ((member 'wumpus node)
      (if charging
          (princ "You shot the wumpus! WIN!!!!!")
        (princ "The wumpus shot you! Game over!")))
     ;; lost if charging
     (charging (princ "You shot in the air! Game over!"))
     ;; glow-worms, random node
     (has-worm
      (let ((nnode (random-node)))
        (princ "you ran into glow worms! Teleportation...")
        (princ nnode)
        (handle-new-place nil nnode nil))))))

;; check if the movement is legal then, move the player to the position pos
(defun handle-direction (pos charging)
  (let ((edge (assoc pos
                     (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
        ;; legal
        (handle-new-place edge pos charging)
        ;; illegal
        (princ "That location does not exist or is not reachable from here!"))))

;; walk in the direction pos without firing the gun
(defun walk (pos)
  (handle-direction pos nil))

;; walk in the direction pos, ready to shoot the wumpus
(defun charge (pos)
  (handle-direction pos 't))

;; some play with the game
(new-game)
(walk 0)
;; That location does not exist or is not reachable from here!
(walk 21)
(walk 25)
(walk 13)
(walk 6)
;; That location does not exist or is not reachable from here!
(walk 20)
(walk 14)
(walk 5)
(walk 14)
(walk 11)
(walk 9)
(walk 8)
(charge 3)

;; utility function near hand to fire the drawing
(draw-city "city.dot")
(display-graph  "city.dot.png")
(draw-known-city "known-city.dot")
(display-graph  "known-city.dot.png")

