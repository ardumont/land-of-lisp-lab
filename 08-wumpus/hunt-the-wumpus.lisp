;; load another lisp file
(load "../07-graph/graph.lisp")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *player-pos* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
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

;; create the random graph
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
(defparameter *edges* '((1 . 26) (26 . 1) (15 . 5) (5 . 15) (26 . 5) (5 . 26) (5 . 9) (9 . 5) (1 . 16) (16 . 1) (21 . 26) (26 . 21) (21 . 12) (12 . 21) (30 . 26) (26 . 30) (5 . 23) (23 . 5) (28 . 19) (19 . 28) (13 . 16) (16 . 13) (28 . 23) (23 . 28) (3 . 26) (26 . 3) (16 . 29) (29 . 16) (28 . 10) (10 . 28) (16 . 11) (11 . 16) (26 . 14) (14 . 26) (25 . 5) (5 . 25) (23 . 8) (8 . 23) (18 . 5) (5 . 18) (23 . 18) (18 . 23) (7 . 11) (11 . 7) (1 . 5) (5 . 1) (15 . 29) (29 . 15) (5 . 27) (27 . 5) (14 . 24) (24 . 14) (15 . 8) (8 . 15) (30 . 14) (14 . 30) (6 . 26) (26 . 6) (25 . 17) (17 . 25) (20 . 7) (7 . 20) (15 . 9) (9 . 15) (26 . 14) (14 . 26) (18 . 22) (22 . 18) (20 . 9) (9 . 20) (19 . 9) (9 . 19) (2 . 12) (12 . 2) (17 . 7) (7 . 17) (2 . 11) (11 . 2) (15 . 11) (11 . 15) (22 . 9) (9 . 22) (13 . 18) (18 . 13) (5 . 9) (9 . 5) (8 . 25) (25 . 8)))

;; Find all the edges which start from a given node
(defun direct-edges (node edges)
  (remove-if-not (lambda (edge) (eql (car edge) node)) edges))

(direct-edges 1 *edges*)
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

(get-connected 1 *edges*)

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

(all-nodes *edges*)
;; '(21 3 28 10 16 23 1 29 27 24 30 6 26 14 20 19 12 17 7 2 15 11 22 13 18 5 9 8 25)

(find-islands (all-nodes *edges*) *edges*)
;; '((6 3 24 14 30 27 10 22 25 17 12 2 11 7 20 9 19 28 23 8 29 15 5 18 13 16 1 26 21))

;; connect islands between them
;; for this we connect the first node of the island with the first node of the second island
;; then recursively connect the other remaining islands
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(connect-with-bridges '((1 2 3) (4 5 6)))
;; '((1 . 4) (4 . 1))
(connect-with-bridges '(()))
;; nil
(connect-with-bridges (find-islands (all-nodes *edges*) *edges*))
;; nil

(defun connect-all-islands (nodes edges)
  (append (connect-with-bridges (find-islands nodes edges)) edges))

(connect-all-islands (all-nodes *edges*) *edges*)
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

(add-cops (edges-to-alist *edges*) '((21 . 26)))
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

;; return the list containing the neighbors from b is b is in the neighbor of a
(defun within-one (a b a-edges)
  (member b (neighbors a a-edges)))

(defun within-two (a b a-edges)
  (or (within-one a b a-edges)
      (some
       (lambda (node) (within-one node b a-edges))
       (neighbors a a-edges))))

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

(defun find-empty-node ()
  (let ((rn (random-node)))
    (if (cdr (assoc rn *congestion-city-nodes*))
        (find-empty-node)
      rn)))

;; draw the city map
(defun draw-city ()
  (ugraph->png "city.dot" *congestion-city-nodes* *congestion-city-edges*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; one tour

;; Display some informations on node we visited
;; (* for player, ? for one-node near not yet visited)
(defun known-city-nodes ()
  (mapcar
   (lambda (node)
     (if (member node *visited-nodes*)
         (let ((n (assoc node *congestion-city-edges*)))
           (if (eql node *player-pos*)
               (append n '(*))
             n))
       (list node '?)))
   ;; computes the visited nodes and the one just one node from them
   (remove-duplicates
    (append *visited-nodes*
            (mapcan
             (lambda (node)
               (neighbors node *congestion-city-edges*))
             *visited-nodes*)))))

(known-city-nodes)
;; possible output:
;; '((17 (5) (3) (26) *) (5 ?) (3 ?) (26 ?))
;; which reads:
;; - the player is currently on node 17
;; - there are 3 nodes 5, 3 and 26 he can visit at the next step
;; how can we check?
;; - look at the graph you generated with new-game, you should spot the corresponding subgraph

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

(defun draw-known-city ()
  (ugraph->png "know-city.dot" (known-city-nodes) (known-city-edges)))

(draw-known-city)

;; this function starts a new game and computes the position of each protagonist
(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(new-game)
