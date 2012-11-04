;; load another lisp file
(load "../07-graph/graph.lisp")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
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





