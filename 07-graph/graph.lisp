;; association list which represents the possible places of the game
(defparameter *wizard-nodes*
  '((living-room (you are in the living-room. a wizard is snoring on the couch.))
    (garden      (you are in a beautiful garden. there is a well in front of you.))
    (attic       (you are in the attic. there is a giant welding torch in the corner.))))

;; the paths the player can take from one place (one node) to another (node)
(defparameter *wizard-edges*
  '((living-room (garden west door)
                 (attic upstairs ladder))
    (garden      (living-room east door))
    (attic       (living-room downstairs ladder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; directed graphs

;; compute a graphviz's dot format compliant name
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(dot-name 'this_is_some_test!)
(dot-name 'this-is-some_test!)
;; "THIS_IS_SOME_TEST_"

(defparameter *max-length* 30)

(write-to-string 'this-is-way-too-much-in-regards-of-the-30-characters-limit
                 :pretty nil)

;; compute the label (and limit to 30 characters)
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (< *max-length* (length s))
            ;; +
            (concatenate 'string (subseq s 0 (- *max-length* 3)) "...")
          s))
    ""))

(dot-label nil)
;; ""

(dot-label 'this-is-ok)
;; "THIS-IS-OK"

(dot-label 'this-is-way-too-much-in-regards-of-the-30-characters-limit)
;; "THIS-IS-WAY-TOO-MUCH-IN-REG..."

;; print the graphviz label
(defun glabel (name)
  (concatenate 'string "[label=\"" name "\"];"))

;; compute the nodes of the graph
(defun nodes->dot (nodes)
  (mapc
   (lambda (node)
     (fresh-line)
     (princ (dot-name (car node)))
     (princ (glabel (dot-label node))))
   nodes))

(nodes->dot *wizard-nodes*)
;; prints
;; LIVING_ROOM[label="(YOU ARE IN THE LIVING-ROOM..."];
;; GARDEN[label="(YOU ARE IN A BEAUTIFUL GAR..."];
;; ATTIC[label="(YOU ARE IN THE ATTIC. THER..."];

;; compute the edges of the graph
(defun edges->dot (edges)
  (mapc
   (lambda (edge)
     (let ((node-src (car edge)))
       (mapc
        (lambda (dest)
          (fresh-line)
          (princ (dot-name node-src))
          (princ "->")
          (princ (dot-name (car dest)))
          (princ (glabel (dot-label (cdr dest)))))
        (cdr edge))))
   edges))

(edges->dot *wizard-edges*)
;; prints
;; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
;; LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
;; GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
;; ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];

;; compute the graph altogether
(defun graph->dot (nodes edges)
  (fresh-line)
  (princ "digraph {")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(graph->dot *wizard-nodes* *wizard-edges*)
;; prints
;; digraph {
;; living_ROOM[label="(YOU ARE IN THE LIVING-ROOM..."];
;; GARDEN[label="(YOU ARE IN A BEAUTIFUL GAR..."];
;; ATTIC[label="(YOU ARE IN THE ATTIC. THER..."];
;; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
;; LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
;; GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
;; ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"] ;}

;; create the graph file
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
                  (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng " fname " > " fname ".png")))

(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda () (graph->dot nodes edges))))

(graph->png "wizard-graph.dot" *wizard-nodes* *wizard-edges*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; undirected graphs

(defun uedges->dot (edges)
  (maplist
   (lambda (lst)
     (mapc (lambda (edge)
             (unless (assoc (car edge) (cdr lst))
               (fresh-line)
               (princ (dot-name (caar lst)))
               (princ "--")
               (princ (dot-name (car edge)))
               (princ (glabel (dot-label (cdr edge))))))
           (cdar lst)))
   edges))

(uedges->dot *wizard-edges*)

(defun ugraph->dot (nodes edges)
  (princ "graph {")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda () (ugraph->dot nodes edges))))

(ugraph->png "wizard-ugraph.dot" *wizard-nodes* *wizard-edges*)
