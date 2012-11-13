(load "../07-graph/graph.lisp")

;; compute the nodes from the assoc list of edges
(defun compute-nodes (functions-list)
  (let ((nodes nil))
    (mapc
     (lambda (nd)
       (unless (member (list (car nd)) nodes)
         (push (list (car nd)) nodes))
       (mapc (lambda (n)
               (unless (member (list n) nodes)
                 (push (list n) nodes)))
             (cadr nd)))
     functions-list)
    nodes))

;; compute the dot file
(defun graph-functions (file functions)
  (graph->png file (compute-nodes functions) functions))
