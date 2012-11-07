(load "hunt-the-wumpus.lisp")

(setf *edge-num* 1000)
(setf *node-num* 1000)

;; the old implementation with list
(time (dotimes (i 100)
        (get-connected 1 (make-edge-list))))
;; Real time: 94.51717 sec.
;; Run time: 94.4059 sec.
;; Space: 35607128 Bytes
;; GC: 67, GC time: 0.352021 sec.
;; NIL

(time (dotimes (i 100)
        (get-connected-hash 1
                            (hash-edges (make-edge-list)))))
;; Real time: 2.497152 sec.
;; Run time: 2.488156 sec.
;; Space: 33384344 Bytes
;; GC: 63, GC time: 0.312021 sec.
;; NIL
