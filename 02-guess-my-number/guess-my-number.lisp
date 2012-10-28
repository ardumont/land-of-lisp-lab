;; the guess my number implementation

;; the minimal borne
(defparameter *small* 1)
;; the maximal one
(defparameter *big* 100)

(defun guess-my-number ()
  ;; shift to the left (divide by 2)
  (ash (+ *small* *big*) -1))

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over ()
  ;; the minimal borne
  (defparameter *small* 1)
  ;; the maximal one
  (defparameter *big* 100)
  ;; launch a new game
  (guess-my-number))
