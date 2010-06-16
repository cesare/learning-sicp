;;
;; Exercise 3.5
;;

(use srfi-27)
(define random (lambda (n) (* (random-real) n)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


(define (estimate-integral pred x1 y1 x2 y2 trials)
  (*. (monte-carlo trials pred)
      (* (- x2 x1) (- y2 y1))))


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (square n) (* n n))

(define (within-unit-circle?)
  (<= (+ (square (- (random-in-range 0 2) 1))
         (square (- (random-in-range 0 2) 1)))
      1))
