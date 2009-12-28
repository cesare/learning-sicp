;;
;; Exercise 2.2
;;


(define (make-segment start-x start-y end-x end-y)
  (list (make-point start-x start-y)
	(make-point end-x end-y)))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (car (cdr line)))


(define (make-point x y)
  (list x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (car (cdr p)))


(define (midpoint-segment line)
  (define (average a b) (/ (+ a b) 2))
  (let ((start (start-segment line))
	(end (end-segment line)))
  (make-point (average (x-point start) (x-point end))
	      (average (y-point start) (y-point end)))))


(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))



;; example
(make-segment 98 76 12 34) ;; => ((98 76) (12 34))

(start-segment (make-segment 98 76 12 34)) ;; => (98 76)
(end-segment (make-segment 98 76 12 34))   ;; => (12 34)

(midpoint-segment (make-segment 98 76 12 34)) ;; => (55 55)
(print-point (midpoint-segment (make-segment 98 76 12 34)))


;; example (much more understandable)
(print-point (midpoint-segment (make-segment 0 0 12 34))) ;; => (6,17)


;; another example
(print-point (midpoint-segment (make-segment 0 0 11 33))) ;; => (11/2,33/2)

