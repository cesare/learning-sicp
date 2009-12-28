;;
;; Exercise 2.3
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from exercise 2.2

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-rectangle x1 y1 x2 y2)
  (make-segment x1 y1 x2 y2))

(define (width rect)
  (let ((point-a (start-segment rect))
	(point-b (end-segment rect)))
    (abs (- (x-point point-a) (x-point point-b)))))

(define (height rect)
  (let ((point-a (start-segment rect))
	(point-b (end-segment rect)))
    (abs (- (y-point point-a) (y-point point-b)))))


(define (area rect)
  (* (width rect) (height rect)))


;; example
(make-rectangle 0 0 10 20)  ;; => ((0 0) (10 20))
(width (make-rectangle 0 0 10 20))  ;; => 10
(height (make-rectangle 0 0 10 20)) ;; => 20
(area (make-rectangle 0 0 10 20))   ;; => 200

