;;
;; Section 2.1.4
;;

(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

;; exercise 2.7

(define (lower-bound interval)
  (let ((a (car interval))
	(b (cdr interval)))
    (if (> a b) b a)))

(define (upper-bound interval)
  (let ((a (car interval))
	(b (cdr interval)))
    (if (> a b) a b)))


(lower-bound (make-interval 10 20)) ;; => 10
(upper-bound (make-interval 10 20)) ;; => 20

(lower-bound (make-interval 99 11)) ;; => 11
(upper-bound (make-interval 99 11)) ;; => 99


(add-interval (make-interval 10 20) (make-interval 99 11)) ;; => (21 . 119)
(mul-interval (make-interval 10 20) (make-interval 99 11)) ;; => (110 . 1980)



;; exercise 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

(sub-interval (make-interval 10 20) (make-interval 99 11)) ;; => (-89 . 9)
(sub-interval (make-interval 99 11) (make-interval 10 20)) ;; => (-9 . 89)

