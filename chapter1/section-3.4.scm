;;
;; 1.3.4
;;
(define (average a b) (/ (+ a b) 2))
(define (square x) (* x x))

(define (average-dump f)
  (lambda (x) (average x (f x))))


((average-dump square) 10)
;; => 55

(define (sqrt x)
  (fixed-point (average-dump (lambda (y) (/ x y)))
	       1.0))

(sqrt 2)
;; => 1.4142135623746899


(define (cube-root x)
  (fixed-point (average-dump (lambda (y) (/ x (square y))))
	       1.0))

(cube-root 3)
;; => 1.4422517984541108



;;
;; Newton's Method
;;

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (square x) (* x x))
(define (cube x) (* x x x))

((deriv cube) 5)
;; => 75.00014999664018


(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))

(sqrt 5)
;; => 2.2360679775020436
(map sqrt '(1 2 3 4 5 6 7 8 9 10))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Abstractions and first-class procedures
;;

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			    average-dump
			    1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
			    newton-transform
			    1.0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Exercise 1.40
;;

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(newtons-method (cubic 2 3 4) 1)

;; correct???



;;
;; Exercise 1.41
;;
(define (inc n) (+ n 1))

(define (double f)
  (lambda (x)
    (f (f x))))

(((double (double double)) inc) 5)


((double inc) 3) ;; => 5
((double inc) 7) ;; => 9
((double (double inc)) 3) ;; => 7
(((double double) inc) 5) ;; => 9


((double square) 3) ;; => 81
((double square) 5) ;; => 625


;;
;; Exercise 1.42
;;

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6) ;; => 49


;;
;; Exercise 1.43
;;

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5) ;; =>   625
((repeated inc 10) 5)   ;; =>    15
((repeated cube 2) 3)   ;; => 19683


;; another answer
(define (repeated f n)
  (cond ((= n 0) identity)
	((= n 1) f)
	(else (compose f (repeated f (- n 1))))))

((repeated square 0) 5) ;; =>   5
((repeated cube 0) 987) ;; => 987



;;
;; Exercise 1.44
;;

(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx)))
       3)))

((smooth square) 5)


((smooth (smooth square)) 5)
(((repeated smooth 2) square) 5)

(define (n-fold-smooth f n)
  ((repeated smooth n) f)) ;; -->(repeated (smooth f) n)


((n-fold-smooth square 10) 12)


;;
;; Exercise 1.45
;;
(define (fourth-root x)
  (fixed-point (lambda (f) (average-dump (average-dump f))
		       (lambda (y) (/ x (* y y y)))) 1.0))
;; still working...

(fourth-root 16) ; should == 2.00
