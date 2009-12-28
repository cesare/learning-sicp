;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; 1.3.1
;;

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(sum-integers 9 12) ;; => 42
(+ 9 (sum-integers 10 12))
(+ 9 (+ 10 (sum-integers 11 12)))
(+ 9 (+ 10 (+ 11 (sum-integers 12 12))))
(+ 9 (+ 10 (+ 11 (+ 12 (sum-integers 13 12)))))
(+ 9 (+ 10 (+ 11 (+ 12 0))))
(+ 9 (+ 10 (+ 11 12)))
(+ 9 (+ 10 23))
(+ 9 33)
42


(define (cube a) (* a a a))
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(cube 5) ;; => 125
(cube 6) ;; => 216
(cube 7) ;; => 343
(+ (cube 5) (cube 6) (cube 7)) ;; 684

(sum-cubes 5 7) ;; => 684 : 125 + 216 + 343

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (cube n) (* n n n))

(define (sum-cube a b)
  (sum cube a inc b))

(define (identity n) n)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Exercise 1.29
;;

(define (simpson-integal f a b n)
  (let ((h (/ n (- b a))))
    (define (y k)
      (f (+ a (* k h))))
    (define (simpson a b current)
      (print "h=" h)
      (print "(simpson " a " " b " " current ")")
      (cond ((= current 0)
	     (y current))
	    ((= current n)
	     (y current))
	    ((even? current)
	     (* 2 (y current)))
	    (else
	     (* 4 (y current)))))
    (define (simpson-integral-iter a b current)
      (if (= current 0)))))

(simpson-integal cube 0 1 100)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; still working
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Exercise 1.30
;;

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

(sum identity 1 (lambda (n) (+ n 1)) 10)
;; => 55
(sum identity 1 (lambda (n) (+ n 1)) 100)
;; => 5050
(sum (lambda (n) (* n n)) 1 (lambda (x) (+ x 1)) 5)
;; => 55, 1 + 4 + 9 + 16 + 25


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Exercise 1.31
;;

(define (inc n) (+ n 1))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

(product identity 1 inc 10)  ;; => 3628800

(define (factorial n)
  (product identity 1 inc n))

(factorial 10) ;; => 3628800

(define (our-pi)
  (define (numer n)
    (product (lambda (x) (if (odd? x) (+ x 1) (+ x 2))) 1 inc n))
  (define (denom n)
    (product (lambda (x) (if (odd? x) (+ x 2) (+ x 1))) 1 inc n))
  (/. (* 4 (numer 1000)) (denom 1000)))

(our-pi) ;; => 3.1570301764551676


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Exercise 1.32
;;

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (acc-sum a b)
  (accumulate + 0 identity a inc b))

(acc-sum 1 100) ;; => 5050

(define (acc-prod a b)
  (accumulate * 1 identity a inc b))

(* 1 2 3 4 5)   ;; => 120
(acc-prod 1 5)  ;; => 120

(* 1 2 3 4 5 6 7 8 9 10) ;; => 3628800
(acc-prod 1 10)          ;; => 3628800


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Exercise 1.33
;;

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
	result
	(if (filter a)
	    (iter (next a) (combiner result (term a)))
	    (iter (next a) result))))
  (iter a null-value))

;; example
(define (inc a) (+ a 1))
(filtered-accumulate + 0 identity 1 inc 100 even?) ;; => 2550
(filtered-accumulate + 0 identity 1 inc 100 odd?)  ;; => 2500


;; answer for a.
(define (sum-of-square-of-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

(sum-of-square-of-prime 1 100) ;; => 65797

;; copied from chapter 1.2.6;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (square x) (* x x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; answer for b.
(define (product-of-relatively-prime-to n)
  (define (relatively-prime? i)
    (= (gcd n i) 1))
  (filtered-accumulate * 1 identity 1 inc n relatively-prime?))

(product-of-relatively-prime-to 10) ;; 189
(product-of-relatively-prime-to 20) ;; 8729721



;;
;; Exercise 1.34
;;

(define (square x) (* x x))
(define (f g) (g 2))

(f square) ;; => 4
(f (lambda (z) (* z (+ z 1)))) ;; => 6

(f f)
(f 2)
(2 2) ;; => no such function '2'



;;
;; 1.3.3
;;
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (f x) (+ 1 (* x 2)))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))


(half-interval-method sin 2.0 4.0)
;; => 3.14111328125

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
		      1.0 2.0)
;; => 1.89306640625
;; f(x) = x^3 - 2x - 3



(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
;;    (print "(try " guess ")")
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(fixed-point cos 1.0)
;; => 0.7390822985224023

(fixed-point cos 100)

(fixed-point (lambda (y) (+ (sin y) (cos y)))
	     1.0)
;; => 1.2587315962971173



(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
	       1.0))
;; => never converge

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))
(define (average a b) (/ (+ a b) 2))

(sqrt 2)
;; => 1.4142135623746899

(sqrt 3)
;; => 1.7320508075688772


;;
;; Exercise 1.35
;;
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;; => 1.6180327868852458

;;
;; Exercise 1.36
;;
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(let ((log1000 (log 1000)))
  (fixed-point (lambda (x) (/ log1000 (log x))) 1.1))

;;
;; Exercise 1.37
;;

(define (cont-frac n d k)
  (define (calculate-cont-frac i)
    (if (= i k)
	(/ (n i) (d i))
	(/ (n i)
	   (+ (d i) (calculate-cont-frac (+ i 1))))))
  (calculate-cont-frac 1))

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   20)
;; => 0.6180339850173578


;;
;; Exercise 1.38
;;

(cont-frac (lambda (i) 1.0)
	   (lambda (i)
	     (if (= (remainder i 3) 2)
		 (* (/ (+ i 1) 3) 2)
		 1))
	   10)
;; => 0.7182817182817183

(exp 1)       ;; => 2.718281828459045
(- (exp 1) 2) ;; => 0.7182818284590451


;;
;; Exercise 1.39
;;

(define (tan-cf x k)
  (define (calculate-tan-cf i)
    (/ (if (= i 1) x (* x x))
       (- (- (* i 2) 1)
	  (if (= i k) 0 (calculate-tan-cf (+ i 1))))))
  (calculate-tan-cf 1))

(tan 1.0)          ;; => 1.557407724654902
(tan-cf 1.0 10)    ;; => 1.557407724654902

(tan 3.1415)       ;; => -9.265359005819131e-5
(tan-cf 3.1415 10) ;; => -9.265548215448259e-5
