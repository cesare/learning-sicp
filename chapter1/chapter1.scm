(define (sum-of-square x y)
  (+ (* x x) (* y y)))

;;
;; Exercise 1.3
;;

;;
;; simple solution
;;
(define (sum-of-larger-square x y z)
  (if (> x y)
      (sum-of-square x (max y z))
      (sum-of-square y (max x z))))

;;
;; another solution
;;
(define (sum-of-larger-square2 x y z)
  (sum-of-square (max x y) (max (min x y) z)))

(sum-of-larger-square 9 7 5)
(sum-of-larger-square 9 5 7)
(sum-of-larger-square 7 9 5)
(sum-of-larger-square 7 5 9)
(sum-of-larger-square 5 7 9)
(sum-of-larger-square 7 9 7)

;;
;; 1.1.7
;;
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.000001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (square x) (* x x))

;;
;; Exercise 1.6
;;
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))


(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess)
		     x)))


; => sqrt-iter never terminates
;     because 3rd parameter for new-if should be evaluated before passed to new-if procedure.
;     (it causes infinite recursion)



;;
;; Exercise 1.8
;;


;;
;; Exercise 1.9
;;
(define (+ a b)  ;; [a]
  (if (= a 0)
    b
    (inc (+ (dec a) b))))

(define (+ a b)  ;; [b]
  (if (= a 0)
    b
    (+ (dec a) (inc b))))

;; => (+ 4 5) in [a]
(+ 4 5)
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc (+ (dec 3) 5)))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9


;; => (+ 4 5) in [b]
(+ 4 5)
(+ (dec 4) (inc 5))
(+ 3 6)
(+ (dec 3) (inc 6))
(+ 2 7)
(+ (dec 2) (inc 7))
(+ 1 8)
(+ (dec 1) (inc 8))
(+ 0 9)
9


;;
;; Exercise 1.10
;;
(define (A x y )
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 2 4)

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

;; positive integer means n > 0
;;
;; f = 2y
;; g = 2^n
;; h = 2^(2^n)

(g 3)
(A 1 3)
(A 0 (A 1 2))
(A 0 (A 0 (A 1 1)))
(A 0 (A 0 2))
(A 0 4)
8

(g 4)
(A 1 4)
(A 0 (A 1 3))
(A 0 (A 0 (A 1 2)))
(A 0 (A 0 (A 0 (A 1 1))))
(A 0 (A 0 (A 0 2)))
(A 0 (A 0 4))
(A 0 8)
16

(h 3)
(A 2 3)
(A 1 (A 2 2))
(A 1 (A 1 (A 2 1)))
(A 1 (A 1 2))
(A 1 (A 0 (A 1 1)))
(A 1 (A 0 2))
(A 1 4)
(A 0 (A 1 3))
(A 0 (A 0 (A 1 2)))
(A 0 (A 0 (A 0 (A 1 1))))
(A 0 (A 0 (A 0 2)))
(A 0 (A 0 4))
(A 0 8)
16

(h 4) ;; using (A 1 n) => 2^n
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 4))
(A 1 16)
65536


;;
;; Example: Counting change
;;
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount (- kinds-of-coins 1))
		 (cc (- amount (first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1)  1)
	((= kinds-of-coins 2)  5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(count-change 200)


;;
;; Exercise 1.11
;;
(define (f n)
  (cond ((< n 3) n)
	(else (+ (f (- n 1))
		 (* 2 (f (- n 2)))
		 (* 3 (f (- n 3)))))))

(define (g n)
  (g-iter 2 1 0 n))

(define (g-iter a b c count)
  (if (= count 0)
      c
      (g-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

;;
;; Exercise 1.12
;;
(define (pascals-triangle n)
  (define (pascals-triangle-l x)
    (if (<= x 0)
	()
	(cons (pascals-number n x) (pascals-triangle-l (- x 1)))))
  (define (pascals-number level i)
    (cond ((= i 1) 1)
	  ((= i level) 1)
	  (else (let ((prev (- level 1)))
		  (+ (pascals-number prev i)
		     (pascals-number prev (- i 1)))))))
  (pascals-triangle-l n))

;; example to run => 
(map pascals-triangle '(1 2 3 4 5 6 7 8 9 10))


;;
;; Exercise 1.15
;;

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)
(p (sine (/ 12.15 3.0)))
(p (sine 4.05))
(p (p (sine (/ 4.05 3.0))))
(p (p (sine 1.35)))
(p (p (p (sine (/ 1.35 3.0)))))
(p (p (p (sine 0.45))))
(p (p (p (p (sine (/ 0.45 3.0))))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine (/ 0.15 3.0)))))))
(p (p (p (p (p (sine 0.05))))))
(p (p (p (p (p 0.05)))))
(p (p (p (p 0.14950000000000002))))
(p (p (p 0.43513455050000005)))
(p (p 0.9758465331678772))
(p -0.7895631144708228)
-0.39980345741334

;; Math.sin(12.15) #=> -0.40444382284914

;; answer for a: 5 times.
;; answer for b: Θ(n) in both space and steps
;;   => Θ(ceiling(a / 3.0) * 2) in steps
;;      Θ(ceiling(a / 3.0)) in space


;;
;; 1.2.4
;;
(define (square n) (* n n))
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))


;;
;; Exercise 1.16
;;
(define (xpt b n)
  (define (xpt-iter b n a)
    (cond ((= n 0) a)
	  ((even? n) (expt-iter (* b b) (/ n 2) a))
	  (else (expt-iter b (- n 1) (* a b)))))
  (xpt-iter b n 1))

(fast-expt 2 10)
(xpt 2 10)
;;          b  n    a
(xpt-iter   2 10    1)
(xpt-iter   4  5    1)
(xpt-iter   4  4    4)
(xpt-iter  16  2    4)
(xpt-iter 256  1    4)
(xpt-iter 256  0 1024)
1024

;;
;; Exercise 1.17
;;
(define (mul1 a b)
  (if (= b 0)
      0
      (+ a (mul1 a (- b 1)))))

(mul1 5 6)
(+ 5 (mul1 5 5))
(+ 5 (+ 5 (mul 5 4)))
(+ 5 (+ 5 (+ 5 (mul 5 3))))
(+ 5 (+ 5 (+ 5 (+ 5 (mul 5 2)))))
(+ 5 (+ 5 (+ 5 (+ 5 (+ 5 (mul 5 1))))))
(+ 5 (+ 5 (+ 5 (+ 5 (+ 5 (+ 5 (mul 5 0)))))))
(+ 5 (+ 5 (+ 5 (+ 5 (+ 5 (+ 5 0))))))
(+ 5 (+ 5 (+ 5 (+ 5 (+ 5 5)))))
(+ 5 (+ 5 (+ 5 (+ 5 10))))
(+ 5 (+ 5 (+ 5 15)))
(+ 5 (+ 5 20))
(+ 5 25)
30


(define (double n) (* n 2))
(define (halve  n) (/ n 2))

(define (mul a b)
  (cond ((= b 0) 0)
	((even? b) (double (mul a (halve b))))
	(else (+ a (mul a (- b 1))))))

(mul 9 12)
(double (mul 9 6))
(double (double (mul 9 3)))
(double (double (+ 9 (mul 9 2))))
(double (double (+ 9 (double (mul 9 1)))))
(double (double (+ 9 (double (+ 9 (mul 9 0))))))
(double (double (+ 9 (double (+ 9 0)))))
(double (double (+ 9 (double 9))))
(double (double (+ 9 18)))
(double (double 27))
(double 54)
108


;;
;; Exercise 1.18
;;
(define (double n) (* n 2))
(define (halve  n) (/ n 2))

(define (multiply a b)
  (define (multiply-iter a b s)
    (cond ((= b 0) s)
	  ((even? b) (multiply-iter (double a) (halve b) s))
	  (else (multiply-iter a (- b 1) (+ s a)))))
  (multiply-iter a b 0))

(multiply 96 65)
(* 96 65)

(multiply 9 12)
;;              a  b   s
(multiply-iter  9 12   0)
(multiply-iter 18  6   0)
(multiply-iter 36  3   0)
(multiply-iter 36  2  36)
(multiply-iter 72  1  36)
(multiply-iter 72  0 108)
108

;; => ab + s = 2a   b/2   + s
;;           =  a (b - 1) + s + a
;;           =  a    0    + s + ab


;;
;; Exercise 1.19
;;
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
;;  (print "(fib-iter " a " " b " " p " " q " " count ")")
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (* p p) (* q q))   ;; p' <- p^2 + q^2
		   (+ (* q q) (* 2 p q)) ;; q' <- q^2 + 2pq
		   (/ count 2)))
	(else (fib-iter (+         (* b q) (* a q) (* a p))
			(+ (* b p)         (* a q)        )
			p
			q
			(- count 1)))))

;;
;; 1.2.5
;;

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;
;; Exercise 1.20
;;
(gcd 206 40) ;; with applicative-order evaluation
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2

(gcd 206 40) ;; with normal-order evaluation
(gcd 40 (remainder 206 40))
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;; ....
;; => never converge


;;
;; 1.2.6
;;
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

(use srfi-27)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(expmod 12 13 13)
(remainder (* 12 (expmod 12 12 13)) 13)
(remainder (* 12 (remainder (square (expmod 12 6 13)) 13)) 13)
(remainder (* 12 (remainder (square (remainder (square (expmod 12 3 13)) 13)) 13)) 13)
(remainder (* 12 (remainder (square (remainder (square (remainder (* 12 (expmod 12 2 13)) 13)) 13)) 13)) 13)
(remainder (* 12 (remainder (square (remainder (square (remainder (* 12 (remainder (square (expmod 12 1 13)) 13)) 13)) 13)) 13)) 13)
(remainder (* 12 (remainder (square (remainder (square (remainder (* 12 (remainder (square (remainder (* 12 (expmod 12 0 13)) 13)) 13)) 13)) 13)) 13)) 13)
(remainder (* 12 (remainder (square (remainder (square (remainder (* 12 (remainder (square (remainder (* 12 1) 13)) 13)) 13)) 13)) 13)) 13)
(remainder (* 12 (remainder (square (remainder (square (remainder (* 12 (remainder (square (remainder 12 13)) 13)) 13)) 13)) 13)) 13)
(remainder (* 12 (remainder (square (remainder (square (remainder (* 12 (remainder (square 12) 13)) 13)) 13)) 13)) 13)
(remainder (* 12 (remainder (square (remainder (square (remainder (* 12 (remainder 144 13)) 13)) 13)) 13)) 13)
(remainder (* 12 (remainder (square (remainder (square (remainder (* 12 1) 13)) 13)) 13)) 13)
(remainder (* 12 (remainder (square (remainder (square (remainder 12 13)) 13)) 13)) 13)
(remainder (* 12 (remainder (square (remainder (square 12) 13)) 13)) 13)
(remainder (* 12 (remainder (square (remainder 144 13)) 13)) 13)
(remainder (* 12 (remainder (square 1) 13)) 13)
(remainder (* 12 (remainder 1 13)) 13)
(remainder (* 12 1) 13)
(remainder 12 13)
12



(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else #f)))

;;
;; Exercise 1.21
;;
(map smallest-divisor '(199 1999 19999))

;;
;; Exercise 1.22
;;
(define (runtime)
  (receive (sec nano-sec)
	   (sys-gettimeofday)
	   (+ (* sec 1000000) nano-sec)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
  (newline))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-time (- (runtime) start-time))))

(define (report-time elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (search-for-primes start n)
  (define (search-for-primes-iter i results)
;;    (print "(search-for-primes-iter " i " " results ")")
    (if (= (length results) n)
	results
	(let ((next (+ i 2)))
	  (if (prime? i)
	      (search-for-primes-iter next (append results (list i)))
	      (search-for-primes-iter next results)))))
  (search-for-primes-iter (if (even? start) (+ start 1) start) ()))

(define (timed-search-for-primes start n)
  (let ((start-time (runtime)))
    (display (search-for-primes start n))
    (newline)
    (display (- (runtime) start-time))
    (newline)))


(timed-search-for-primes     1000 3) ;; => (1009 1013 1019)              103
(timed-search-for-primes    10000 3) ;; => (10007 10009 10037)           351
(timed-search-for-primes   100000 3) ;; => (100003 100019 100043)        788
(timed-search-for-primes  1000000 3) ;; => (1000003 1000033 1000037)    1715
(timed-search-for-primes 10000000 3) ;; => (10000019 10000079 10000103) 6892
;;
;; results:
;;        10,000 =>    184
;;       100,000 =>    809
;;     1,000,000 =>  2,479
;;    10,000,000 =>  5,027
;;   100,000,000 => 12,650
;; 1,000,000,000 => 58,225
;;


;;
;; Exercise 1.23
;;
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

;; copied from 1.2.6
(define (divides? a b)
  (= (remainder b a) 0))

;; copied from 1.2.6
(define (prime? n)
  (= n (smallest-divisor n)))

;; copied from exercise 1.22
(define (search-for-primes start n)
  (define (search-for-primes-iter i results)
;;    (print "(search-for-primes-iter " i " " results ")")
    (if (= (length results) n)
	results
	(let ((next (+ i 2)))
	  (if (prime? i)
	      (search-for-primes-iter next (append results (list i)))
	      (search-for-primes-iter next results)))))
  (search-for-primes-iter (if (even? start) (+ start 1) start) ()))

;; copied from exercise 1.22
(define (timed-search-for-primes start n)
  (let ((start-time (runtime)))
    (display (search-for-primes start n))
    (newline)
    (display (- (runtime) start-time))
    (newline)))


(timed-search-for-primes     1000 3) ;; => (1009 1013 1019)              110
(timed-search-for-primes    10000 3) ;; => (10007 10009 10037)           294
(timed-search-for-primes   100000 3) ;; => (100003 100019 100043)        581
(timed-search-for-primes  1000000 3) ;; => (1000003 1000033 1000037)    1239
(timed-search-for-primes 10000000 3) ;; => (10000019 10000079 10000103) 4824

;; previous results
(timed-search-for-primes    10000 3) ;; => (10007 10009 10037)           351
(timed-search-for-primes   100000 3) ;; => (100003 100019 100043)        788
(timed-search-for-primes  1000000 3) ;; => (1000003 1000033 1000037)    1715
(timed-search-for-primes 10000000 3) ;; => (10000019 10000079 10000103) 6892


;;
;; Exercise 1.24
;;
(use srfi-27)

(define (runtime)
  (receive (sec nano-sec)
	   (sys-gettimeofday)
	   (+ (* sec 1000000) nano-sec)))

;; copied from exercise 1.22
(define (search-for-primes start n)
  (define (search-for-primes-iter i results)
;;    (print "(search-for-primes-iter " i " " results ")")
    (if (= (length results) n)
	results
	(let ((next (+ i 2)))
	  (if (prime? i)
	      (search-for-primes-iter next (append results (list i)))
	      (search-for-primes-iter next results)))))
  (search-for-primes-iter (if (even? start) (+ start 1) start) ()))

;; copied from exercise 1.22
(define (timed-search-for-primes start n)
  (let ((start-time (runtime)))
    (display (search-for-primes start n))
    (newline)
    (display (- (runtime) start-time))
    (newline)))


 (define (prime? n)
   (fast-prime? n 1))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else #f)))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(timed-prime-test 10007)
(timed-prime-test 10000019)



;;
;; Exercise 1.27
;;
(define (test-expmod n)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
	  ((even? exp)
	   (remainder (square (expmod base (/ exp 2) m))
		      m))
	  (else
	   (remainder (* base (expmod base (- exp 1) m))
		      m))))
  (define (test-all n a)
;;    (print "(expmod " a " " n " " n ") => "  (expmod a n n))
    (cond ((= a 0) #t)
	  ((= (expmod a n n) a)
	   (test-all n (- a 1)))
	  (else #f)))
  (test-all n (- n 1)))

(define *carmichael* '(561 1105 1729 2465 2821 6601))
(map prime? *carmichael*)      ;; => (#f #f #f #f #f #f)
(map test-expmod *carmichael*) ;; => (#t #t #t #t #t #t)



