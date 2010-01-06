;;
;; Exercise-2.56
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; copied from section-2.3.2
;;

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr  s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier   p) (cadr  p))
(define (multiplicand p) (caddr p))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (** base exponent) (expt base exponent))

(define (exponentiation? exp)
  (and (pair? exp) (= (length exp) 3) (eq? (car exp) '**)))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

(define (make-exponentiation base exponent)
  (cond ((and (number? base) (number? exponent)) (expt base exponent))
        ((=number? base 0) 0)
        ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
   (else (list '** base exponent))))



(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;;
;; testing
;;
(use gauche.test)
(test-start "**")
(test "(** 2 10)" 1024 (lambda () (** 2 10)))
(test "(** 3  4)"   81 (lambda () (** 3  4)))
(test-end)

(test-start "exponentiation?")
(test "(exponentiation? '(** 2 10))" #t (lambda () (exponentiation? '(** 2 10))))
(test "(exponentiation? '(*  2 10))" #f (lambda () (exponentiation? '(*  2 10))))
(test "(exponentiation? '(**))"          #f (lambda () (exponentiation? '(**))))
(test "(exponentiation? '(** 2))"        #f (lambda () (exponentiation? '(** 2))))
(test "(exponentiation? '(** 2 10 100))" #f (lambda () (exponentiation? '(** 2 10 100))))

(test-start "base")
(test "(base '(** 2 10))" 2 (lambda () (base '(** 2 10))))
(test-end)

(test-start "exponent")
(test "(exponent '(** 2 10))" 10 (lambda () (exponent '(** 2 10))))
(test-end)

(test-start "make-exponentiation")
(test "(make-exponentiation  0 10)"    0 (lambda () (make-exponentiation  0 10)))
(test "(make-exponentiation  1 10)"    1 (lambda () (make-exponentiation  1 10)))
(test "(make-exponentiation  2 10)" 1024 (lambda () (make-exponentiation  2 10)))
(test "(make-exponentiation 99  1)"   99 (lambda () (make-exponentiation 99  1)))
(test "(make-exponentiation 99  0)"    1 (lambda () (make-exponentiation 99  0)))
(test "(make-exponentiation  'x 'y)" '(** x y) (lambda () (make-exponentiation 'x 'y)))
(test-end)

(test-start "deriv")
(test "x^3" '(* 3 (** x 2)) (lambda () (deriv '(** x 3) 'x)))
(test "5 * x^3" '(* 5 (* 3 (** x 2))) (lambda () (deriv '(* 5 (** x 3)) 'x)))
(test "x^3 + (3 * x^2)"
      '(+ (* 3 (** x 2)) (* 3 (* 2 x)))
      (lambda () (deriv '(+ (** x 3) (* 3 (** x 2))) 'x)))
(test-end)
