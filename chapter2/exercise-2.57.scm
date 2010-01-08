;;
;; Exercise-2.57
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

;;
;; fix augend and multiplicand,
;; in order to handle sums and products of arbitrary numbers
;;

(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (cons (car s) (cdr (cdr s)))))

(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (cons (car p) (cdr (cdr p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; testing
;;
(use gauche.test)

(test-start "augend")
(test "(+ 1 2)"   2        (lambda () (augend '(+ 1 2))))
(test "(+ 1 2 3)" '(+ 2 3) (lambda () (augend '(+ 1 2 3))))
(test-end)

(test-start "multiplicand")
(test "(* 1 2)"   2        (lambda () (multiplicand '(* 1 2))))
(test "(* 1 2 3)" '(* 2 3) (lambda () (multiplicand '(* 1 2 3))))
(test-end)


(test-start "deriv")
(test "(+ x 3)"   1 (lambda () (deriv '(+ x 3)   'x)))
(test "(+ x 3 5)" 1 (lambda () (deriv '(+ x 3 5) 'x)))
(test "(* x y (+ x 3))" '(+ (* x y) (* y (+ x 3))) (lambda () (deriv '(* (* x y) (+ x 3)) 'x)))
(test "(* x y (+ x 3))" '(+ (* x y) (* y (+ x 3))) (lambda () (deriv '(* x y (+ x 3)) 'x)))
(test-end)
