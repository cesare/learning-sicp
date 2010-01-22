;;
;; Exercise-2.58
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
;; a. enable infix form
;;

(define (sum? x)
  (and (pair? x) (eq? (car (cdr x)) '+)))

(define (addend s) (car  s))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))


(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier   p) (car  p))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; testing
;;
(use gauche.test)

(test-start "sum?")
(test "(+ 1 2)" #f (lambda () (sum? '(+ 1 2))))
(test "(1 + 2)" #t (lambda () (sum? '(1 + 2))))
(test-end)

(test-start "addend")
(test "(2 + 3)"  2 (lambda () (addend '(2 + 3))))
(test "(x + 3)" 'x (lambda () (addend '(x + 3))))
(test-end)

(test-start "augend")
(test "(2 + 3)"  3 (lambda () (augend '(2 + 3))))
(test "(2 + y)" 'y (lambda () (augend '(2 + y))))
(test-end)


(test-start "product?")
(test "(* 1 2)" #f (lambda () (product? '(* 1 2))))
(test "(1 * 2)" #t (lambda () (product? '(1 * 2))))
(test-end)

(test-start "multiplier")
(test "(2 * 3)"  2 (lambda () (multiplier '(2 * 3))))
(test "(x * 3)" 'x (lambda () (multiplier '(x * 3))))
(test-end)

(test-start "multiplicand")
(test "(2 * 3)"  3 (lambda () (multiplicand '(2 * 3))))
(test "(2 * y)" 'y (lambda () (multiplicand '(2 * y))))
(test-end)


(test-start "deriv")
(test "(x + 3)" 1 (lambda () (deriv '(x + 3) 'x)))
(test "((x * y) * (x + 3))" '((x * y) + (y * (x + 3))) (lambda () (deriv '((x * y) * (x + 3)) 'x)))
(test-end)
