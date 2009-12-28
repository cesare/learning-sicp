;;
;; Exercise 2.32
;;

(define nil ())

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (e) (cons (car s) e))
			  rest)))))


;;
;; testing
;;
(use gauche.test)
(test-start "subsets")
(test "()" '(()) (lambda () (subsets ())))
(test "(1 2 3)" '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)) (lambda () (subsets '(1 2 3))))
(test-end)

;;
;; analysis
;;
; (subsets '(1 2 3))
; (subsets '(2 3))
; (subsets '(3))
; (subsets '())
; (append '(()) (map (lambda (e) (cons (car '(3)) e)) '(())))
; (append '(()) (map (lambda (e) (cons 3 e)) '(())))
; (append '(()) '((3)))
; '(() (3))
; (append '(() (3)) (map (lambda (e) (cons (car '(2 3)) e)) '(() (3)))
; (append '(() (3)) (map (lambda (e) (cons 2 e)) '(() (3)))
; (append '(() (3)) '((2) (2 3)))
; (() (3) (2) (2 3))
; (append '(() (3) (2) (2 3)) (map (lambda (e) (cons (car '(1 2 3)) e)) '(() (3) (2) (2 3))))
; (append '(() (3) (2) (2 3)) (map (lambda (e) (cons 1 e)) '(() (3) (2) (2 3))))
; (append '(() (3) (2) (2 3)) '((1) (1 3) (1 2) (1 2 3)))
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
