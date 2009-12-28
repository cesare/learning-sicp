;;
;; Exercise-2.34
;;

;; defined on p.116
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
	      0
	      coefficient-sequence))


;;
;; testing
;;
(use gauche.test)
(test-start "horner-eval")
(test "(horner-eval 2 '(1 3 0 5 0 1))" 79 (lambda () (horner-eval 2 '(1 3 0 5 0 1))))
(test-end)
