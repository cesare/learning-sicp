;;
;; Exercise 2.20
;;
(define (same-parity head . lst)
  (define (same-parity-iter predicate src results)
    (if (null? src)
	results
	(let ((current (car src)))
	  (if (predicate current)
	    (same-parity-iter predicate
			      (cdr src)
			      (append results (list current)))
	    (same-parity-iter predicate
			      (cdr src)
			      results)))))
  (same-parity-iter (if (even? head) even? odd?) lst (list head)))

;;
;; Testing
;;
(use gauche.test)
(test-start "last-pair")
(test "(1 3 5 7)" '(1 3 5 7) (lambda () (same-parity 1 2 3 4 5 6 7)))
(test "(2 4 6)" '(2 4 6) (lambda () (same-parity  2 3 4 5 6 7)))
(test-end)
