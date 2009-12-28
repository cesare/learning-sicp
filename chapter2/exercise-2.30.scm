;;
;; Exercise 2.30
;;

(define (square n) (* n n))

(define (square-tree tree)
  (cond ((null? tree) tree)
	((pair? tree) (cons (square-tree (car tree))
			    (square-tree (cdr tree))))
	(else (square tree))))


(define (square-tree2 tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree2 sub-tree)
	     (square sub-tree))) tree))


;;
;; testing
;;
(use gauche.test)
(test-start "square-tree")
(test "()" '() (lambda () (square-tree ())))
(test "(1 (2 (3 4) 5) (6 7))"
      '(1 (4 (9 16) 25) (36 49))
      (lambda () (square-tree '(1 (2 (3 4) 5) (6 7)))))
(test-end)

(test-start "square-tree2")
(test "()" '() (lambda () (square-tree2 ())))
(test "(1 (2 (3 4) 5) (6 7))"
      '(1 (4 (9 16) 25) (36 49))
      (lambda () (square-tree2 '(1 (2 (3 4) 5) (6 7)))))
(test-end)
