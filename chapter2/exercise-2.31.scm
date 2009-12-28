;;
;; Exercise 2.31
;;

(define (square n) (* n n))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map proc sub-tree)
	     (proc sub-tree))) tree))


(define (square-tree tree) (tree-map square tree))

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
