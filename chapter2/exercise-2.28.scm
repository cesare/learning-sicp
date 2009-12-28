;;
;; Exercise 2.28
;;
(define (fringe tree)
  (cond ((null? tree) tree)
	((pair? tree)
	 (append (fringe (car tree))
		 (fringe (cdr tree))))
	(else (list tree))))

;;
;; Testing
;;
(use gauche.test)
(test-start "fringe")
(test "()" '() (lambda () (fringe ())))
(test "1"  '(1) (lambda () (fringe 1)))
(test "(1 2 3 4)" '(1 2 3 4) (lambda () (fringe '(1 2 3 4))))
(test "((1 2) (3 4))" '(1 2 3 4) (lambda () (fringe '((1 2) (3 4)))))
(test "((1 2) (3 4 (5 6 (7 8) 9 10) 11) 12)"
      '(1 2 3 4 5 6 7 8 9 10 11 12)
      (lambda () (fringe '((1 2) (3 4 (5 6 (7 8) 9 10) 11) 12))))
(test "(1 2 () 3 4)" '(1 2 3 4) (lambda () (fringe '(1 2 () 3 4))))
(test-end)
