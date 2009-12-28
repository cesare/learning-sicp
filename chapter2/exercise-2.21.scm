;;
;; Exercise 2.21
;;

(define (square-list1 items)
  (define (square n) (* n n))
  (if (null? items)
      ()
      (cons (square (car items))
	    (square-list1 (cdr items)))))

(define (square-list2 items)
  (map (lambda (n) (* n n)) items))


;;
;; Testing
;;
(use gauche.test)
(test-start "square-list1")
(test "(1 2 3 4)" '(1 4 9 16) (lambda () (square-list1 (list 1 2 3 4))))
(test-end)
(test-start "square-list2")
(test "(1 2 3 4)" '(1 4 9 16) (lambda () (square-list2 (list 1 2 3 4))))
(test-end)
