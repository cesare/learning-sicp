;;
;; Exercise 2.27
;;

;; original ones (see ex.2.18)
(define (ex-reverse lst)
  (if (null? lst)
      lst
      (append (ex-reverse (cdr lst))
	      (list (car lst)))))
(define (ex-reverse2 lst)
  (define (ex-reverse2-iter src rev)
    (if (null? src)
	rev
	(let ((current (list (car src))))
	  (ex-reverse2-iter (cdr src)
			    (append current rev)))))
  (ex-reverse2-iter lst ()))



(define (deep-reverse items)
  (if (null? items)
      items
      (let ((head (car items)))
	(append (deep-reverse (cdr items))
		(list (if (pair? head)
			  (deep-reverse head)
			  head))))))



;;
;; Testing
;;
(use gauche.test)
(test-start "deep-reverse")
(test "()" '() (lambda () (deep-reverse ())))
(test "(1 2 3 4)" '(4 3 2 1) (lambda () (deep-reverse '(1 2 3 4))))
(test "((1 2) (3 4))" '((4 3) (2 1)) (lambda () (deep-reverse '((1 2) (3 4)))))
(test-end)
