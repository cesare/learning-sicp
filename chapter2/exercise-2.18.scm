;;
;; Exercise 2.18
;;
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


;;
;; Testing
;;
(use gauche.test)
(test-start "ex-reverse")
(test "()" '() (lambda () (ex-reverse ())))
(test "(1 4 9 16 25)" '(25 16 9 4 1) (lambda () (ex-reverse (list 1 4 9 16 25))))
(test-start "ex-reverse2")
(test "()" '() (lambda () (ex-reverse2 ())))
(test "(1 4 9 16 25)" '(25 16 9 4 1) (lambda () (ex-reverse2 (list 1 4 9 16 25))))
(test-end)
