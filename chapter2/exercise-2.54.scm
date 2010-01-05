;;
;; Exercise-2.54
;;

(equal? '(this is a list)   '(this is a list))   ;; => true
(equal? '(this is a list)   '(this (is a) list)) ;; => false

(eq? '(this is a list)   '(this is a list))   ;; => false
(eq? '(this is a list)   '(this (is a) list)) ;; => false


(define (my-equal? a b)
  (if (pair? a)
      (if (pair? b)  ;; if pair?(a) and pair?(b)
          (and (my-equal? (car a) (car b))
               (my-equal? (cdr a) (cdr b)))
          #f)        ;; pair?(a) but !pair?(b)
      (eq? a b)))    ;; !pair?(a) and !pair?(b)



;;
;; testing
;;
(use gauche.test)
(test-start "my-equal?")
(test "a a"                     #t (lambda () (my-equal? 'a 'a)))
(test "a b"                     #f (lambda () (my-equal? 'a 'b)))
(test "nil nil"                 #t (lambda () (my-equal? () ())))
(test "(a) (a)"                 #t (lambda () (my-equal? '(a) '(a))))
(test "(a b) (a b)"             #t (lambda () (my-equal? '(a b) '(a b))))
(test "(a b c) (a b)"           #f (lambda () (my-equal? '(a b c) '(a b))))
(test "(a b) (a b c)"           #f (lambda () (my-equal? '(a b) '(a b c))))
(test "(a b) (a c)"             #f (lambda () (my-equal? '(a b) '(a c))))
(test "(a c) (a b)"             #f (lambda () (my-equal? '(a c) '(a b))))
(test "(a b c) (a b c)"         #t (lambda () (my-equal? '(a b c) '(a b c))))
(test "(a b (c)) (a b c)"       #f (lambda () (my-equal? '(a b (c)) '(a b c))))
(test "(a b (c)) (a b (c))"     #t (lambda () (my-equal? '(a b (c)) '(a b (c)))))
(test "(a b (c d)) (a b (c d))" #t (lambda () (my-equal? '(a b (c d)) '(a b (c d)))))
(test-end)
