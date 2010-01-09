;;
;; Exercise-2.62
;;

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                 ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))


;;
;; testing
;;
(use gauche.test)
(test-start "union-set")
(test "(1 2 3) ()"      '(1 2 3)       (lambda () (union-set '(1 2 3) '()     )))
(test "() (4 5 6)"      '(4 5 6)       (lambda () (union-set '()      '(4 5 6))))
(test "(1 2 3) (4 5 6)" '(1 2 3 4 5 6) (lambda () (union-set '(1 2 3) '(4 5 6))))
(test "(1 2 3) (2 3 4)" '(1 2 3 4)     (lambda () (union-set '(1 2 3) '(2 3 4))))
(test "(1 2 3) (1 2 3)" '(1 2 3)       (lambda () (union-set '(1 2 3) '(1 2 3))))
(test "(1 2 3) (3)"     '(1 2 3)       (lambda () (union-set '(1 2 3) '(3)    )))
(test-end)
