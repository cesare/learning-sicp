;;
;; Exercise-2.61
;;

(define (adjoin-set x set)
  (if (null? set) (list x)
      (let ((head (car set)))
        (cond ((= x head) set)
              ((< x head) (cons x set))
              (else (cons head (adjoin-set x (cdr set))))))))


;;
;; testing
;;
(use gauche.test)
(test-start "adjoin-set")
(test "3 (1 2 3 4 5)" '(1 2 3 4 5) (lambda () (adjoin-set 3 '(1 2 3 4 5))))
(test "3 (1 2   4 5)" '(1 2 3 4 5) (lambda () (adjoin-set 3 '(1 2 3 4 5))))
(test "0 (1 2 3 4 5)" '(0 1 2 3 4 5) (lambda () (adjoin-set 0 '(1 2 3 4 5))))
(test "6 (1 2 3 4 5)" '(1 2 3 4 5 6) (lambda () (adjoin-set 6 '(1 2 3 4 5))))
(test-end)
