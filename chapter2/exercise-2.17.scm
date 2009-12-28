;;
;; Exercise 2.17
;;

(define (last-pair lst)
  (let ((tail (cdr lst)))
    (if (null? tail)
        (list (car lst))
        (last-pair tail))))

(define (last-pair lst)
  (let ((tail (cdr lst)))
    (if (null? tail)
        lst
        (last-pair tail))))
;;
;; Testing
;;
(use gauche.test)
(test-start "last-pair")
(test "(34)" '(34) (lambda () (last-pair (list 23 72 149 34))))
(test-end)
