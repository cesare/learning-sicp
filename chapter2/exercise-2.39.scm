;;
;; Exercise-2.39
;;

(define nil ())

;; from exercise-2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))



(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-l sequence)
  (fold-left  (lambda (x y) (cons y x)) nil sequence))


;;
;; testing
;;
(use gauche.test)
(test-start "reverse-r")
(test "()" '() (lambda () (reverse-r '())))
(test "(1)" '(1) (lambda () (reverse-r '(1))))
(test "(1 2 3 4 5)" '(5 4 3 2 1) (lambda () (reverse-r '(1 2 3 4 5))))
(test-end)

(test-start "reverse-l")
(test "()" '() (lambda () (reverse-l '())))
(test "(1)" '(1) (lambda () (reverse-l '(1))))
(test "(1 2 3 4 5)" '(5 4 3 2 1) (lambda () (reverse-l '(1 2 3 4 5))))
(test-end)
