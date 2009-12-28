;;
;; Exercise-2.22
;;

(define (square n) (* n n))
(define nil ())

;; first one
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items nil))


;; try
(square-list (list 1 2 3 4)) ;; => (16 9 4 1)


;; why
(square-list (list 1 2 3 4 ))
(iter '(1 2 3 4) ())  ;; 1st iteration
(iter (cdr '(1 2 3 4)) (cons (square (car '(1 2 3 4))) ()))
(iter '(2 3 4) (cons (square 1) ()))
(iter '(2 3 4) (cons 1 ()))
(iter '(2 3 4) '(1))   ;; 2nd iteration
(iter '(3 4) (cons 4 '(1)))
(iter '(3 4) '(4 1))   ;; 3rd iteration
(iter '(4) (cons 9 '(4 1)))
(iter '(4) '(9 4 1))   ;; 4th iteration
(iter () (cons 16 '(9 4 1)))
(iter () '(16 9 4 1))  ;; 5th iteration
'(16 9 4 1) ;; results


;; second one
(define (square-list items)
  (define (iter things answer)
    (print things ", " answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items nil))


;; try
(square-list (list 1 2 3 4)) ;; => ((((() . 1) . 4) . 9) . 16)

;; why
(square-list (list 1 2 3 4 ))
(iter '(1 2 3 4) ())  ;; 1st iteration
(iter (cdr '(1 2 3 4)) (cons () (square (car '(1 2 3 4)))))
(iter '(2 3 4) (cons () (square 1)))
(iter '(2 3 4) (cons () 1))
(iter '(2 3 4) '(() . 1)) ;; 2nd iteration
(iter (cdr '(2 3 4)) (cons '(() . 1) (square (car '(2 3 4)))))
(iter '(3 4) (cons '(() . 1) (square 2)))
(iter '(3 4) (cons '(() . 1) 4))
(iter '(3 4) '((() . 1) . 4))  ;; 3rd iteration
(iter (cdr '(3 4)) (cons '((() . 1) . 4) (square (car '(3 4)))))
(iter '(4) (cons '((() . 1) . 4) 9))
(iter '(4) '(((() . 1) . 4) . 9)) ;; 4th iteration
(iter '() '((((() . 1) . 4) . 9) . 16))  ;; 5th iteration
'((((() . 1) . 4) . 9) . 16) ;; results


;; solution 1
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append answer (list (square (car things)))))))
  (iter items nil))

;; try
(square-list (list 1 2 3 4)) ;; => (1 4 9 16)


;; solution 2
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (reverse (iter items nil)))

;; try
(square-list (list 1 2 3 4)) ;; => (1 4 9 16)

