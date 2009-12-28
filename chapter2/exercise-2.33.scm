;;
;; Exercise-2.33
;;

(define nil ())

;; defined on p.116
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))


;; map
(define (mymap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

;; append
(define (myappend seq1 seq2)
  (accumulate cons seq2 seq1))

;; length
(define (mylength sequence)
  (accumulate (lambda (c n) (+ n 1)) 0 sequence))


;;
;; testing
;;
(define (square n) (* n n))
(use gauche.test)
(test-start "mymap")
(test "(mymap square '(1 2 3))" '(1 4 9) (lambda () (mymap square '(1 2 3))))
(test-end)

(test-start "myappend")
(test "(myappend '(1 2 3) '(4 5 6))" '(1 2 3 4 5 6) (lambda () (myappend '(1 2 3) '(4 5 6))))
(test-end)

(test-start "mylength")
(test "(mylength '(1 2 3))" 3 (lambda () (mylength '(1 2 3))))
(test "(mylength '(1 2 3 4 5))" 5 (lambda () (mylength '(1 2 3 4 5))))
(test-end)
