;;
;; Exercise-2.6
;;

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))


;; one, two in terms of add-1
(define one-2 (add-1 zero))
(define two-2 (add-1 one-2))


;; sample functions
(define (1+ n) (+ n 1))
(1+ 99) ;; => 100

(define (double n) (* n 2))
(double 99) ;; => 198



(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f (lambda (y) y) x)))


((zero (lambda (n) (+ n 1))) 999) ;; =>  999
((one  (lambda (n) (+ n 1))) 999) ;; => 1000
((two  (lambda (n) (+ n 1))) 999) ;; => 1001

((zero (lambda (n) (* n 2))) 99)  ;; =>  99
((one  (lambda (n) (* n 2))) 99)  ;; => 198
((two  (lambda (n) (* n 2))) 99)  ;; => 396



((one-2 1+) 99)     ;; => 100
((one-2 double) 99) ;; => 198

((two-2 1+) 99)     ;; => 101
((two-2 double) 99) ;; => 398

