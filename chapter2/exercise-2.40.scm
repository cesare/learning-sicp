;;
;; Exercise-2.40
;;

(define (unique-pairs max)
  (flatmap (lambda (n)
             (map (lambda (m) (list n m))
                  (enumerate-interval 1 (- n 1))))
           (enumerate-interval 1 max)))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nil ())

;; defined in section 2.2.3
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (enumerate-interval start end)
  (define (iter result minimum current)
    (if (< current minimum)
        result
        (cons current (iter result minimum (- current 1)))))
  (reverse (iter () start end)))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (filter proc seq)
  (define (iter proc result seq)
    (if (null? seq)
        result
        (let ((x  (car seq))
              (xs (cdr seq)))
          (if (proc x)
              (cons x (iter proc result xs))
              (iter proc result xs)))))
  (iter proc () seq))

;; defined on p.116
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


;; from section-1.2.6
(define (square n) (* n n))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; testing
;;
(use gauche.test)
(test-start "unique-pairs")
(test "0" '() (lambda () (unique-pairs 0)))
(test "1" '() (lambda () (unique-pairs 1)))
(test "2" '((2 1)) (lambda () (unique-pairs 2)))
(test "3" '((2 1) (3 1) (3 2)) (lambda () (unique-pairs 3)))
(test "4" '((2 1) (3 1) (3 2) (4 1) (4 2) (4 3)) (lambda () (unique-pairs 4)))
(test-end)

(test-start "prime-sum-pairs")
(test "0" '() (lambda () (prime-sum-pairs 0)))
(test "1" '() (lambda () (prime-sum-pairs 1)))
(test "2" '((2 1 3)) (lambda () (prime-sum-pairs 2)))
(test "3" '((2 1 3) (3 2 5)) (lambda () (prime-sum-pairs 3)))
(test "4" '((2 1 3) (3 2 5) (4 1 5) (4 3 7)) (lambda () (prime-sum-pairs 4)))
(test "5" '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7)) (lambda () (prime-sum-pairs 5)))
(test-end)
