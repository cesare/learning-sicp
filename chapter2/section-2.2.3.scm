;;
;; Nested Mappings (p.122 - )
;;

(define nil ())

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


(define (enumerate-interval start end)
  (define (iter result minimum current)
    (if (< current minimum)
        result
        (cons current (iter result minimum (- current 1)))))
  (reverse (iter () start end)))

(enumerate-interval 1 10)

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



(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(permutations '(1 2 3))

