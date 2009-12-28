;;
;; Exercise-2.
;;
(define (ordered-doubles n)
  (flatmap (lambda (a) (map (lambda (b) (list a b)) (enumerate-interval (+ a 1) n)))
       (enumerate-interval 1 n)))


(define (ordered-triples n)
  (flatmap (lambda (a)
             (flatmap (lambda (b)
                        (map (lambda (c) (list a b c))
                             (enumerate-interval (+ b 1) n)))
                      (enumerate-interval (+ a 1) n)))
           (enumerate-interval 1 n)))


(define (ordered-triples-to-sum n s)
  (filter (lambda (triple) (= (accumulate + 0 triple) s)) (ordered-triples n)))


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



;;
;; testing
;;
(use gauche.test)
(test-start "ordered-doubles")
(test "2" '((1 2)) (lambda () (ordered-doubles 2)))
(test "3" '((1 2) (1 3) (2 3)) (lambda () (ordered-doubles 3)))
(test "4" '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)) (lambda () (ordered-doubles 4)))

(test-start "ordered-triples")
(test "3" '((1 2 3)) (lambda () (ordered-triples 3)))
(test "4" '((1 2 3) (1 2 4) (1 3 4) (2 3 4)) (lambda () (ordered-triples 4)))
(test "5" '((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5) (2 3 4) (2 3 5) (2 4 5) (3 4 5)) (lambda () (ordered-triples 5)))
(test-end)
