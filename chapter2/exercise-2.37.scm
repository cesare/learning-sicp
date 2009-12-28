;;
;; Exercise-2.37
;;

(define nil ())

;; defined on p.116
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; from exercise-2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))




(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))


(define (transpose m)
  (accumulate-n cons () m))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))



;;
;; testing
;;
(use gauche.test)
(test-start "dot-product")
(test "(dot-product '(1 2 3) '(4 5 6))" 32 (lambda () (dot-product '(1 2 3) '(4 5 6))))
(test-end)

(test-start "matrix-*-vector")
(test "(matrix-*-vector '((1 2 3) '(4 5 6)) '(7 8 9))" '(50 122) (lambda () (matrix-*-vector '((1 2 3) (4 5 6)) '(7 8 9))))
(test-end)

(test-start "transpose")
(test "(transpose '((1 2) (3 4)))" '((1 3) (2 4)) (lambda () (transpose '((1 2) (3 4)))))
(test-end)

(test-start "matrix-*-matrix")
(test "(matrix-*-matrix '((1 2) (3 4)) '((5 6) (7 8)))" '((19 22) (43 50)) (lambda () (matrix-*-matrix '((1 2) (3 4)) '((5 6) (7 8)))))
(test-end)
