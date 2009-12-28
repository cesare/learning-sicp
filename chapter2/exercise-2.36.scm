;;
;; Exercise-2.36
;;

(define nil ())

;; defined on p.116
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


;;
;; testing
;;
(use gauche.test)
(test-start "accumulate-n")
(test "(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))" '(22 26 30) (lambda () (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))))
(test-end)
