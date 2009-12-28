;;
;; Exercise-2.35
;;

(define nil ())

;; defined on p.116
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;; defined on p.116
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))


;; use enumerate-tree
(define (count-leaves t)
  (accumulate (lambda (c a) (+ a 1)) 0 (map identity (enumerate-tree t))))

;; above one is equivalent to,
(define (count-leaves t)
  (accumulate (lambda (c a) (+ a 1)) 0 (enumerate-tree t)))


;; this is it
(define (count-leaves t)
  (accumulate (lambda (c a) (+ a (if (pair? c) (count-leaves c) c)))
	      0
	      (map (lambda (n) (if (pair? n) n 1)) t)))

;;
;; testing
;;
(use gauche.test)
(test-start "count-leaves")
(test "(count-leaves '(1 2 (3 4 5) 6))" 6 (lambda () (count-leaves '(1 2 (3 4 5) 6))))
(test-end)
