;;
;; Exercise 2.29
;;

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))


;; a

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))


;; try a
(left-branch  (make-branch 1 2)) ;; => 1
(right-branch (make-branch 1 2)) ;; => 2

(branch-length    (make-branch 10 '(dummy-structure))) ;; => 10
(branch-structure (make-branch 10 '(dummy-structure))) ;; => (dummy-structure)


;; b

;; define pseudo-predicator
(define (mobile? object)
  (pair? object))
(define (branch? object)
  (pair? object))

;; additional selectors
(define (weight branch)
  (car branch))
(define (structure branch)
  (car (cdr branch)))


(define (total-weight-of-branch branch)
  (fold +
	(weight branch)
	(map (lambda (element)
	       (if (branch? element)
		   (total-weight-of-branch element)
		   element))
	     (structure branch))))

(define (total-weight mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (if (branch? mobile)
	(+ (total-weight-of-branch left) (total-weight-of-branch right)))))


;; try
(total-weight-of-branch (make-branch 10 '(1 2 3))) ;; => 16
(total-weight-of-branch (make-branch 20 '(9 8 7))) ;; => 44

(total-weight (make-mobile (make-branch 10 '(1 2 3)) (make-branch 20 '(9 8 7)))) ;; => 60


(total-weight (make-mobile (make-branch 10 (list  1  2 (make-branch 20 '(30 40))))
			   (make-branch 11 (list 12 13 (make-branch 14 '(15 16))))))
;; => 184  <- (+ 10 1 2 20 30 40 11 12 13 14 15 16)



;; c

(define (balanced? mobile)
  (= (total-weight-of-branch (left-branch mobile))
     (total-weight-of-branch (right-branch mobile))))


;; try

(balanced? (make-mobile (make-branch 10 (list  1  2 (make-branch 20 '(30 40))))
			(make-branch 11 (list 12 13 (make-branch 14 '(15 16)))))) ;; => #f

(balanced? (make-mobile (make-branch 10 (list  1  2 (make-branch 20 '(30 40))))
			(make-branch 40 (list 30 20 (make-branch  2 '( 1 10)))))) ;; => #t


;; d

just rewrite selectors.

