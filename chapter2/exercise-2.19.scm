;;
;; Exercise 2.19
;;


;; original procedures (from 1.2.2, p.41)
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-conis)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-conis 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-conis 1))
		 (cc (- amount
			(first-denomination kinds-of-conis))
		     kinds-of-conis)))))

(define (first-denomination kinds-of-conis)
  (cond ((= kinds-of-conis 1) 1)
	((= kinds-of-conis 2) 5)
	((= kinds-of-conis 3) 10)
	((= kinds-of-conis 4) 25)
	((= kinds-of-conis 5) 50)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; converted
(define (cc amount coins)
  ;(print "(cc " amount " " coins ")")
  (cond ((= amount 0) 1)
	((or (< amount 0) (null? coin-values)) 0)
	(else (+ (cc amount
		     (cdr coins))
		 (cc (- amount
			(car coins))
		     coins)))))



(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else (+ (cc amount
		     (except-first-denomination coin-values))
		 (cc (- amount
			(first-denomination coin-values))
		     coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))


;;
;; Testing
;;
(use gauche.test)
(test-start "cc")
(test "(cc 100 us-coins)"    292 (lambda () (cc 100 us-coins)))
(test "(cc 100 uk-coins)" 104561 (lambda () (cc 100 uk-coins)))
(test-end)
