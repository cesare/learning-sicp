;;
;; Exercise 2.1
;;
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (positive? d)
	(cons (/ n g) (/ d g))
	(cons (/ (- n) g) (/ (- d) g)))))

;; revised version
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (let ((g2 (if (positive? d) g (- g))))
      (cons (/ n g2) (/ d g2)))))

;; another way
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (set! g (if (positive? d) g (- g)))
    (cons (/ n g) (/ d g))))

;;
;; Testing
;;
(use gauche.test)
(test-start "make-rat")
(test " 2 /  4" '( 1 . 2) (lambda () (make-rat  2  4)))
(test "-2 / -4" '( 1 . 2) (lambda () (make-rat -2 -4)))
(test "-2 /  4" '(-1 . 2) (lambda () (make-rat -2  4)))
(test " 2 / -4" '(-1 . 2) (lambda () (make-rat  2 -4)))
(test-end)


