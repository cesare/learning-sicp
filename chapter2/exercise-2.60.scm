;;
;; Exercise-2.60
;;


(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))


(define (remove-duplicates set)
  (define (iter src dst)
    (if (null? src) dst
        (let ((head (car src))
              (tail (cdr src)))
          (if (element-of-set? head dst)
              (iter tail dst)
              (iter tail (cons head dst))))))
  (iter set '()))


(define (adjoin-set x set)
  (define (adjoin x set)
    (if (element-of-set? x set)
        set
        (cons x set)))
  (adjoin x (remove-duplicates set)))


(define (intersection-set set1 set2)
  (define (intersect s1 s2)
    (cond ((or (null? s1) (null? s2)) '())
          ((element-of-set? (car s1) s2)
           (cons (car s1) (intersect (cdr s1) s2)))
          (else (intersect (cdr s1) s2))))
  (intersect (remove-duplicates set1) (remove-duplicates set2)))


(define (union-set set1 set2)
  (define (unite s1 s2)
    (cond ((null? s1) s2)
        ((null? s2) s1)
        ((element-of-set? (car s1) s2)
         (unite (cdr s1) s2))
        (else (cons (car s1) (unite (cdr s1) s2)))))
  (unite (remove-duplicates set1) (remove-duplicates set2)))


;;
;; testing
;;
(use gauche.test)

(test-start "element-of-set?")
(test "1 '(2 3 2 1 3 2 2)" #t (lambda () (element-of-set? 1 '(2 3 2 1 3 2 2))))
(test "4 '(2 3 2 1 3 2 2)" #f (lambda () (element-of-set? 4 '(2 3 2 1 3 2 2))))
(test-end)

(test-start "remove-duplicates")
(test "()"          '()        (lambda () (sort (remove-duplicates '()         ))))
(test "(1 2 3)"     '(1 2 3)   (lambda () (sort (remove-duplicates '(1 2 3)    ))))
(test "(1 2 3 4 1)" '(1 2 3 4) (lambda () (sort (remove-duplicates '(1 2 3 4 1)))))
(test "(1 2 1 2 1)" '(1 2)     (lambda () (sort (remove-duplicates '(1 2 1 2 1)))))
(test "(1 1 1 1 1)" '(1)       (lambda () (sort (remove-duplicates '(1 1 1 1 1)))))
(test-end)

(test-start "adjoin-set")
(test "1 '(2 3 2 1 3 2 2)" '(1 2 3)   (lambda () (sort (adjoin-set 1 '(2 3 2 1 3 2 2)))))
(test "4 '(2 3 2 1 3 2 2)" '(1 2 3 4) (lambda () (sort (adjoin-set 4 '(2 3 2 1 3 2 2)))))
(test-end)

(test-start "intersection-set")
(test "(1 2)   (4 5 6 4)" '()    (lambda () (sort (intersection-set '(1 2)   '(4 5 6 4)))))
(test "(1 2)   (2 1 2 3)" '(1 2) (lambda () (sort (intersection-set '(1 2)   '(2 1 2 3)))))
(test "(1 2 1) (2 1 2 3)" '(1 2) (lambda () (sort (intersection-set '(1 2 1) '(2 1 2 3)))))
(test-end)

(test-start "union-set")
(test "(1 2 1) ()"      '(1 2)     (lambda () (sort (union-set '(1 2 1) '()     ))))
(test "() (3 4 3)"      '(3 4)     (lambda () (sort (union-set '()      '(3 4 3)))))
(test "(1 2 1) (3 4 3)" '(1 2 3 4) (lambda () (sort (union-set '(1 2 1) '(3 4 3)))))
(test-end)
