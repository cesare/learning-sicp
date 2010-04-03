;;
;; Exercise-2.73
;;

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


;;
;; copied from section-2.3.2
;;
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define package (make-hash-table 'equal?))

(define (put op type item)
  (let ((pkg (if (hash-table-exists? package type)
                 (hash-table-get package type)
                 (make-hash-table))))
    (hash-table-put! pkg op item)
    (hash-table-put! package type pkg)))

(define (get op type)
  (let ((pkg (hash-table-get package type)))
    (hash-table-get pkg op)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-deriv)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
  (put 'deriv '+ make-sum))
