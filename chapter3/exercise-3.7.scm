;;
;; Exercise 3.7
;;

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (reject any)
    "Incorrect password")
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        reject))
  dispatch)


(define (make-joint acc password new-password)
  (define (dispatch p m)
    (if (eq? p new-password)
        (acc password m)
        (lambda (any) "Incorrect password!")))
  dispatch)


(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 15) ;; =>  85
((paul-acc  'rosebud     'withdraw) 10) ;; =>  75

((peter-acc 'open-sesame 'deposit) 20)  ;; =>  95
((paul-acc  'rosebud     'deposit) 10)  ;; => 105

((paul-acc  'fake-password 'withdraw) 105) ;; => "Incorrect password!"

