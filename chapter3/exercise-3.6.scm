;;
;; Exercise 3.6
;;

;; fake for testing behaviour
(define random-init 0)
(define (rand-update x)
  (+ x 1))


(define rand
  ((lambda ()
     (let ((x random-init))
       (define (generate)
         (set! x (rand-update x))
         x)
       (define (reset new-init)
         (set! x new-init))
       (define (dispatch m)
         (cond ((eq? m 'generate)
                (generate))
               ((eq? m 'reset)
                reset)
               (else (error "Unknown request -- MAKE-RAND" m))))
       dispatch))))


(rand 'generate) ;; => 1
(rand 'generate) ;; => 2
(rand 'generate) ;; => 3
(rand 'generate) ;; => 4

((rand 'reset) 99)
(rand 'generate) ;; => 100
