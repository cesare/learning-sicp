
(define (make-monitored f)
  (let ((count 0))
    (define (how-many-calls?)
      count)
    (define (reset-count)
      (set! count 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
            ((eq? m 'reset-count) (reset-count))
            (else (begin (set! count (+ count 1))
                         (f m)))))
    dispatch))


(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 'reset-count)
