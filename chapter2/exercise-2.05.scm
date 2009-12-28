;;
;; Exercise-2.5
;;

(define (ab pair)
  (* (expt 2 (car pair)) (expt 3 (cdr pair))))

(ab (cons 10 20))
