;;
;; Exercise-2.23
;;

(define (foreach proc items)
  (if (null? items)
      #t
      ((lambda ()
	(proc (car items))
	(foreach proc (cdr items))))))


;; try
(foreach (lambda (n) (print n " => " (* n n))) '(1 2 3 4 5))
;; 1 => 1
;; 2 => 4
;; 3 => 9
;; 4 => 16
;; 5 => 25
;; #t



;; Scheme (or only Gauche?) doesn't have progn,
;; so we should use lambda to bind multiple procedure callings.

