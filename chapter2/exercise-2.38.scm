;;
;; Exercise-2.38
;;

(define nil ())

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 '(1 2 3))       ;; => 3/2
(fold-left  / 1 '(1 2 3))       ;; => 1/6
(fold-right list nil '(1 2 3))  ;; => (1 (2 (3 ())))
(fold-left  list nil '(1 2 3))  ;; => (((() 1) 2) 3)

(fold-right + 0 '(1 2 3 4 5))   ;; => 15
(fold-left  + 0 '(1 2 3 4 5))   ;; => 15

(fold-right * 1 '(1 2 3 4 5))   ;; => 120
(fold-left  * 1 '(1 2 3 4 5))   ;; => 120


;; property that op should satisfy:
;;   (1) op takes exactly two arguments
;;   (2) for all a, b, (op a b) = (op b a)
;;

;;
;; testing
;;
(use gauche.test)
(test-start "fold-left")
(test-end)
