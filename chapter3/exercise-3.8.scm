;;
;; Exercise 3.8
;;

(define f
  (let ((x 0))
    (lambda (n)
      (let ((retval x))
        (set! x n)
        retval))))

;;
;; left -> right
;;
(f 0) ;; =>  0
(f 1) ;; =>  0
;;    (+ (f 0) (f 1))
;; => (+ 0     (f 1))
;; => (+ 0     0)
;; => 0



;;
;; left <- right
;;
(f 1) ;; => 0
(f 0) ;; => 1
;;    (+ (f 0) (f 1))
;; => (+ (f 0) 0)
;; => (+ 1     0)
;; => 1

