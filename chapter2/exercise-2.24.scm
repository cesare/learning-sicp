;;
;; Exercise-2.24
;;

(list 1 (list 2 (list 3 4))) ;; => (1 (2 (3 4)))


(define nil ())

                                    (cons 4 nil)              ;; => (4)
                            (cons 3 (cons 4 nil))             ;; => (3 4)
                      (cons (cons 3 (cons 4 nil)) nil)        ;; => ((3 4))
              (cons 2 (cons (cons 3 (cons 4 nil)) nil))       ;; => (2 (3 4))
        (cons (cons 2 (cons (cons 3 (cons 4 nil)) nil)) nil)  ;; => ((2 (3 4)))
(cons 1 (cons (cons 2 (cons (cons 3 (cons 4 nil)) nil)) nil)) ;; => (1 (2 (3 4)))

