;;
;; Exercise-2.55
;;

(car ''abracadabra)

; is equivalent to
(car '(quote abracadabra))

; which is equivalent to
(car (quote (quote abracadabra)))

; is equals to quote.

