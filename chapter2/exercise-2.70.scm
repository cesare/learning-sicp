;;
;; Exercise-2.70
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; copied from section 2.3.4
;;
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; from exercise-2.68
;;
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (cond ((member? symbol (symbols (left-branch tree)))
             (cons 0 (encode-symbol symbol (left-branch tree))))
            ((member? symbol (symbols (right-branch tree)))
             (cons 1 (encode-symbol symbol (right-branch tree))))
            (else (error "unknown symbol -- ENCODE-SYMBOL" symbol)))))

(define (member? x lst)
  (cond ((null? lst) #f)
        ((eq? (car lst) x) #t)
        (else (member? x (cdr lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; from exercise-2.69
;;
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (= (length leaves) 1)
      (car leaves)
      (successive-merge
       (adjoin-set (make-code-tree (car leaves) (cadr leaves))
                   (cdr (cdr leaves))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define frequencies
  '((A    2)
    (BOOM 1)
    (GET  2)
    (JOB  2)
    (NA  16)
    (SHA  3)
    (YIP  9)
    (WAH  1)))

(define frequency-tree (generate-huffman-tree frequencies))

(define lyrics '((GET A JOB)
                 (SHA NA NA NA NA NA NA NA NA)
                 (GET A JOB)
                 (SHA NA NA NA NA NA NA NA NA)
                 (WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP)
                 (SHA BOOM)))

(encode (fold-right append () lyrics) frequency-tree)
;; => (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

(length (encode (fold-right append () lyrics) frequency-tree)) ;; => 84


;;
;; Q. How many bits are required for the encoding?
;; A. 84 bits.
;;
;; Q. What is the smallest number of bits that would be needed
;;    to encode this song if we used a fixed-length code for eight-symbol alphabet?
;; A. (lg 8) * 36 = 108 bits.
;;
