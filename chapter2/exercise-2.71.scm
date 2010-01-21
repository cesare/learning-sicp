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
       (adjoin-set (make-code-tree (cadr leaves) (car leaves))
                   (cdr (cdr leaves))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; n = 5
;;
(define huffman-tree-2^5
  (generate-huffman-tree '((A 2) (B 4) (C 8) (D 16) (E 32))))

(map (lambda (s) (list s (encode (list s) huffman-tree-2^5))) '(A B C D E))
;; => ((A (1 1 1 1)) 
;;     (B (1 1 1 0))
;;     (C (1 1 0))
;;     (D (1 0))
;;     (E (0)))


;;
;; n = 10
;;
(define huffman-tree-2^10
  (generate-huffman-tree '((A 2) (B 4) (C 8) (D 16) (E 32) (F 64) (G 128) (H 256) (I 512) (J 1024))))

(map (lambda (s) (list s (encode (list s) huffman-tree-2^10))) '(A B C D E F G H I J))
;; => ((A (1 1 1 1 1 1 1 1 1))
;;     (B (1 1 1 1 1 1 1 1 0))
;;     (C (1 1 1 1 1 1 1 0))
;;     (D (1 1 1 1 1 1 0))
;;     (E (1 1 1 1 1 0))
;;     (F (1 1 1 1 0))
;;     (G (1 1 1 0))
;;     (H (1 1 0))
;;     (I (1 0))
;;     (J (0)))


;;
;; Q. How many bits are required to encode the most frequent symbol?
;; A. 1 bit.
;;
;; Q. the least frequent symbol?
;; A. (n - 1) bits.
;;

;;
;; building huffman-tree proceeds as follows,
;;
;; { (A 2) (B 4) (C 8) (D 16) (E 32) }
;; => { (AB 6) (C 8) (D 16) (E 32) }
;; => { (ABC 14) (D 16) (E 32) }
;; => { (ABCD 30) (E 32) }
;; => { (ABCDE 62) }
;;
;; 2^n (n > 1) > sum 2^i (i = 1 .. (n - 1))
;;
