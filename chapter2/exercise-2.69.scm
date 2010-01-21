;;
;; Exercise-2.69
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

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; this one is wrong.
(define (successive-merge leaves)
  (if (= (length leaves) 1)
      (car leaves)
      (successive-merge
       (cons (make-code-tree (cadr leaves) (car leaves))
             (cdr (cdr leaves))))))

;; 2nd try.
(define (successive-merge leaves)
  (if (= (length leaves) 1)
      (car leaves)
      (successive-merge
       (adjoin-set (make-code-tree (car leaves) (cadr leaves))
                   (cdr (cdr leaves))))))

;;
;; testing
;;
(use gauche.test)
(test-start "generate-huffman-tree")
(test "A4 B2 C1 D1"
      '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
      (lambda () (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))))
(test-end)


;;
;;    { (f 5) (e 9) (c 12) (b 13) (d 16) (a 45) }
;; => { (fe 14) (c 12) (b 13) (d 16) (a 45) } ;; merge (f 5) and (e 9) => (fe 14)
;; => { (c 12) (b 13) (fe 14) (d 16) (a 45) } ;; sort
;; => { (cb 25) (fe 14) (d 16) (a 45) }
;; => { (fe 14) (d 16) (cb 25) (a 45) }
;; => { (fed 30) (cb 25) (a 45) }
;; => { (cb 25) (fed 30) (a 45) }
;; => { (cbfed 55) (a 45) }
;; => { (a 45) (cbfed 55) }
;; => { (acbfed 100) }
;;
