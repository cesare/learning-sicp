;;
;; Exercise-2.66
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; copied from section-2.3.3, p.156,157
;;
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((rec (entry set-of-records)))
        (let ((rkey (key rec)))
          (cond ((= rkey given-key) rec)
                ((> rkey given-key)
                 (lookup given-key (left-branch set-of-records)))
                ((< rkey given-key)
                 (lookup given-key (right-branch set-of-records))))))))


(define (key record) (car record))
(define (value record) (cdr record))
(define (make-record k v) (cons k v))


;;
;; testing
;;
(define testing-records
  (make-tree (make-record 3 'three)
             (make-tree (make-record 2 'two)
                        (make-tree (make-record 1 'one) '() '())
                        '())
             (make-tree (make-record 4 'four)
                        '()
                        (make-tree (make-record 5 'five) '() '()))))

(use gauche.test)
(test-start "lookup")
(test "0" #f (lambda () (lookup 0 testing-records)))
(test "1" '(1 . one)   (lambda () (lookup 1 testing-records)))
(test "3" '(3 . three) (lambda () (lookup 3 testing-records)))
(test "5" '(5 . five)  (lambda () (lookup 5 testing-records)))
(test "9" #f (lambda () (lookup 9 testing-records)))
(test-end)
