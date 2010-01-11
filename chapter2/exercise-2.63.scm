;; -*- mode: scheme; coding: utf-8 -*-

;;
;; Exercise-2.63
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

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; helpers
;;
(define nil '())

(define (create-tree lst)
  (fold (lambda (n tree) (adjoin-set n tree))
        (make-tree (car lst) nil nil)
        (cdr lst)))

(define sample-tree (create-tree '(5 1 4 2 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; a.
;;
;; Q. Do the two procedures produce the same result?
;;
;; A. Yes.
;;    Produced lists for the sets on Figure 2.16 are,
;;    (1 3 5 7 9 11)
;;

;;
;; b.
;;
;; Q. Do the two procedures produce the same order of growth?
;;
;; A. No.
;;    tree->list-1 costs Θ(n^2) because "append"ing costs Θ(n), whereas,
;;    tree->list-2 costs Θ(n).
;;    So tree->list-2 grows more slowly.
;;

