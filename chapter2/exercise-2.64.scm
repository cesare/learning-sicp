;; -*- mode: scheme; coding: utf-8 -*-

;;
;; Exercise-2.64
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


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


;;
;; a.
;;
;; Q. How it works?
;;
;; A.
;; 1) partial-tree splits elts into left sub-tree, a center element, right sub-tree.
;; 2) make-tree with the center element, left and right sub-trees.
;;    sub-trees are split into sub-sub-trees, recursively.
;;
;; for example,
;;    (partial-tree '(1 2 3 4 5 6 7 8 9 10) 10)
;; => (make-tree 5
;;               (partial-tree '(1 2 3 4) 4) ;; left sub-tree
;;               (partial-tree '(6 7 8 9) 4) ;; right sub-tree
;; => (make-tree 5
;;               (make-tree 2
;;                          (partial-tree '(1) 1)
;;                          (partial-tree '(3 4) 2)
;;               (make-tree 7
;;                          (partial-tree '(6) 1)
;;                          (partial-tree '(8 9) 2))
;; => (make-tree 5
;;               (make-tree 2
;;                          (make-tree 1
;;                                     (partial-tree '() 0)
;;                                     (partial-tree '() 0))
;;                          (make-tree 3
;;                                     (partial-tree '() 0)
;;                                     (partial-tree '(4) 1))
;;               (make-tree 7
;;                          (make-tree 6
;;                                     (partial-tree '() 0)
;;                                     (partial-tree '() 0))
;;                          (make-tree 8
;;                                     (partial-tree '() 0)
;;                                     (partial-tree '(9) 1))
;; => (make-tree 5
;;               (make-tree 2
;;                          (make-tree 1
;;                                     '()
;;                                     '())
;;                          (make-tree 3
;;                                     '()
;;                                     (make-tree 4
;;                                                (partial-tree '() 0)
;;                                                (partial-tree '() 0))
;;               (make-tree 7
;;                          (make-tree 6
;;                                     '()
;;                                     '())
;;                          (make-tree 8
;;                                     '()
;;                                     (make-tree 9
;;                                                (partial-tree '() 0)
;;                                                (partial-tree '() 0))
;; => (make-tree 5
;;               (make-tree 2
;;                          (make-tree 1
;;                                     '()
;;                                     '())
;;                          (make-tree 3
;;                                     '()
;;                                     (make-tree 4
;;                                                '()
;;                                                '())
;;               (make-tree 7
;;                          (make-tree 6
;;                                     '()
;;                                     '())
;;                          (make-tree 8
;;                                     '()
;;                                     (make-tree 9
;;                                                '()
;;                                                '())
;;
;; => '(5 (2 (1 () ()) (3 () (4 () ()))) (8 (6 () (7 () ())) (9 () (10 () ()))))


;;
;; b.
;;
;; Q. What is the order of growth?
;; A. Θ(n)
;;
