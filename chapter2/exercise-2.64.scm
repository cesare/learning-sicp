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
;; 1) partial-tree splits elts into left sub-list, a center element, right sub-list.
;; 2) make-tree with the center element, left and right sub-lists.
;;    sub-lists are split into sub-sub-lists, recursively.
;;
;; for example,
;;    (partial-tree '(1 3 5 7 9 11) 6)
;; => (make-tree 5
;;               (partial-tree '(1 3) 2)    ;; left sub-list
;;               (partial-tree '(7 9 11) 3) ;; right sub-list
;; => (make-tree 5
;;               (make-tree 1
;;                          (partial-tree '() 0)
;;                          (partial-tree '(3) 1))
;;               (make-tree 9
;;                          (partial-tree '(7) 1)
;;                          (partial-tree '(11) 1)))
;; => (make-tree 5
;;               (make-tree 1
;;                          '()
;;                          (make-tree 3
;;                                     (partial-tree '() 0)
;;                                     (partial-tree '() 0)))
;;               (make-tree 9
;;                          (make-tree 7
;;                                     (partial-tree '() 0)
;;                                     (partial-tree '() 0))
;;                          (make-tree 11
;;                                     (partial-tree '() 0)
;;                                     (partial-tree '() 0)))))
;; => (make-tree 5
;;               (make-tree 1
;;                          '()
;;                          (make-tree 3
;;                                     '()
;;                                     '()))
;;               (make-tree 9
;;                          (make-tree 7
;;                                     '()
;;                                     '())
;;                          (make-tree 11
;;                                     '()
;;                                     '())))
;; => '(5 (1 () (3 () ())) (9 (6 () ()) (11 () ())))
;;

;;
;; b.
;;
;; Q. What is the order of growth?
;; A. Θ(n)
;;
