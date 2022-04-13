#|
Standard Unfancy Binary Tree
|#

(include "tree-api.ss")
#|
(k prefix get-root  get-key  get-value  get-parent  get-left  get-right  get-compare get-equal
          set-root! set-key! set-value! set-parent! set-left! set-right! tree-nil make-node)
|#
(define-tree-api "" tree-root node-key node-value node-parent node-left node-right tree-compare tree-equal
  tree-root-set! node-key-set! node-value-set! node-parent-set! node-left-set! node-right-set! nil
  (lambda (k v p) (make-node p nil nil k v)))

(define-record-type node
  (fields (mutable parent) (mutable left)
          (mutable right) (mutable key) (mutable value)))
(define-record-type tree
  (fields compare equal (mutable root)))
(define nil (make-node #f #f #f #f #f))
(define tree-search
  (lambda (T k)
    (let ([r (tree-search-internal T k)])
      (if r
          (node-key r)
          #f))))
(define tree-predecessor
  (lambda (T k)
    (let ([p (tree-predecessor-internal T k)])
      (if p
          (node-key p)
          #f))))
(define tree-successor
  (lambda (T k)
    (let ([s (tree-successor-internal T k)])
      (if s
          (node-key s)
          #f))))
(define tree-leftmost
  (lambda (T)
    (let ([r (tree-leftmost-internal T)])
      (if r
          (node-key r)
          #f))))
(define tree-rightmost
  (lambda (T)
    (let ([r (tree-rightmost-internal T)])
      (if r
          (node-key r)
          #f))))
(define tree-insert! tree-insert!-internal)
(define tree-remove!
  (lambda (T k)
    (let ([old (tree-remove!-internal T k)])
      #f)))
(define tree-print tree-print-internal)
(define tree-to-dot tree-to-dot-internal)

(printf "==========t1:~n")
(define t1 (make-tree < = '()))
(define insert!
  (lambda (T v)
    (tree-insert! T v v)))
(insert! t1 50)
(insert! t1 20)
(insert! t1 80)
(insert! t1 10)
(insert! t1 30)
(insert! t1 60)
(insert! t1 90)
;;(tree-insert! t1 50 50)
(tree-print t1)
(printf "removing 80~n")
(tree-remove! t1 80)
(tree-print t1)
(printf "removing 50~n")
(tree-remove! t1 50)
(tree-print t1)
(printf "removing 10~n")
(tree-remove! t1 10)
(tree-print t1)
(printf "removing 60~n")
(tree-remove! t1 60)
(tree-print t1)

(printf "==========t2 (one-sided):~n")
(define t2 (make-tree < = '()))
(do ([i 0 (add1 i)])
    ((= i 10))
  (insert! t2 i))
(tree-print t2)

(printf "==========t3 (one-sided):~n")
(define t3 (make-tree < = '()))
(do ([i 10 (sub1 i)])
    ((= i 0))
  (insert! t3 i))
(tree-print t3)
