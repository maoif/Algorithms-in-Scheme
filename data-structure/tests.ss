(define sorted?
  (lambda (pred v)
    (call/cc (lambda (k)
               (do ([i 0 (add1 i)])
                   ((= i (sub1 (vector-length v))) #t)
                 (unless (pred (vector-ref v i) (vector-ref v (add1 i)))
                   (k #f)))))))

(define random-vector
  (lambda (size range)
    (let ([v (make-vector size)])
      (do ([i 0 (add1 i)])
          ((= i (vector-length v)))
        (vector-set! v i (random range)))
      v)))

(include "../mics/shuffle.ss")

;; stack.ss
(define test-stack
  (lambda ()
    (define s1 (make-stack (string->list "Scheme is a beautiful language.") char=?))
    (define s2 (make-stack))
    (printf "s1: ~a~n" (stack-pop!-until s1 #\x))

    (printf "s2: ~a~n")
    (stack-push! s2 7)
    (stack-push! s2 4)
    (stack-push! s2 8)
    (stack-push! s2 0)
    (stack-push! s2 2)
    (stack-push! s2 3)
    (stack-pop!-unti s2 4)
    (printf "s2: ~a~n")
    ))

;; heap.ss
(define test-heap
  (lambda ()
    (include "heap.ss")
    (define v1 (random-vector 10 100))
    (define v2 (random-vector 999 500))
    (define v3 (random-vector 99999 1000))

    (define l1 (vector->list (random-vector 10 100)))
    (define l2 (vector->list (random-vector 999 500)))
    (define l3 (vector->list (random-vector 99999 1000)))

    (define h1 (make-heap > 50))

    (define hv1 (time (make-heap > v1)))
    (define hv2 (time (make-heap > v2)))
    (define hv3 (time (make-heap > v3)))
    (define hl1 (time (make-heap > l1)))
    (define hl2 (time (make-heap > l2)))
    (define hl3 (time (make-heap > l3)))
    (printf "hv1 size: ~a load: ~a~n" (heap-size hv1) (heap-load hv1))
    (printf "hv2 size: ~a load: ~a~n" (heap-size hv2) (heap-load hv2))
    (printf "hv3 size: ~a load: ~a~n" (heap-size hv3) (heap-load hv3))

    (heap-print hv1)

    (do ([i 0 (add1 i)])
        ((= i 31))
      (heap-insert! h1 i))
    (heap-print h1)
    (printf "size: ~a load: ~a~n" (heap-size h1) (heap-load h1))


    (printf "hv1 sorted: ~a~n" (sorted? >= (do ([i 0 (add1 i)]
                                                [v (make-vector (heap-load hv1))]
                                                [load (heap-load hv1)])
                                               ((= i load) v)
                                             (vector-set! v i (heap-extract-top! hv1)))))

    (printf "hv2 sorted: ~a~n" (sorted? >= (do ([i 0 (add1 i)]
                                                [v (make-vector (heap-load hv2))]
                                                [load (heap-load hv2)])
                                               ((= i load) v)
                                             (vector-set! v i (heap-extract-top! hv2)))))


    (printf "hv3 sorted: ~a~n" (sorted? >= (do ([i 0 (add1 i)]
                                                [v (make-vector (heap-load hv3))]
                                                [load (heap-load hv3)])
                                               ((= i load) v)
                                             (vector-set! v i (heap-extract-top! hv3)))))))

;; dset.ss
(define test-dset
  (lambda ()
    (include "dset.ss")
    (define l1 '(1 2 3 4))
    (define l2 '#(0 9 3 7 2 5 6))

    (define s1 (make-dset l1))
    (define s2 (make-dset l2))

    (printf "dset1: ~a~n" (hashtable-cells s1))
    (printf "dset2: ~a~n" (hashtable-cells s2))
    (dset-union! s1 1 2)
    (printf "dset1: ~a~n" (hashtable-cells s1))
    (dset-union! s1 4 2)
    (printf "dset1: ~a~n" (hashtable-cells s1))
    (dset-union! s1 1 3)
    (printf "dset1: ~a~n" (hashtable-cells s1))
    (dset-union! s1 3 4)
    (printf "dset1: ~a~n" (hashtable-cells s1))

    (printf "0 and 9: ~a~n" (dset=? s2 (dset-find s2 0) (dset-find s2 9)))
    (dset-union! s2 0 9)
    (printf "0 and 9: ~a~n" (dset=? s2 (dset-find s2 0) (dset-find s2 9)))

    (printf "5 and 7: ~a~n" (dset=? s2 (dset-find s2 5) (dset-find s2 7)))
    (dset-union! s2 5 7)
    (printf "5 and 7: ~a~n" (dset=? s2 (dset-find s2 5) (dset-find s2 7)))

    (printf "2 and 7: ~a~n" (dset=? s2 (dset-find s2 2) (dset-find s2 7)))
    (dset-union! s2 2 7)
    (printf "2 and 7: ~a~n" (dset=? s2 (dset-find s2 2) (dset-find s2 7)))

    (printf "5 and 2: ~a~n" (dset=? s2 (dset-find s2 5) (dset-find s2 2)))
    ))

;; AVL.ss
(define test-avltree
  (lambda ()
    (include "AVL.ss")
    (define insert!
      (lambda (T v)
        (avltree-insert! T v v)))

    (let ()
      (define t1 (make-avltree < = '()))
      (printf "==========t1 (insertion):~n")
      (insert! t1 50)
      (insert! t1 20)
      (insert! t1 80)
      (insert! t1 10)
      (insert! t1 30)
      (insert! t1 60)
      (insert! t1 90)
      ;;(tree-insert! t1 50 50)
      (avltree-print t1)
      (insert! t1 5)
      (avltree-print t1)
      (insert! t1 3)
      (avltree-print t1)
      (insert! t1 11)
      (avltree-print t1)
      )

    (let ()
      (define t2 (make-avltree < = '()))
      (printf "==========t2 (insertion):~n")
      (insert! t2 50)
      (insert! t2 80)
      (insert! t2 90)
      ;;(tree-insert! t2 50 50)
      (avltree-print t2)
      (insert! t2 100)
      (avltree-print t2)
      (insert! t2 150)
      (avltree-print t2)
      (insert! t2 130)
      (avltree-print t2)
      )

    (let ()
      (define t3 (make-avltree < = '()))
      (printf "==========t3 (removal):~n")
      (insert! t3 50)
      (insert! t3 20)
      (insert! t3 80)
      (insert! t3 10)
      (insert! t3 30)
      (insert! t3 60)
      (insert! t3 35)
      (insert! t3 25)
      (insert! t3 65)
      (insert! t3 55)
      (insert! t3 90)
      (avltree-print t3)

      ;; get a right-heavy
      (avltree-delete! t3 10)
      (avltree-print t3)
      ;; get a left-heavy
      (avltree-delete! t3 30)
      (avltree-print t3)
      )



    (let ()
      (define t4 (make-avltree < = '()))
      (printf "==========t4 (removal):~n")
      (insert! t4 50)
      (insert! t4 20)
      (insert! t4 80)
      (insert! t4 10)
      (insert! t4 30)
      (insert! t4 60)
      (insert! t4 35)
      (insert! t4 25)
      (insert! t4 65)
      (insert! t4 55)
      (insert! t4 90)
      (avltree-print t4)
      ;; get a left-heavy
      (avltree-delete! t4 90)
      (avltree-print t4)
      (avltree-delete! t4 80)
      (avltree-print t4)
      (avltree-delete! t4 65)
      (avltree-print t4)
      (avltree-delete! t4 55)
      (avltree-print t4)
      )

    (let ()
      (define t5 (make-avltree < = '()))
      (printf "==========t5 (random):~n")
      (do ([i 0  (add1 i)])
          ((= i 10))
        (set! t5 (make-avltree < = '()))
        (do ([j 0 (add1 j)])
            ((= j 50) (printf "random insertion ~a time(s)~n" i))
          (insert! t5 (random 9999999))))
      ;; (avltree-to-dot-internal t5 "../avl.dot")
      )

    (let ()
      (define t6 (make-avltree < = '()))
      (define vec (random-vector 99999 999999))
      (printf "==========t6 (random removal):~n")
      (vector-for-each (lambda (x) (insert! t6 x)) vec)
      (shuffle! vec)
      (printf "STARTING TO DELETE~n")
      (for-each (lambda (x) (avltree-delete! t6 x)) (vector->list vec))
      )
    ))

;; btree.ss
;; rbtree.ss
(define test-rbtree
  (lambda ()
    (include "rbtree.ss")
    (define r1 (random-vector 500 999999))
    (define rb1 (make-rbtree < = (vector->list (vector-map (lambda (x) (cons x x)) r1))))
    (do ([i 0 (add1 i)]
         [n (+ 50 (random 999999)) (+ 50 (random 999999))])
        ((= i 50))
      (printf "========rbtree insertion test: ~a========~n" i)
      (printf "approximate number of nodes: ~a~n" n)
      (printf "rb1 valid? ~a~n" (rbtree-valid? rb1))
      (do ([j 0 (add1 j)])
          ((= j (/ n 2)))
;;        (printf "delete ~a time(s)~n" j)
        (rbtree-delete! rb1 (random n))
        (printf "rb1 valid after removal? ~a~n" (rbtree-valid? rb1)))
      (set! r1 (random-vector n 99999999))
      (set! rb1 (make-rbtree < = (vector->list (vector-map (lambda (x) (cons x x)) r1)))))

    ;;(rbtree-to-dot rb2 "../rb.dot")
    ))

;; splay-tree.ss
#|
(define test-sptree
  (lambda ()
    (include "splay-tree.ss")
    (define sp1 (make-sptree < = '()))
    #f))
|#

;; trie.ss
(define test-trie
  (lambda ()
    #f))


;; btree.ss
(define test-btree
  (lambda ()
    (include "btree.ss")
    (define insert!
      (lambda (T k) (btree-insert! T k k)))
    ;; default 2-3-4 tree
    ;;(define b1 (make-btree))

    (define b1 (make-btree < = 3))
    (define c 0)
    (insert! b1 50)
    (insert! b1 10)
    (insert! b1 90)

    (for-each (lambda (v) (insert! b1 v)) '(60 30 5 100 5 5 1 1 80 120 120 15 95 96 97
                                               35 42 78 87 99 66 55 33 57 122 321 49
                                               210 129 143 156 87))

    (printf "btree-map: ~a~n" (btree-map b1 (lambda (k v) k)))

    (printf "btree-for-each~n")
    (btree-for-each b1 (lambda (k v)
                         (printf "K: ~a    V: ~a~n" k v)))

    (printf "btree-fold-left~n")
    (btree-fold-left b1 (lambda (acc k v)
                         (printf "ACC: ~a    K: ~a    V: ~a~n" acc k v)
                         (+ acc k))
                    0)

    (printf "btree-fold-right~n")
    (btree-fold-right b1 (lambda (k v acc)
                         (printf "K: ~a    V: ~a    ACC: ~a~n" k v acc)
                         (+ acc k))
                    0)
    ;; I used my naked eye to check the sequence of generated
    ;; tree diagrams during deletion and found no errors.
    ;; Actually I found some errors and corrected them.
    #|
    (for-each (lambda (v)
                (btree-to-dot b1 (format "../b-delete~a.dot" c))
                (btree-delete! b1 v)
                (set! c (add1 c)))
              '(60 30 5 100 5 5 1 1 80 120 120 15 95 96 97))
    (btree-to-dot b1 (format "../b-delete~a.dot" c))
    |#
    #|
    (for-each (lambda (v)
    (btree-to-dot b1 (format "../dd~a.dot" c))
    (btree-delete! b1 v)
    (set! c (add1 c)))
    '(35 42 78 87 99 66 55 33 57 122 321 49))
    (btree-to-dot b1 (format "../dd~a.dot" c))
    |#
    ))
