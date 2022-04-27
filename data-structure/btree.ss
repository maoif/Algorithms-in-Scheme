#|
B-tree

;;; API
(make-btree)
(make-btree degree)
(make-btree compare equal)
(make-btree compare equal degree)
(make-btree compare equal kvs)
(btree-search T k)
(btree-insert! T k v)
(btree-remove! T k)
(btree-map T proc)
(btree-for-each T proc)
(btree-fold-left T proc)
(btree-fold-right T proc)
(btree-empty? T)

;;; Implementation
This B-tree implementation is taken from [Cormen].

Unlike binary trees, B-trees have more than two children per node.
Like binary trees, keys and children in B-tree nodes are also ordered.

The degree d (d >= 2) of B-tree constrains the maximum and minimum
number of keys each node could have. Given d, each node can have
at most 2d-1 keys, hence at most 2d children; each node can have
at least d-1 keys and d children. The root can have at least 1 key and
2 children. Here the maximum number of keys is always odd so we can
always find a truly middle key when splitting.

The base case is when d=2, and we have a 2-3-4 tree.

DO pay attention to edge cases and the index.

Alternative decisions:
1. use list, not vector, to store keys, values, children
2. scan from right to left
|#

(define-syntax define-who
  (lambda (x)
    (syntax-case x ()
      [(k (id . args) b1 b2 ...)
       #'(k id (lambda args b1 b2 ...))]
      [(k #(prefix id) e)
       (and (identifier? #'prefix) (identifier? #'id))
       (with-implicit (k who)
                      (with-syntax ([ext-id (construct-name #'id #'prefix #'id)])
                                   #'(define ext-id (let ([who 'id]) (rec id e)))))]
      [(k id e)
       (identifier? #'id)
       (with-implicit (k who)
                      #'(define id (let ([who 'id]) e)))])))


(define-record-type bnode
  (fields (mutable leaf?) (mutable size) (mutable keys)
          (mutable values) (mutable children)))
(define-record-type (btree mk-btree btree?)
  (fields compare equal degree (mutable root)))

(define make-btree
  (case-lambda
   [()  (mk-btree < = 2 '())]
   [(degree) (mk-btree < = degree '())]
   [(compare equal) (if (and (procedure? compare) (procedure? equal))
                        (mk-btree compare equal 2 '())
                        (errorf "make-btree" "invalid procedure argument"))]
   [(compare equal arg)
    (if (and (procedure? compare) (procedure? equal))
        (cond [(and (integer? arg) (>= arg 2))
               (mk-btree compare equal arg '())]
              [(list? arg)
               (let ([T (mk-btree compare equal 2 '())])
                 (for-each (lambda (kv)
                             (if (pair? kv)
                                 (btree-insert! T (car kv) (cdr kv))
                                 (errorf "make-btree" "invalid key-value representation: ~a" kv)))
                           kvs)
                 T)]
              [else (errorf "make-btree" "invalid third argument: ~a" arg)])
        (errorf "make-btree" "invalid procedure argument"))]
   [(compare equal degree kvs)
    (if (and (procedure? compare) (procedure? equal))
        (if (and (integer? arg) (>= arg 2))
            (if (list? kvs)
                (let ([T (mk-btree compare equal degree '())])
                  (for-each (lambda (kv)
                              (if (pair? kv)
                                  (btree-insert! T (car kv) (cdr kv))
                                  (errorf "make-btree" "invalid key-value representation: ~a" kv)))
                            kvs)
                  T)
                (errorf "make-btree" "invalid btree key-value pairs"))
            (errorf "make-btree" "invalid btree degree"))
        (errorf "make-btree" "invalid procedure argument"))]))

(define btree-empty?
  (lambda (T) (null? (btree-root T))))

(define btree-search
  (lambda (T k)
    (let ([tree (btree-root T)])
      (if (null? tree)
          #f
          (let loop ([tree tree])
            #f)))))

(define btree-map
  (lambda (T proc)
    (if (procedure? proc)
        (let ([result '()]
              [tree (btree-root T)])
          (if (null? tree)
              '()
              (begin (let loop ([tree tree])
                       (let ([k* (bnode-keys tree)]
                             [v* (bnode-values tree)]
                             [size (bnode-size tree)])
                         (if (bnode-leaf? tree)
                             ;; walk kvs
                             (set! result (append result
                                                  (vector-walk k* v* proc size)))

                             ;; in-order
                             (do ([i 0 (add1 i)]
                                  [c* (bnode-children tree)])
                                 ((= i size) (loop (vector-ref c* i)))
                               (loop (vector-ref c* i))
                               (set! result (append result (list (proc (vector-ref k* i)
                                                                       (vector-ref v* i)))))))))
                     result)))
        (errorf "btree-map" "not a procedure: ~a" proc))))

;; Walk the len elements in vector(s) from left to right,
;; feed the to proc, return the list of result.
(define vector-walk
  (lambda (v1 v2 proc len)
    (do ([i 0 (add1 i)]
         [result '()])
        ((= i len) result)
      (set! result (append result (list (proc (vector-ref v1 i)
                                              (vector-ref v2 i))))))))

;; walk for effects
(define vector-walk!
  (lambda (v1 v2 proc len)
    (do ([i 0 (add1 i)])
        ((= i len))
      (proc (vector-ref v1 i)
            (vector-ref v2 i)))))
;; reverse walk for effects
(define vector-walk-rev!
  (lambda (v1 v2 proc len)
    (do ([i (sub1 len) (sub1 i)])
        ((= i -1))
      (proc (vector-ref v1 i)
            (vector-ref v2 i)))))

;; proc: k v -> a
(define btree-for-each
  (lambda (T proc)
    (if (procedure? proc)
        (let ([tree (btree-root T)])
          (if (null? tree)
              '()
              (let loop ([tree tree])
                (let ([k* (bnode-keys tree)]
                      [v* (bnode-values tree)]
                      [size (bnode-size tree)])
                  (if (bnode-leaf? tree)
                      ;; walk kvs
                      (vector-walk! k* v* proc size)
                      ;; in-order
                      (do ([i 0 (add1 i)]
                           [c* (bnode-children tree)])
                          ((= i size) (loop (vector-ref c* i)))
                        (loop (vector-ref c* i))
                        (proc (vector-ref k* i)
                              (vector-ref v* i))))))))
        (errorf "btree-for-each" "not a procedure: ~a" proc))))

;; proc: acc k v -> a
(define btree-fold-left
  (lambda (T proc acc)
    (if (procedure? proc)
        (let ([tree (btree-root T)])
          (if (null? tree)
              '()
              (let loop ([tree tree]
                         [acc acc])
                (let ([k* (bnode-keys tree)]
                      [v* (bnode-values tree)]
                      [size (bnode-size tree)])
                  (if (bnode-leaf? tree)
                      ;; walk kvs
                      (let ([res acc])
                        (vector-walk! k* v* (lambda (k v) (set! res (proc res k v))) size)
                        res)
                      ;; go to children
                      (do ([i 0 (add1 i)]
                           [c* (bnode-children tree)])
                          ((= i size) ;; last child, just return folded the value
                           (loop (vector-ref c* i) acc))
                        (set! acc (proc (loop (vector-ref c* i) acc)
                                        (vector-ref k* i)
                                        (vector-ref v* i)))))))))
        (errorf "btree-fold-left" "not a procedure: ~a" proc))))

;; proc: k v acc -> a
(define btree-fold-right
  (lambda (T proc acc)
        (if (procedure? proc)
        (let ([tree (btree-root T)])
          (if (null? tree)
              '()
              (let loop ([tree tree]
                         [acc acc])
                (let ([k* (bnode-keys tree)]
                      [v* (bnode-values tree)]
                      [size (bnode-size tree)])
                  (if (bnode-leaf? tree)
                      ;; walk kvs
                      (let ([res acc])
                        (vector-walk-rev! k* v* (lambda (k v) (set! res (proc k v res))) size)
                        res)
                      ;; go to children
                      (do ([i size (sub1 i)]
                           [c* (bnode-children tree)])
                          ;; first child, just return the folded value
                          ((= i 0) (loop (vector-ref c* i) acc))
                        (set! acc (proc (vector-ref k* (sub1 i))
                                        (vector-ref v* (sub1 i))
                                        (loop (vector-ref c* i) acc)))))))))
        (errorf "btree-fold-right" "not a procedure: ~a" proc))))

;; When searching for a position to insert, we split
;; all full nodes along the way, in order to avoid
;; the need to back up and split.
(define btree-insert!
  (lambda (T k v)
    (define t (btree-degree T))
    ;; Splits B, and return the other half.
    (define split-node!
      (lambda (B)
        (let* ([leaf? (bnode-leaf? B)]
               [Bk* (bnode-keys B)]
               [Bv* (bnode-values B)]
               [Bc* (bnode-children B)]
               [size (bnode-size B)]
               [middle (fxsrl (sub1 (* 2 t)) 1)]
               [new (make-bnode leaf? middle
                                (make-vector size #f)
                                (make-vector size #f)
                                (make-vector (add1 size) #f))])
          ;; copy the portion of the new node
          (do ([i (add1 middle) (add1 i)])
              ((= i size))
            ;; copy to new should start from 0
            (let ([n (- i (add1 middle))])
              (vector-set! (bnode-keys new)     n (vector-ref (bnode-keys B) i))
              (vector-set! (bnode-values new)   n (vector-ref (bnode-values B) i))
              (vector-set! (bnode-children new) n (vector-ref (bnode-children B) i))

              (vector-set! (bnode-keys B) i #f)
              (vector-set! (bnode-values B) i #f)
              (vector-set! (bnode-children B) i #f)))
          ;; don't forget the last child
          (vector-set! (bnode-children new) middle (vector-ref (bnode-children B) size))
          (vector-set! (bnode-children B) size #f)

          (bnode-size-set! B middle)

          new)))
    (define split!
      (lambda (B P)
        (if (null? P)
            ;; When P is null, B is the root. In this case we create a
            ;; new root (from the middle element of the old root), split
            ;; the old root and make them the children of the new one.
            (let* ([size (sub1 (* 2 t))]
                   [new-root (make-bnode #f 1
                                         (make-vector size #f)
                                         (make-vector size #f)
                                         (make-vector (add1 size) #f))]
                   [middle (fxsrl size 1)]
                   [new (split-node! B)])
              (vector-set! (bnode-keys new-root)   0 (vector-ref (bnode-keys B) middle))
              (vector-set! (bnode-values new-root) 0 (vector-ref (bnode-values B) middle))

              (vector-set! (bnode-keys B) middle #f)
              (vector-set! (bnode-values B) middle #f)

              (vector-set! (bnode-children new-root) 0 B)
              (vector-set! (bnode-children new-root) 1 new)
              (btree-root-set! T new-root))
            ;; We are guaranteed that P is not full, since if it were,
            ;; it would've been splitted beforehand.
            (let* ([new (split-node! B)]
                   [middle (fxsrl (sub1 (* 2 (btree-degree T))) 1)]
                   [middle-k (vector-ref (bnode-keys B) middle)]
                   [middle-v (vector-ref (bnode-values B) middle)])
              (vector-set! (bnode-keys B) middle #f)
              (vector-set! (bnode-values B) middle #f)
              ;; push the middle node up to the parent
              (do ([i (sub1 (bnode-size P)) (sub1 i)]
                   [pk* (bnode-keys P)]
                   [pv* (bnode-values P)]
                   [pc* (bnode-children P)]
                   [compare (btree-compare T)]
                   [done? #f])
                  (done?)
                (if (or (= i -1) (compare (vector-ref pk* i) middle-k))
                    (begin (set! done? #t)
                           (vector-set! pk* (add1 i) middle-k)
                           (vector-set! pv* (add1 i) middle-v)
                           (vector-set! pc* (+ 1 i) B)
                           (vector-set! pc* (+ 2 i) new)
                           (bnode-size-set! P (add1 (bnode-size P))))
                    (begin (vector-set! pk* (add1 i) (vector-ref pk* i))
                           (vector-set! pv* (add1 i) (vector-ref pv* i))
                           (vector-set! pc* (+ 2 i) (vector-ref pc* (+ 1 i))))))))))

    (let* ([tree (btree-root T)]
           [degree (btree-degree T)]
           [compare (btree-compare T)]
           [equal (btree-equal T)]
           [max-size (sub1 (* 2 degree))])
      (if (null? tree)
          (let ([rt (make-bnode #t 1 (make-vector max-size #f)
                                (make-vector max-size #f)
                                (make-vector (* 2 degree) #f))])
            (vector-set! (bnode-keys rt) 0 k)
            (vector-set! (bnode-values rt) 0 v)
            (btree-root-set! T rt))
          (begin (when (= max-size (bnode-size tree))
                   (split! tree '()))
                 (let loop ([P '()]
                            ;; The root could change if the old root is split,
                            ;; so get it again.
                            [B (btree-root T)])
                   (let ([n (bnode-size B)])
                     (let ([leaf? (bnode-leaf? B)]
                           [k* (bnode-keys B)]
                           [v* (bnode-values B)]
                           [c* (bnode-children B)])
                       (if leaf?
                           ;; If the current node is a leaf, then it's the node
                           ;; into which we'll insert the new key-value. We compare
                           ;; the keys from right to left and shift the key/value/children
                           ;; until we find the right position.
                           (do ([i (sub1 n) (sub1 i)]
                                ;; use this so that the loop won't continue
                                ;; after we insert the element
                                [done? #f])
                               (done?)
                             (when (or (= i -1) (compare (vector-ref k* i) k))
                               (set! done? #t)
                               (if (= i (sub1 n))
                                   ;; k is the biggest
                                   (begin (vector-set! k* (add1 i) k)
                                          (vector-set! v* (add1 i) v)
                                          (bnode-size-set! B (add1 n)))
                                   ;; if key exists then do nothing
                                   (unless (equal k (vector-ref k* (add1 i)))
                                     (begin (vector-insert! k* k (add1 i) (add1 n))
                                            (vector-insert! v* v (add1 i) (add1 n))
                                            (bnode-size-set! B (add1 n)))))))
                           (do ([i (sub1 n) (sub1 i)]
                                [done? #f])
                               (done?)
                             (when (or (= i -1) (compare (vector-ref k* i) k))
                               (set! done? #t)
                               (when (or (= i (sub1 n))
                                         (not (equal k (vector-ref k* (add1 i)))))
                                 ;; k is the biggest, or v[i+1]!=k
                                 ;; if key exists then do nothing
                                 (if (= max-size (bnode-size (vector-ref c* (add1 i))))
                                     (begin (split! (vector-ref c* (add1 i)) B)
                                            ;; Splitting the child might cause B to be
                                            ;; full, but we don't have to detect B upon
                                            ;; the next loop.
                                            (loop P B))
                                     (loop B (vector-ref c* (add1 i))))))))))))))))

;; Contrary to insertion wherein we split the full nodes before
;; descending down so that we don't have to back up and split,
;; we merge, or borrow keys from siblings to make sure that the
;; nodes we visit, and the leaf from which we delete key, all have
;; at least degree keys, and that after deletion, the number of
;; keys in the leaf is >= degree-1. Hence no need to back up
;; and fix the holes.
(define btree-delete!
  (lambda (T k)
    (let ([tree (btree-root T)])
      (if (null? tree)
          #f
          ;; get the index which either corresponds to the key,
          ;; or whose child contains the key
          (let ([compare (btree-compare T)]
                [equal (btree-equal T)]
                [t (btree-degree T)])
            (let loop ([tree tree])
              (let ([leaf? (bnode-leaf? tree)]
                    [size (bnode-size tree)]
                    [k* (bnode-keys tree)]
                    [v* (bnode-values tree)]
                    [c* (bnode-children tree)])
                (do ([i (sub1 size) (sub1 i)]
                     [done? #f])
                    (done?)
                  (when (or (= i -1) (compare (vector-ref k* i) k))
                    (set! done? #t)
                    (if leaf?
                        (if (= i (sub1 size))
                            ;; no key found
                            #f
                            (if (equal k (vector-ref k* (add1 i)))
                                ;; if the key=k, and tree is leaf, delete (slide the later keys)
                                (begin
                                  (vector-shift! k* (add1 i))
                                  (vector-shift! v* (add1 i))
                                  (bnode-size-set! tree (sub1 (bnode-size tree)))
                                  (when (and (eq? tree (btree-root T)) (= 0 (bnode-size tree)))
                                    (btree-root-set! T '())))
                                #f))
                        ;; internal node
                        (if (= i (sub1 size))
                            (if (<= t (bnode-size (vector-ref c* size)))
                                (loop (vector-ref c* size))
                                ;; check left sib
                                (let ([left-sib (vector-ref c* (sub1 size))])
                                  (if (<= t (bnode-size left-sib))
                                      (loop (bnode-borrow! tree size (sub1 size)))
                                      (loop (bnode-merge! T tree (sub1 size))))))
                            (if (equal k (vector-ref k* (add1 i)))
                                ;; find pred or succ
                                (let ([left (vector-ref c* (+ i 1))]
                                      [right (vector-ref c* (+ i 2))])
                                  (cond [(<= t (bnode-size left))
                                         ;; find predecessor
                                         (let find-pred ([left left])
                                           (if (bnode-leaf? left)
                                               ;; swap value and delete the pred
                                               (let ([n (sub1 (bnode-size left))])
                                                 (vector-set! k* (add1 i) (vector-ref (bnode-keys left) n))
                                                 (vector-set! v* (add1 i) (vector-ref (bnode-values left) n))
                                                 (vector-set! (bnode-keys left) n #f)
                                                 (vector-set! (bnode-values left) n #f)
                                                 (bnode-size-set! left n))
                                               (let* ([rightest (vector-ref (bnode-children left) (bnode-size left))]
                                                      [rightest-c* (bnode-children rightest)]
                                                      [rightest-size (bnode-size rightest)])
                                                 (if (<= t rightest-size)
                                                     (find-pred rightest)
                                                     (let* ([left-sib (vector-ref (bnode-children left) (sub1 (bnode-size left)))]
                                                            [left-sib-c* (bnode-children left-sib)]
                                                            [left-sib-size (bnode-size left-sib)])
                                                       (if (<= t left-sib-size)
                                                           ;; if left sib has enough keys, borrow from it
                                                           (find-pred (bnode-borrow! left (bnode-size left) (sub1 (bnode-size left))))
                                                           ;; else merge
                                                           (find-pred (bnode-merge! T left (sub1 (bnode-size left))))))))))]
                                        [(<= t (bnode-size right))
                                         ;; find successor
                                         (let find-succ ([right right])
                                           (if (bnode-leaf? right)
                                               ;; swap value and delete the succ
                                               (begin
                                                 (vector-set! k* (add1 i) (vector-ref (bnode-keys right) 0))
                                                 (vector-set! v* (add1 i) (vector-ref (bnode-values right) 0))
                                                 ;; because we're deleting the first, not the last...
                                                 (vector-shift! (bnode-keys right) 0)
                                                 (vector-shift! (bnode-values right) 0)
                                                 (bnode-size-set! right (sub1 (bnode-size right))))
                                               (let* ([leftest (vector-ref (bnode-children right) 0)]
                                                      [leftest-c* (bnode-children leftest)]
                                                      [leftest-size (bnode-size leftest)])
                                                 (if (<= t leftest-size)
                                                     (find-succ leftest)
                                                     (let* ([right-sib (vector-ref (bnode-children right) 1)]
                                                            [right-sib-c* (bnode-children right-sib)]
                                                            [right-sib-size (bnode-size right-sib)])
                                                       (if (<= t right-sib-size)
                                                           ;; if right sib has enough keys, borrow from it
                                                           (find-succ (bnode-borrow! right 0 1))
                                                           ;; else merge
                                                           (find-succ (bnode-merge! T right 0))))))))]
                                        [else ;; merge left and right, then recursively delete k in the merged node
                                         (loop (bnode-merge! T tree (add1 i)))]))
                                (let ([c (vector-ref c* (add1 i))])
                                  (if (<= t (bnode-size c))
                                      (loop c)
                                      (if (= i -1)
                                          ;; check the only right sib
                                          (let ([r-sib (vector-ref c* 1)])
                                            (if (<= t (bnode-size r-sib))
                                                (loop (bnode-borrow! tree 0 1))
                                                (loop (bnode-merge! T tree 0))))
                                          (let ([l-sib (vector-ref c* i)]
                                                [r-sib (vector-ref c* (+ 2 i))])
                                            (cond [(<= t (bnode-size l-sib))
                                                   ;; borrow from left
                                                   (loop (bnode-borrow! tree (add1 i) i))]
                                                  [(<= t (bnode-size r-sib))
                                                   ;; borrow from right
                                                   (loop (bnode-borrow! tree (add1 i) (+ 2 i)))]
                                                  [else
                                                   ;; merge c with left and the current key,
                                                   ;; then loop with the merged node
                                                   (loop (bnode-merge! T tree i))])))))))))))))))))

(define bnode-borrow!
  (lambda (P to from)
    (let ([c* (bnode-children P)])
      (let* ([borrower (vector-ref c* to)]
             [borrowee (vector-ref c* from)]
             [borrower-size (bnode-size borrower)]
             [borrowee-size (bnode-size borrowee)])
        (if (< to from)
            ;; borrow from right
            (let* ([p-index to]
                   [p-key (vector-ref (bnode-keys P) p-index)]
                   [p-value (vector-ref (bnode-values P) p-index)]
                   [right-key (vector-ref (bnode-keys borrowee) 0)]
                   [right-value (vector-ref (bnode-values borrowee) 0)]
                   [right-child (vector-ref (bnode-children borrowee) 0)])
              ;; set the size first for vector-insert! to work correcly
              (bnode-size-set! borrower (add1 borrower-size))
              (vector-insert! (bnode-keys borrower) p-key (sub1 (bnode-size borrower)) (bnode-size borrower))
              (vector-insert! (bnode-values borrower) p-value (sub1 (bnode-size borrower)) (bnode-size borrower))
              (vector-insert! (bnode-children borrower) right-child (bnode-size borrower) (add1 (bnode-size borrower)))

              (vector-set! (bnode-keys P) p-index right-key)
              (vector-set! (bnode-values P) p-index right-value)

              (vector-shift! (bnode-keys borrowee) 0)
              (vector-shift! (bnode-values borrowee) 0)
              (vector-shift! (bnode-children borrowee) 0)
              (bnode-size-set! borrowee (sub1 borrowee-size)))
            ;; borrow from left
            (let* ([p-index (sub1 to)]
                   [p-key (vector-ref (bnode-keys P) p-index)]
                   [p-value (vector-ref (bnode-values P) p-index)]
                   [left-key (vector-ref (bnode-keys borrowee) (sub1 borrowee-size))]
                   [left-value (vector-ref (bnode-values borrowee) (sub1 borrowee-size))]
                   [left-child (vector-ref (bnode-children borrowee) borrowee-size)])
              (bnode-size-set! borrower (add1 borrower-size))
              (vector-insert! (bnode-keys borrower) p-key 0 (bnode-size borrower))
              (vector-insert! (bnode-values borrower) p-value 0 (bnode-size borrower))
              (vector-insert! (bnode-children borrower) left-child 0 (add1 (bnode-size borrower)))

              (vector-set! (bnode-keys P) p-index left-key)
              (vector-set! (bnode-values P) p-index left-value)

              (vector-set! (bnode-keys borrowee) (sub1 borrowee-size) #f)
              (vector-set! (bnode-values borrowee) (sub1 borrowee-size) #f)
              (vector-set! (bnode-children borrowee) borrowee-size #f)
              (bnode-size-set! borrowee (sub1 borrowee-size))))

        borrower))))

(define bnode-merge!
  (lambda (T P i)
    ;; merge right to left, then drop right, return left
    (let* ([p-c* (bnode-children P)]
           [p-key (vector-ref (bnode-keys P) i)]
           [p-value (vector-ref (bnode-values P) i)]
           [left (vector-ref p-c* i)]
           [right (vector-ref p-c* (add1 i))])
      (let ([l-size (add1 (bnode-size left))])
        (vector-insert! (bnode-keys left) p-key (sub1 l-size) l-size)
        (vector-insert! (bnode-values left) p-value (sub1 l-size) l-size)
        (bnode-size-set! left l-size)
        (vector-copy-range! (bnode-keys right) (bnode-keys left) 0 l-size (bnode-size right))
        (vector-copy-range! (bnode-values right) (bnode-values left) 0 l-size (bnode-size right))
        (vector-copy-range! (bnode-children right) (bnode-children left) 0 l-size (add1 (bnode-size right)))

        (bnode-size-set! left (+ (bnode-size left) (bnode-size right))))

      (vector-shift! (bnode-keys P) i)
      (vector-shift! (bnode-values P) i)
      (vector-shift! (bnode-children P) (add1 i))
      (bnode-size-set! P (sub1 (bnode-size P)))
      (when (= 0 (bnode-size P))
        (btree-root-set! T left))

      left)))

(define btree-to-dot
  (lambda (T path)
    (define gen (let ([i -1]) (lambda () (set! i (add1 i)) i)))
    (define build-key
      (lambda (k) (format "<td>~a</td>~n" k)))
    (define build-child
      (lambda (i) (format "<td port=\"c~a\" bgcolor=\"lightgrey\"></td>~n" i)))
    (define build-node
      (lambda (tree index)
        (let* ([k* (bnode-keys tree)]
               [n (bnode-size tree)]
               [c0 (build-child 0)]
               [node (format "node_~a" index)])
          (string-append (let loop ([res (string-append (format "~a [label=<~n" node)
                                                        (format "<table border=\"0\" cellspacing=\"0\" cellborder=\"1\">~n")
                                                        (format "<tr>~n")
                                                        c0)]
                                    ;; cells is a list, so can't append directly
                                    [cells (let ([cells '()])
                                             (let f ([i 0])
                                               (if (= i n)
                                                   cells
                                                   (begin (set! cells
                                                                (append cells
                                                                        (list (build-key (vector-ref k* i))
                                                                              (build-child (add1 i)))))
                                                          (f (add1 i))))))])
                           (if (null? cells)
                               res
                               (loop (string-append res (car cells)) (cdr cells))))
                         (format "</tr>~n")
                         (format "</table>~n")
                         (format ">];~n")))))

    (let ([tree (btree-root T)])
      (if (null? tree)
          (printf "btree is empty, nothing written~n")
          ;; output one child,
          ;; then output key[i] and child[i+1] until i=size
          (call-with-output-file path
            (lambda (p)
              (printf "dot file at ~a~n" path)
              (put-string p "digraph {")
              (fresh-line p)
              (put-string p "node [shape=none,fontname=monospace];")
              (fresh-line p)
              (let ([edges '()])
                (let loop ([tree tree]
                           [index (gen)])
                  (put-string p (build-node tree index))
                  (unless (bnode-leaf? tree)
                    (let ([c* (bnode-children tree)]
                          [size (bnode-size tree)])
                      (do ([i 0 (add1 i)]
                           [ind (gen) (gen)])
                          ((= i (add1 size)))
                        (loop (vector-ref c* i) ind)
                        (set! edges (cons (format "node_~a:c~a:s -> node_~a:c0:nw;~n"
                                                  index i ind)
                                          edges))))))
                ;; write edges
                (for-each (lambda (e) (put-string p e)) edges))
              (put-string p "}")))))))

;; return the index i where (compare v[i+1] x) is #t
;; i+1 because we use this to calculate the children index
;; i could be -1 if k is the smallest,
;; or
(define find-index
  (lambda (v x end compare)
    #f))

;; i: the position to insert
;; size: number of non-#f elements, should set size of node before using this,
;; Slides elements to right to spare a slot for val.
(define vector-insert!
  (lambda (v val i size)
    (if (< i (vector-length v))
        (if (= i (sub1 size))
            (vector-set! v i val)
            (do ([j (- size 2) (sub1 j)]
                 [done? #f])
                (done?)
              (if (= i j)
                  (begin (set! done? #t)
                         (vector-set! v (add1 j) (vector-ref v j))
                         (vector-set! v j val))
                  (vector-set! v (add1 j) (vector-ref v j)))))
        (errorf "vector-insert!" "index out of range: ~a" i))))

;; Right shift elements in v from i, fill the leftmost with #f,
;; stop when i = length(v)-1 or when v[i+1]=#f,
;; used when one key is removed from a node.
(define vector-shift!
  (lambda (v i)
    (let loop ([i i])
      (if (or (= i (sub1 (vector-length v)))
              (not (vector-ref v (add1 i))))
          (vector-set! v i #f)
          (begin (vector-set! v i (vector-ref v (add1 i)))
                 (loop (add1 i)))))))

;; copy len values starting from from[from-i] to to[to-i]
(define vector-copy-range!
  (lambda (from to from-i to-i len)
    (if (and (<= (+ from-i len) (vector-length from))
             (<= (+ to-i len) (vector-length to)))
        (let loop ([i len]
                   [f from-i]
                   [t to-i])
          (unless (= 0 i)
            (vector-set! to t (vector-ref from f))
            (loop (sub1 i) (add1 f) (add1 t))))
        (errorf "vector-copy-range!" "invalid index ~a or ~a" from-i to-i))))
