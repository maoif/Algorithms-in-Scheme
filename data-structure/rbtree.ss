#|
Red-black Tree

;;; API
(make-rbtree [kvs])
(make-rbtree compare equal [kvs])
kvs is a list of pairs, the car and cdr of the pairs
are keys and values respectively. the keys need to
be comparable under compare and equal.

If compare and equal are not given, rbtree assuems that
keys are integers and compare is < and equal is =.

(rbtree-search T k)
(rbtree-insert! T k v)
Inserts key k and value v into T. If k exists, update its value to v.

(rbtree-remove! T k)
Deletes the node associated with key k. Exception is thrown if
k doesn't exist.

(rbtree-for-each T proc)
Iterate through the whole tree from left to right in order.
proc has two parameters, the first will be the key,
the second will be the corresponding value.
(rbtree-fold-left T proc)
(rbtree-fold-right T proc)
(rbtree-leftmost T)
(rbtree-rightmost T)
(rbtree-predecessor T k)
(rbtree-successor T k)
(rbtree-exists? T k)
Determines whether a key k exists in T.

;;; Implementation
1. every node is either red or black
2. the root node is black
3. every leaf(nil) is black
4. if a node is red, its children are black
5. for each node, all simple paths from the node
   to descendent leaves contain the same number
   of black nodes

|#

#|
(k prefix get-root  get-key  get-value  get-parent  get-left  get-right  get-compare get-equal
          set-root! set-key! set-value! set-parent! set-left! set-right! tree-nil make-node)
|#

(module (make-rbtree rbtree-search rbtree-insert! rbtree-delete! rbtree-to-dot rbtree-valid?)
        (include "tree-api.ss")
        (define-tree-api "rb"
          rbtree-root rbnode-key rbnode-value rbnode-parent rbnode-left rbnode-right rbtree-compare rbtree-equal
          rbtree-root-set! rbnode-key-set! rbnode-value-set! rbnode-parent-set! rbnode-left-set! rbnode-right-set!
          rb-nil (lambda (k v p) (mk-rbnode RED p rb-nil rb-nil k v)))

        (define RED #t)
        (define BLACK #f)
        (define-record-type (rbnode mk-rbnode rbnode?)
          (fields (mutable color) (mutable parent) (mutable left)
                  (mutable right) (mutable key) (mutable value)))

        (define-record-type (rbtree mk-rbtree rbtree?)
          (fields compare equal (mutable root)))

        ;; The unique empty leaf, also the parent of the root.
        ;; During node deletion, the parent field of this node may
        ;; be set, but that's OK since it isn't used anywhere else.
        (define rb-nil (mk-rbnode BLACK #f #f #f #f #f))

        ;; ((k . v) (k . v) (k . v) ... )
        (define make-rbtree
          (case-lambda
           [() (mk-rbtree < = '())]
           [(kvs) (if (and (list? kvs) (pair? (car kvs)) (integer? (caar kvs)))
                      (make-rbtree-from-kvs < = kvs)
                      (errorf "make-rbtree" "invalid key-value list: ~a" kvs))]
           [(compare equal) (if (and (procedure? compare) (procedure? equal?))
                                (mk-rbtree compare equal '())
                                (errorf "make-rbtree" "invalid predicate: ~a or ~a" compare equal))]
           [(compare equal kvs) (if (and (procedure? compare) (procedure? equal?))
                                    (if (and (list? kvs) (pair? (car kvs)) (integer? (caar kvs)))
                                        (make-rbtree-from-kvs compare equal kvs)
                                        (errorf "make-rbtree" "invalid key-value list: ~a" kvs))
                                    (errorf "make-rbtree" "invalid predicate: ~a or ~a" compare equal))]))

        (define make-rbtree-from-kvs
          (lambda (compare equal kvs)
            (let ([T (mk-rbtree compare equal '())])
              (for-each (lambda (kv)
                          (if (pair? kv)
                              (rbtree-insert! T (car kv) (cdr kv))
                              (errorf "make-rbtree-from-kvs" "invalid key-value representation: ~a" kv)))
                        kvs)
              T)))

        (define rbtree-search rbtree-search-internal)

        (define rbtree-insert!
          (lambda (T k v)
            (let ([new (rbtree-insert!-internal T k v)])
              (if (is-root? T new)
                  (rbnode-color-set! new BLACK)
                  (unless (is-root? T (rbnode-parent new))
                    (insertion-fix! T new))))))
        (define is-root?
          (lambda (T x)
            (eq? x (rbtree-root T))))
        #;
        (define insertion-fix!          ;
        (lambda (T x)                   ;
        (define is-left-child? (lambda (x) (not (is-right-child? x)))) ;
        (define is-triangle?            ;
        (lambda (x)                     ;
        (let ([p (rbnode-parent x)])    ;
        (or (and (is-right-child? x) (is-left-child? p)) ;
        (and (is-left-child? x) (is-right-child? p)))))) ;
        (let loop ([x x])               ;
        (unless (or (eq? x (rbtree-root T)) (eq? x rb-nil)) ;
        (when (and (is-red? (rbnode-parent x)) (not (eq? (rbnode-parent x) rb-nil))) ;
        (printf "x: ~a parent: ~a~n" (rbnode-key x) (rbnode-key (rbnode-parent x))) ;
        (assert (not (eq? (rbnode-parent (rbnode-parent x)) rb-nil))) ;
        (if (is-red? (get-uncle x))     ;
        (begin (printf "uncle red~n")   ;
        (rbnode-color-set! (rbnode-parent x) BLACK) ;
        (rbnode-color-set! (get-uncle x) BLACK) ;
        (rbnode-color-set! (rbnode-parent (rbnode-parent x)) RED) ;
        (loop (rbnode-parent (rbnode-parent x)))) ;
        (let ([pd (is-right-child? (rbnode-parent x))]) ;
        (printf "uncle black~n")        ;
                            ;; uncle is black ;
                            ;; triangle, rotate first ;
        (when (is-triangle? x)          ;
        (rotate-to-parent! T x))        ;
                            ;; line     ;
        (rbnode-color-set! x BLACK)     ;
        (rbnode-color-set! (rbnode-parent x) RED) ;
        (rotate-to-parent! T x)         ;
        (loop (if pd (rbnode-right x) (rbnode-left x)))))))) ;
        (rbnode-color-set! (rbtree-root T) BLACK)))

        (define insertion-fix!
          (lambda (T x)
            (let loop ([x x])
              (if (is-root? T x)
                  (rbnode-color-set! x BLACK)
                  (let ([p (rbnode-parent x)])
                    ;; if p's root, do nothing
                    (when (is-red? p)
                      (if (is-right-child? p)
                          ;; right parent
                          (let ([uncle (rbnode-left (rbnode-parent p))])
                            (if (is-red? uncle)
                                (begin (rbnode-color-set! uncle BLACK)
                                       (rbnode-color-set! p BLACK)
                                       (rbnode-color-set! (rbnode-parent p) RED)
                                       (loop (rbnode-parent p)))
                                (begin (when (is-left-child? x)
                                         (set! x (rbnode-parent x))
                                         (rotate-right! T x 'aa))
                                       (rbnode-color-set! (rbnode-parent x) BLACK)
                                       (rbnode-color-set! (rbnode-parent (rbnode-parent x)) RED)
                                       (rotate-left! T (rbnode-parent (rbnode-parent x)) 'bb)
                                       (loop x))))
                          ;; left parent
                          (let ([uncle (rbnode-right (rbnode-parent p))])
                            (if (is-red? uncle)
                                (begin (rbnode-color-set! uncle BLACK)
                                       (rbnode-color-set! p BLACK)
                                       (rbnode-color-set! (rbnode-parent p) RED)
                                       (loop (rbnode-parent p)))
                                (begin (when (is-right-child? x)
                                         (set! x (rbnode-parent x))
                                         (rotate-left! T x 'cc))
                                       (rbnode-color-set! (rbnode-parent x) BLACK)
                                       (rbnode-color-set! (rbnode-parent (rbnode-parent x)) RED)
                                       (rotate-right! T (rbnode-parent (rbnode-parent x)) 'dd)
                                       (loop x)))))))))))

        (define rotate-left!
          (lambda (T x t)
            (let* ([p (rbnode-parent x)]
                   [y (rbnode-right x)]
                   [y-left (rbnode-left y)])
              (if (is-root? T x)
                  (rbtree-root-set! T y)
                  (if (is-right-child? x)
                      (rbnode-right-set! p y)
                      (rbnode-left-set! p y)))
              (rbnode-parent-set! x y)
              (rbnode-parent-set! y p)
              (rbnode-left-set! y x)
              (rbnode-right-set! x y-left)
              (unless (eq? y-left rb-nil)
                (rbnode-parent-set! y-left x)))))
        (define rotate-right!
          (lambda (T x t)
            (let* ([p (rbnode-parent x)]
                   [y (rbnode-left x)]
                   [y-right (rbnode-right y)])
              (if (is-root? T x)
                  (rbtree-root-set! T y)
                  (if (is-right-child? x)
                      (rbnode-right-set! p y)
                      (rbnode-left-set! p y)))
              (rbnode-parent-set! x y)
              (rbnode-parent-set! y p)
              (rbnode-right-set! y x)
              (rbnode-left-set! x y-right)
              (unless (eq? y-right rb-nil)
                (rbnode-parent-set! y-right x)))))
        ;; print helper
        (define ph
          (lambda (x)
            (if (rbnode? x)
                (rbnode-key x)
                x)))

        (define leftmost
          (lambda (x)
            (let loop ([x x])
              (let ([left (rbnode-left x)])
                (if (eq? left rb-nil)
                    x
                    left)))))
        #;
        (define rbtree-delete!          ;
        (lambda (T k)                   ;
        (let ([old (rbtree-delete!-internal T k)]) ;
        (when (and old (not (is-red? old))) ;
        (deletion-fix! T old)))))
        (define rbtree-delete!
          (lambda (T k)
            (rbtree-delete!-helper T k)))

        (define rbtree-delete!-helper
          (lambda (T k)
            (define nil?
              (lambda (x) (eq? x rb-nil)))
            (define rb-transplant!
              (lambda (x y)
                (if (is-root? T x)
                    (rbtree-root-set! T y)
                    (if (eq? x (rbnode-right (rbnode-parent x)))
                        (rbnode-right-set! (rbnode-parent x) y)
                        (rbnode-left-set! (rbnode-parent x) y)))
                ;; here y may be rb-nil, set it anyway
                (rbnode-parent-set! y (rbnode-parent x))))
            (printf "rbtree-delete!-internal~n")
            (if (rbtree? T)
                (let ([tree (rbtree-root T)]
                      [compare (rbtree-compare T)]
                      [equal (rbtree-equal T)])
                  (if (null? tree)
                      #f
                      (let loop ([tree tree])
                        (let ([key (rbnode-key tree)])
                          (cond [(equal k key)
                                 (if (is-root? T tree)
                                     ;; deleted the root
                                     (begin (rbtree-root-set! T '())
                                            #f)
                                     (let* ([y tree]
                                            [orig (rbnode-color y)]
                                            [x '()])
                                       (cond [(eq? rb-nil (rbnode-left tree))
                                              (set! x (rbnode-right tree))
                                              (rb-transplant! tree x)]
                                             [(eq? rb-nil (rbnode-right tree))
                                              (set! x (rbnode-left tree))
                                              (rb-transplant! tree x)]
                                             [else (set! y (leftmost (rbnode-right tree)))
                                                   (set! y (rbnode-color y))
                                                   (set! x (rbnode-right y))
                                                   (if (eq? (rbnode-parent y) tree)
                                                       (rbnode-parent-set! x y)
                                                       (begin (rb-transplant! tree y)
                                                              (rbnode-left-set! y (rbnode-left tree))
                                                              (rbnode-parent-set! (rbnode-left y) y)
                                                              (rbnode-color-set! y (rbnode-color tree))
                                                              (when (eq? BLACK orig)
                                                                (deletion-fix! T x))))])))]
                                [(compare k key) (let ([left (rbnode-left tree)])
                                                   ;; if key not found, do nothing
                                                   (if (nil? left) #f (loop left)))]
                                [else (let ([right (rbnode-right tree)])
                                        (if (nil? right) #f (loop right)))])))))
                (errorf "rbtree-delete" "not valid rbtree: ~a" T))))

        (define deletion-fix!
          (lambda (T x)
            (printf "fix~n")
            (let loop ([x x])
              (if (and (not (is-root? T x))
                       (not (is-red? x)))
                  (if (is-left-child? x)
                      ;; left
                      (let ([sib (get-sibling x)])
                        (when (is-red? sib)
                          (rbnode-color-set! sib BLACK)
                          (rbnode-color-set! (rbnode-parent x) RED)
                          (rotate-left! T (rbnode-parent x) 'Ra)
                          (set! sib (rbnode-right (rbnode-parent x))))
                        (if (and (not (is-red? (rbnode-left sib)))
                                 (not (is-red? (rbnode-right sib))))
                            (begin (rbnode-color-set! sib RED)
                                   (loop (rbnode-parent x)))
                            (begin (when (not (is-red? (rbnode-right sib)))
                                     (rbnode-color-set! (rbnode-left sib) BLACK)
                                     (rbnode-color-set! sib RED)
                                     (rotate-right! T sib 'Rb)
                                     (set! sib (rbnode-right (rbnode-parent x))))
                                   (rbnode-color-set! sib (rbnode-color (rbnode-parent x)))
                                   (rbnode-color-set! (rbnode-parent x) BLACK)
                                   (rbnode-color-set! (rbnode-right sib) BLACK)
                                   (rotate-left! T (rbnode-parent x) 'Rc)
                                   (loop (rbtree-root T)))))
                      ;; right
                      (let ([sib (get-sibling x)])
                        (when (is-red? sib)
                          (rbnode-color-set! sib BLACK)
                          (rbnode-color-set! (rbnode-parent x) RED)
                          (rotate-right! T (rbnode-parent x) 'Raa)
                          (set! sib (rbnode-left (rbnode-parent x))))
                        (if (and (not (is-red? (rbnode-left sib)))
                                 (not (is-red? (rbnode-right sib))))
                            (begin (rbnode-color-set! sib RED)
                                   (loop (rbnode-parent x)))
                            (begin (when (not (is-red? (rbnode-left sib)))
                                     (rbnode-color-set! (rbnode-right sib) BLACK)
                                     (rbnode-color-set! sib RED)
                                     (rotate-left! T sib 'Rbb)
                                     (set! sib (rbnode-left (rbnode-parent x))))
                                   (rbnode-color-set! sib (rbnode-color (rbnode-parent x)))
                                   (rbnode-color-set! (rbnode-parent x) BLACK)
                                   (rbnode-color-set! (rbnode-left sib) BLACK)
                                   (rotate-right! T (rbnode-parent x) 'Rcc)
                                   (loop (rbtree-root T))))))
                  (rbnode-color-set! x BLACK)))
            ))

        #;
        (define deletion-fix!            ; ;
        (lambda (T x)                   ; ;
        (printf "fix~n")                ; ;
        (let loop ([x x])               ; ;
        (if (is-root? T x)              ; ;
        (rbnode-color-set! x BLACK)     ; ;
        (unless (is-red? (rbnode-color x)) ; ;
        (let ([sib (get-sibling x)])    ; ;
        (if (is-red? sib)               ; ;
        (begin (rbnode-color-set! (rbnode-parent x) RED) ; ;
        (rbnode-color-set! sib BLACK)   ; ;
        (rotate-to-parent! T sib)       ; ;
        (loop x))                       ; ;
        (cond                           ; ;
        [(is-red? (get-nephew x))       ; ;
        (rbnode-color-set! sib (rbnode-color (rbnode-parent x))) ; ;
        (rbnode-color-set! (rbnode-parent x) BLACK) ; ;
        (rbnode-color-set! (get-nephew x) BLACK) ; ;
        (rotate-to-parent! T sib)       ; ;
                            ;; break the loop ; ;
        (loop (rbtree-root T))]         ; ;
        [(is-red? (get-niece x))        ; ;
        (rbnode-color-set! (get-niece x) BLACK) ; ;
        (rbnode-color-set! sib RED)     ; ;
        (rotate-to-parent! T (get-niece x)) ; ;
        (loop x)]                       ; ;
                           ;; two black children of sibling ; ;
        [else (rbnode-color-set! sib RED) ; ;
        (loop (get-parent x))]))))))))

        (define rbtree-for-each
          (lambda (T proc)
            #f))

        (define rbtree-fold-left
          (lambda (T proc)
            #f))

        (define rbtree-fold-right
          (lambda (T proc)
            #f))

        (define rotate-to-parent!
          (lambda (T x)
            ;; assume that x's parent exists
            (if (is-right-child? x)
                ;; if x is a right child
                (rbtree-rotate-left!-internal T x)
                ;; x is a left child
                (rbtree-rotate-right!-internal T x))))

        (define is-red?
          (lambda (x) (if (rbnode? x)
                          (rbnode-color x)
                          (errorf "is-red?" "not rbnode: ~a" x))))

        (define flip-color!
          (lambda (x)
            (if (rbnode-color x)
                (rbnode-color-set! x BLACK)
                (rbnode-color-set! x RED))))

        (define is-right-child?
          (lambda (x)
            (let ([p (rbnode-parent x)])
              (eq? x (rbnode-right p)))))
        (define is-left-child? (lambda (x) (not (is-right-child? x))))
        (define get-parent rbnode-parent)
        (define get-uncle
          (lambda (x)
            (let ([p (rbnode-parent x)])
              (if (eq? rb-nil p)
                  (errorf "get-uncle" "node is root: ~a" x)
                  (let ([pp (rbnode-parent p)])
                    (if (eq? rb-nil pp)
                        (errorf "get-uncle" "parent is root: ~a" p)
                        (if (is-right-child? p)
                            (rbnode-left pp)
                            (rbnode-right pp))))))))
        (define get-sibling
          (lambda (x)
            (let ([p (rbnode-parent x)])
              (if (eq? rb-nil p)
                  (errorf "get-sibling" "node is root: ~a" x)
                  (if (is-right-child? x)
                      (rbnode-left p)
                      (rbnode-right p))))))
        ;; If x is the right/left child of its parent,
        ;; then x's niece is the right/left child of x's sibling.
        (define get-niece
          (lambda (x)
            (let ([sib (get-sibling x)])
              (if (is-right-child? x)
                  (rbnode-right sib)
                  (rbnode-left sib)))))
        ;; If x is the right/left child of its parent,
        ;; then x's niece is the left/right child of x's sibling.
        (define get-nephew
          (lambda (x)
            (let ([sib (get-sibling x)])
              (if (is-right-child? x)
                  (rbnode-left sib)
                  (rbnode-right sib)))))

        (define rbtree-valid?
          (lambda (T)
            (call/cc (lambda (k)
                       (let ([tree (rbtree-root T)]
                             [compare (rbtree-compare T)])
                         (if (null? tree)
                             (k #t)
                             (let loop ([tree tree])
                               (let ([clr (rbnode-color tree)]
                                     [left (rbnode-left tree)]
                                     [right (rbnode-right tree)])
                                 (if (and (eq? clr RED) (or (eq? RED (rbnode-color left))
                                                            (eq? RED (rbnode-color right))))
                                     (k "color violation")
                                     (begin (unless (eq? rb-nil left)
                                              (if (not (compare (rbnode-key left) (rbnode-key tree)))
                                                  (k "bad left key")
                                                  (loop left)))
                                            (unless (eq? rb-nil right)
                                              (if (not (compare (rbnode-key tree) (rbnode-key right)))
                                                  (k "bad right key")
                                                  (loop right))))))))
                         #t)))))

        (define rbtree-print
          (lambda (T)
            #f))

        (define rbtree-to-dot
          (lambda (T path)
            ;; if file exists, error
            (call-with-output-file path
              (lambda (p)
                (printf "dot file at ~a~n" path)
                (put-string p "digraph {")
                (fresh-line p)
                (put-string p "node [style=filled,color=black,fontcolor=white,fontname=monospace];")
                (fresh-line p)
                (let ([nodes '()])
                  (let ([tree (rbtree-root T)])
                    (unless (null? tree)
                      (let loop ([tree tree])
                        (let ([left (rbnode-left tree)]
                              [right (rbnode-right tree)])
                          (set! nodes (cons tree nodes))
                          (if (eq? rb-nil left)
                              (put-string p (format "~a -> ~a;~n" (rbnode-key tree) "NIL"))
                              (begin (put-string p (format "~a -> ~a;~n" (rbnode-key tree) (rbnode-key left)))
                                     (loop left)))
                          (if (eq? rb-nil right)
                              (put-string p (format "~a -> ~a;~n" (rbnode-key tree) "NIL"))
                              (begin (put-string p (format "~a -> ~a;~n" (rbnode-key tree) (rbnode-key right)))
                                     (loop right)))))))
                  ;; set node color
                  (for-each (lambda (x)
                              (put-string p (format "~a [fillcolor=~a];~n"
                                                    (rbnode-key x)
                                                    (if (is-red? x) "red" "black")))) nodes)
                  (put-string p "NIL [fillcolor=black];")
                  (fresh-line p))
                (put-string p "}")
                ))))

;;;;;;;;;;;;;;;;;;;;;
        ;; deprecated
;;;;;;;;;;;;;;;;;;;;;
        #|
        (define rbtree-insert!          ; ; ;
        (lambda (T k v)                 ; ; ;
        ;; if the tree is null, populate the root first ; ; ;
        (let ([tree (rbtree-root T)]    ; ; ;
        [new #f])                       ; ; ;
        (if (null? tree)                ; ; ;
        (rbtree-root-set! (make-rbnode BLACK rb-nil rb-nil rb-nil k v)) ; ; ;
        (let ([compare (rbtree-compare T)] ; ; ;
        [equal (rbtree-equal T)])       ; ; ;
        (let loop ([tree tree])         ; ; ;
        (let ([left (rbnode-left tree)] ; ; ;
        [right (rbnode-right tree)])    ; ; ;
        (cond [(equal k (rbnode-key tree)) ; ; ;
        (rbnode-value-set! tree v)]     ; ; ;
        [(compare k (rbnode-key tree))  ; ; ;
        (if (equal? left rb-nil)        ; ; ;
        (let ([l (mk-rbnode RED tree rb-nil rb-nil k v)]) ; ; ;
        (set! new l)                    ; ; ;
        (rbnode-left-set! tree l))      ; ; ;
        (loop (rbnode-left tree))]      ; ; ;
        [else (if (equal? right rb-nil) ; ; ;
        (let ([r (mk-rbnode RED tree rb-nil rb-nil k v)]) ; ; ;
        (set! new r)                    ; ; ;
        (rbnode-right-set! tree r))     ; ; ;
        (loop (rbnode-right tree)))]))))) ; ; ;
        (insert-fix! T new)))))         ; ; ;
                                        ; ; ;
                                        ; ; ;
                                        ; ; ;
        (define rotate-left!            ; ; ;
        (lambda (x)                     ; ; ;
        ;; assume that x's parent exists ; ; ;
        (let* ([y (rbnode-parent x)]    ; ; ;
        [y-left (rbnode-left y)]        ; ; ;
        [x-right (rbnode-right x)])     ; ; ;
        ;; set x's parent to y's parent ; ; ;
        (rbnode-parent-set! x (rbnode-parent y)) ; ; ;
        ;; set x's parent's child       ; ; ;
                                        ; ; ;
        ;; set y's parent to x          ; ; ;
        (rbnode-parent-set! y x)        ; ; ;
        ;; set y's left child to x's right child ; ; ;
        (rbnode-left-set! y x-right)    ; ; ;
        ;; if x's right child is not nil, update its parent to y ; ; ;
        (unless (eq? x-right rb-nil)    ; ; ;
        (rbnode-parent-set! x-right y))))) ; ; ;
                                        ; ; ;
        (define rotate-right!           ; ; ;
        (lambda (x)                     ; ; ;
        ;; assume that x's parent exists ; ; ;
        (let* ([y (rbnode-parent x)]    ; ; ;
        [y-right (rbnode-right y)]      ; ; ;
        [x-left (rbnode-left x)])       ; ; ;
        ;; set x's parent to y's parent ; ; ;
        (rbnode-parent-set! x (rbnode-parent y)) ; ; ;
        ;; set x's parent's child       ; ; ;
                                        ; ; ;
        ;; set y's parent to x          ; ; ;
        (rbnode-parent-set! y x)        ; ; ;
        ;; set y's right child to x's left child ; ; ;
        (rbnode-right-set! y x-left)    ; ; ;
        ;; if x's left child is not nil, update its parent to y ; ; ;
        (unless (eq? x-left rb-nil)     ; ; ;
        (rbnode-parent-set! x-left y))))) ; ; ;
        |#

        )
