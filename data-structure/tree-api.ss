#|
Unified Tree API

If a node's parent is tree-nil, then it's the root;
it a node's children are tree-nil, then it's then leaf.

If a tree is empty, then (null? (get-root T)) is #t.

tree-nil should also be a node, so its parent can be set
during transplantation.

TODO
pred and succ return the nearest when key doesn't exist?


;;; API
See the comments in the macro definition.

(define-tree-api tree-prefix get-root get-key get-value get-left get-right get-compare get-equal)
get-root: get the tree per se from the tree object
tree-prefix: a string that identifies the tree type, e.g., "rb" for red-black trees,
             "b" for b-trees
get-key: given a tree node, returns the key
get-value: given a tree node and key, returns the value associated with the key
get-right: given a tree node, returns the right child, or #f if none
get-left: given a tree node, returns the left child, or #f if none
get-compare: get the comparision procedure for keys
get-equal: get the equality comparision procedure for keys

define-tree-api is a macro that defines some procedures when given appropriate
arguments. For example, when invoked as:
(define-tree-api "rb" rb-node-key rb-node-value rb-node-left rb-node-right < =)
assuming that we have defined a red-black tree (node) by:
(define-record-type rb-node (fields color parent key value right left))
then define-tree-api will generate the following procedures for us:
(rbtree-search-internal T k) search for a certain key k in red-black tree T
(rbtree-leftmost-internal T) get the leftmost key and value in T, in the form (key . value),
        usually this is the minimum in the tree
(rbtree-rightmost-internal T) get the rightmost key and value in T, in the form (key . value)
        usually this is the maximum in the tree
(rbtree-predecessor-internal T k) get the predecessor of key k in T, in the form (key . value)
(rbtree-successor-internal T k) get the successor of key k in T, in the form (key . value)
(rbtree-insert!-internal T k v)
(rbtree-delete!-internal T k)
(rbtree-rotate-left!-internal n)
(rbtree-rotate-right!-internal n)
|#

(define-syntax define-tree-api
  (lambda (x)
    (define construct-name
      (lambda (stx prefix ss)
        (datum->syntax stx (string->symbol (format "~a~a-internal" (syntax->datum prefix) ss)))))
    (syntax-case x ()
      [(k prefix get-root  get-key  get-value  get-parent  get-left  get-right  get-compare get-equal
          set-root! set-key! set-value! set-parent! set-left! set-right! tree-nil make-node)
       ;; Without with-syntax, things defined in the module will be
       ;; renamed (alpha-converted) to some Marsian language.
       ;; Nneed to use #' to convert k and prefix to syntax objects.
       (with-syntax ([tree-search (construct-name #'k #'prefix "tree-search")]
                     [tree-leftmost (construct-name #'k #'prefix "tree-leftmost")]
                     [tree-rightmost (construct-name #'k #'prefix "tree-rightmost")]
                     [leftmost (construct-name #'k #'prefix "leftmost")]
                     [rightmost (construct-name #'k #'prefix "rightmost")]
                     [tree-predecessor (construct-name #'k #'prefix "tree-predecessor")]
                     [tree-successor (construct-name #'k #'prefix "tree-successor")]
                     [tree-insert! (construct-name #'k #'prefix "tree-insert!")]
                     [tree-delete! (construct-name #'k #'prefix "tree-delete!")]
                     [tree-rotate-left! (construct-name #'k #'prefix "tree-rotate-left!")]
                     [tree-rotate-right! (construct-name #'k #'prefix "tree-rotate-right!")]
                     [tree-print (construct-name #'k #'prefix "tree-print")]
                     [tree-to-dot (construct-name #'k #'prefix "tree-to-dot")]
                     [tree-map (construct-name #'k #'prefix "tree-map")]
                     [tree-for-each (construct-name #'k #'prefix "tree-for-each")]
                     [tree-fold-left (construct-name #'k #'prefix "tree-fold-left")]
                     [tree-fold-right (construct-name #'k #'prefix "tree-fold-right")])
                    #'(module (tree-search tree-leftmost tree-rightmost tree-print tree-to-dot
                                           tree-predecessor tree-successor tree-insert!
                                           tree-delete! tree-rotate-left! tree-rotate-right!)
                              (define nil? (lambda (x) (eq? x tree-nil)))
                              (define tree-root?
                                ;; Here I used to determine whether x's parent is nil,
                                ;; that was wrong since when the order of expressions in
                                ;; rotation procedures are not right, we could have wrong
                                ;; results. E.g., suppose x is the root, y is x's left child,
                                ;; and we are right-rotating x. If we update x's parent before
                                ;; determining whether x is the root, then the root will remain
                                ;; unchanged, and we will loose the other half of the tree.
                                (lambda (T x) (eq? x (get-root T))))
#|
                              (define make-tree
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
                              |#

                              (define tree-empty?
                                (lambda (T) (null? (get-root T))))

                              (define tree-search
                                (lambda (T k)
                                  (let ([tree (get-root T)]
                                        [compare (get-compare T)]
                                        [equal (get-equal T)])
                                    (let loop ([tree tree])
                                      (let ([key (get-key tree)])
                                        (cond [(equal k key) tree]
                                              [(compare k key) (let ([left (get-left tree)])
                                                                 (if (nil? left) #f (loop left)))]
                                              [else (let ([right (get-right tree)])
                                                      (if (nil? right) #f (loop right)))]))))))

                              (define tree-map
                                (lambda (T proc)
                                  (let ([tree (get-root T)])
                                    (if (null? tree)
                                        '()
                                        (let ([result '()])
                                          (let loop ([tree tree])
                                            (let ([left (get-left tree)]
                                                  [right (get-right tree)])
                                              (unless (nil? left)
                                                (loop left))
                                              (set! result (append result
                                                                   (list (proc (get-key tree)
                                                                               (get-value tree)))))
                                              (unless (nil? right)
                                                (loop right))))
                                          result)))))

                              (define tree-for-each
                                (lambda (T proc)
                                  (let ([tree (get-root T)])
                                    (unless (null? tree)
                                      (let loop ([tree tree])
                                        (let ([left (get-left tree)]
                                              [right (get-right tree)])
                                          (unless (nil? left)
                                            (loop left))
                                          (proc (get-key tree)
                                                (get-value tree))
                                          (unless (nil? right)
                                            (loop right))))))))

                              ;; proc: acc k v -> a
                              (define tree-fold-left
                                (lambda (T proc acc)
                                  (let ([tree (get-root T)])
                                    (if (null? tree)
                                        '()
                                        (let ([acc acc])
                                          (let loop ([tree tree])
                                            (let ([left (get-left tree)]
                                                  [right (get-right tree)])
                                              (unless (nil? left)
                                                (loop left))
                                              (set! acc
                                                    (proc acc (get-key tree) (get-value tree)))
                                              (unless (nil? right)
                                                (loop right))))
                                          acc)))))

                              ;; proc: k v acc -> a
                              (define tree-fold-right
                                (lambda (T proc acc)
                                  (let ([tree (get-root T)])
                                    (if (null? tree)
                                        '()
                                        (let ([acc acc])
                                          (let loop ([tree tree])
                                            (let ([left (get-left tree)]
                                                  [right (get-right tree)])
                                              (unless (nil? right)
                                                (loop right))
                                              (set! acc
                                                    (proc (get-key tree) (get-value tree) acc))
                                              (unless (nil? left)
                                                (loop left))))
                                          acc)))))

                              ;; search from the tree root
                              (define tree-leftmost
                                (lambda (T)
                                  (let ([tree (get-root T)])
                                    (if (null? tree)
                                        #f
                                        (leftmost tree)))))
                              (define tree-rightmost
                                (lambda (T)
                                  (let ([tree (get-root T)])
                                    (if (null? tree)
                                        #f
                                        (rightmost tree)))))

                              ;; search from a given node
                              (define leftmost
                                (lambda (node)
                                  (let loop ([node node])
                                    (let ([left (get-left node)])
                                      (if (nil? left)
                                          node
                                          (loop left))))))
                              (define rightmost
                                (lambda (node)
                                  (let loop ([node node])
                                    (let ([right (get-right node)])
                                      (if (nil? right)
                                          node
                                          (loop right))))))

                              (define tree-predecessor
                                (lambda (T k)
                                  (let ([tree (tree-search T k)])
                                    (if tree
                                        (let ([left (get-left tree)])
                                          ;; If the node has a left child, find the rightmost in it,
                                          ;; otherwise search upward for a parent that has a right
                                          ;; subtree in which our k-node resides.
                                          (if (nil? left)
                                              (let find-parent ([p tree])
                                                (if (tree-root? T p)
                                                    ;; If p is the root, then tree is the smallest,
                                                    ;; hence no predecessor.
                                                    #f
                                                    (if (eq? p (get-right (get-parent p)))
                                                        (get-parent p)
                                                        (find-parent (get-parent p)))))
                                              (rightmost left)))
                                        #f))))

                              (define tree-successor
                                (lambda (T k)
                                  (let ([tree (tree-search T k)])
                                    (if tree
                                        (let ([right (get-right tree)])
                                          ;; If the node has a right child, find the leftmost in it,
                                          ;; otherwise search upward for a parent that has a left
                                          ;; subtree in which our k-node resides.
                                          (if (nil? right)
                                              (let find-parent ([p tree])
                                                (if (tree-root? T p)
                                                    ;; If p is the root, then tree is the biggest,
                                                    ;; hence no successor.
                                                    #f
                                                    (if (eq? p (get-left (get-parent p)))
                                                        (get-parent p)
                                                        (find-parent (get-parent p)))))
                                              (leftmost right)))
                                        #f))))

                              ;; x is assumed to be a parent, so this procedure
                              ;; slides x down to the left and x's right child
                              ;; becomes x's parent. The same idea applies to
                              ;; right rotation.
                              (define tree-rotate-left!
                                (lambda (T x)
                                  (let* ([p (get-parent x)]
                                         [y (get-right x)]
                                         [y-left (get-left y)])
                                    (when (nil? y) (errorf "tree-rotate-left!" "y cannot be nil"))
                                    (printf "left rot ~a~n" (get-key x))
                                    (if (tree-root? T x)
                                        (set-root! T y)
                                        (if (eq? x (get-right p))
                                            (set-right! p y)
                                            (set-left! p y)))
                                    (set-parent! x y)
                                    (set-parent! y p)
                                    (set-left! y x)
                                    (set-right! x y-left)
                                    (unless (nil? y-left)
                                      (set-parent! y-left x)))))

                              (define tree-rotate-right!
                                (lambda (T x)
                                  (let* ([p (get-parent x)]
                                         [y (get-left x)]
                                         [y-right (get-right y)])
                                    (when (nil? y) (errorf "tree-rotate-right!" "y cannot be nil"))
                                    (printf "right rot ~a~n" (get-key x))
                                    (if (tree-root? T x)
                                        (set-root! T y)
                                        (if (eq? x (get-right p))
                                            (set-right! p y)
                                            (set-left! p y)))
                                    (set-parent! x y)
                                    (set-parent! y p)
                                    (set-right! y x)
                                    (set-left! x y-right)
                                    (unless (nil? y-right)
                                      (set-parent! y-right x)))))

                              ;; Returns #f when the key doesn't exist, or when the deleted
                              ;; node is the root, otherwise return the node that took the
                              ;; place of the deleted node.
                              ;;
                              ;; Find the node (k-node) that contains k as the key,
                              ;; if its left child is nil, replace it with the right child,
                              ;; if its right child is nil, replace it with the left child,
                              ;; if k-nodes's right child's left child is nil, then the right child replaces k-node,
                              ;; else we find k-node's successor, use it to replace k-node, but before that don't
                              ;; forget to replace the successor with its right child.
                              (define tree-delete!
                                (lambda (T k)
                                  ;; replace x with y
                                  ;; Transplantation renders x innaccessible both
                                  ;; from the parent and children afterwards.
                                  (define transplant!
                                    (lambda (x y)
                                      (if (tree-root? T x)
                                          (set-root! T y)
                                          (if (eq? x (get-right (get-parent x)))
                                              (set-right! (get-parent x) y)
                                              (set-left! (get-parent x) y)))
                                      (set-parent! y (get-parent x))))
                                  (let ([tree (tree-search T k)])
                                    (if tree
                                        (cond [(nil? (get-left tree))
                                               (transplant! tree (get-right tree))
                                               ;; The right side could be nil, in this case
                                               ;; we've deleted the root or a leaf.
                                               (if (eq? (get-root T) tree-nil)
                                                   (begin (set-root! T '())
                                                          #f)
                                                   ;; Return the node taking place of the deleted one
                                                   ;; so trees like rbtree can rebalance.
                                                   ;; Then tree will be garbage-collected sometime in the future)
                                                   (get-right tree))]
                                              [(nil? (get-right tree))
                                               (transplant! tree (get-left tree))
                                               (get-left tree)]
                                              ;; Here we separate the two cases where the right child
                                              ;; doesn't have and have a left child.
                                              [(nil? (get-left (get-right tree)))
                                               (let ([y (get-right tree)]
                                                     [x-left (get-left tree)])
                                                 (transplant! tree y)
                                                 (set-left! y x-left)
                                                 (set-parent! x-left y)
                                                 y)]
                                              [else (let* ([suc (leftmost (get-right tree))]
                                                           [suc-right (get-right suc)])
                                                      ;; replace, need a copy function
                                                      (set-key! tree (get-key suc))
                                                      (set-value! tree (get-value suc))
                                                      ;; suc's left child must be nil
                                                      ;; Since suc cannot have left child, no need to
                                                      ;; set the parent. The case where suc-right is nil
                                                      ;; is allowed, and it's parent will be set.
                                                      (transplant! suc suc-right)
                                                      suc-right)])
                                        #f))))

                              ;; Returns #f if the newly inserted node is the root, or
                              ;; if the key already exists. Otherwise returns the
                              ;; newly inserted node.
                              (define tree-insert!
                                (lambda (T k v)
                                  ;; if the tree is null, populate the root first
                                  (let ([tree (get-root T)]
                                        [new #f])
                                    (if (null? tree)
                                        (let ([n (make-node k v tree-nil)])
                                          ;; n is root, leave new as #f since there's nothing to fix
                                          (set-root! T n))
                                        (let ([compare (get-compare T)]
                                              [equal (get-equal T)])
                                          (let loop ([tree tree])
                                            (let ([left (get-left tree)]
                                                  [right (get-right tree)])
                                              (cond [(equal k (get-key tree))
                                                     ;; No need for post-processing,
                                                     ;; leave new as #f.
                                                     (set-value! tree v)]
                                                    [(compare k (get-key tree))
                                                     (if (eq? left tree-nil)
                                                         (let ([l (make-node k v tree)])
                                                           (set! new l)
                                                           (set-left! tree l))
                                                         (loop left))]
                                                     [else (if (eq? right tree-nil)
                                                               (let ([r (make-node k v tree)])
                                                                 (set! new r)
                                                                 (set-right! tree r))
                                                               (loop right))])))))
                                        ;; return the new node so trees like rbtree can rebalance
                                        new)))

                              (define tree-print
                                (lambda (T)
                                  (define print-helper
                                    (lambda (x indent last?)
                                      (define (ind indent)
                                        (let loop ([ls indent])
                                          (unless (null? ls)
                                            (if (car ls)
                                                (begin (printf "~a" "│  ")
                                                       (loop (cdr ls)))
                                                (begin (printf "~a" "   ")
                                                       (loop (cdr ls))))))
                                        (if last?
                                            (printf "└──")
                                            (printf "├──")))

                                      (ind indent)
                                      (if (nil? x)
                                          (printf "~a~n" "NIL")
                                          (let ([l (get-left x)]
                                                [r (get-right x)])
                                            (printf "~a~n" (get-key x))
                                            ;; (append ...): if the current node is the last of its parent,
                                            ;; don't print the vertical bar below it
                                            (print-helper l (append indent `(,(not last?))) #f)
                                            (print-helper r (append indent `(,(not last?))) #t)))))

                                  (let ([tree (get-root T)])
                                    (unless (null? tree)
                                      (begin (printf "~a~n" (get-key tree))
                                             (let ([l (get-left tree)]
                                                   [r (get-right tree)])
                                               (print-helper l '() #f)
                                               (print-helper r '() #t)))))))

                              (define tree-to-dot
                                (lambda (T path)
                                  (call-with-output-file path
                                    (lambda (p)
                                      (printf "dot file at ~a~n" path)
                                      (put-string p "digraph {")
                                      (fresh-line p)
                                      (put-string p "node [fontname=monospace];")
                                      (fresh-line p)
                                      (let ([tree (get-root T)])
                                        (unless (null? tree)
                                          (let loop ([tree tree])
                                            (let ([left (get-left tree)]
                                                  [right (get-right tree)])
                                              (unless (nil? left)
                                                (put-string p (format "~a -> ~a;~n" (get-key tree) (get-key left)))
                                                (loop left))
                                              (unless (nil? right)
                                                (put-string p (format "~a -> ~a;~n" (get-key tree) (get-key right)))
                                                (loop right))))
                                          ))
                                      (put-string p "}")))))
                              )) ;; with-syntax
       ])))
