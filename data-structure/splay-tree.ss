#|
Splay Tree

;;; API
(make-sptree)
(sptree-search T k)
(sptree-search T k)
(sptree-insert! T k v)
(sptree-delete! T k)
(sptree-for-each T proc)

;;; Implementation
|#

(module (make-sptree sptree-search sptree-insert! sptree-delete! sptree-print sptree-to-dot)
        (include "tree-api.ss")
        #|
        (k prefix get-root  get-key  get-value  get-parent  get-left  get-right  get-compare get-equal
        set-root! set-key! set-value! set-parent! set-left! set-right! tree-nil make-node)
        |#
        (define-tree-api "sp" tree-root spnode-key spnode-value spnode-parent
          spnode-left spnode-right tree-compare tree-equal
          tree-root-set! spnode-key-set! spnode-value-set!
          spnode-parent-set! spnode-left-set! spnode-right-set! spnil
          (lambda (k v p) (make-spnode p nil nil k v)))

        (define-record-type spnode
          (fields (mutable parent) (mutable left)
                  (mutable right) (mutable key) (mutable value)))
        (define-record-type (sptree mk-sptree sptree?)
          (fields compare equal (mutable root)))
        (define spnil (make-node #f #f #f #f #f))

        (define make-sptree
          (lambda ()
            #f))
        #|
        (define sptree-predecessor
        (lambda (T k)
        (let ([p (sptree-predecessor-internal T k)])
        (if p
        (node-key p)
        #f))))
        (define sptree-successor
        (lambda (T k)
        (let ([s (sptree-successor-internal T k)])
        (if s
        (node-key s)
        #f))))
        (define sptree-leftmost
        (lambda (T)
        (let ([r (sptree-leftmost-internal T)])
        (if r
        (node-key r)
        #f))))
        (define sptree-rightmost
        (lambda (T)
        (let ([r (sptree-rightmost-internal T)])
        (if r
        (node-key r)
        #f))))
        |#
        (define sptree-search
          (lambda (T k)
            (let ([r (sptree-search-internal T k)])
              (if r
                  (node-key (splay! T r))
                  #f))))

        (define sptree-insert!
          (lambda (T k v)
            (let ([new (sptree-insert!-internal T k)])
              ;; if the key already exists, it won't be splayed
              (when new
                (splay! T new)))))

        (define sptree-delete!
          (lambda (T k)
            (define set-root!
              (lambda (T x)
                (sptree-root-set! T k)
                (spnode-parent-set! k spnil)))
            (let ([old (sptree-search-internal T k)])
              ;; In the case of deletion, we first splay the node being deleted
              ;; to the root, then delete it, then we'll end up having two
              ;; trees——the left and right subtrees of the root.
              ;; If the left tree is not nil, take the max node and splay it as
              ;; the  new root. If the left tree's root T has no right child, then
              ;; T is the root of the whole tree. If the left tree is nil,
              ;; the right tree is the new tree itself.
              (when old
                (splay! T old)
                (let ([left (spnode-left old)]
                      [right (spnode-right old)])
                  (if (eq? left spnil)
                      (if (eq? right spnil)
                          (sptree-root-set! '())
                          (set-root! right))
                      (let ([m (spleftmost-internal left)])
                        (if (eq? (spnode-parent m) old)
                            (begin (spnode-right-set! right)
                                   (set-root! m))
                            (begin
                              ;; ...so that splay! works correctly
                              (set-root! T left)
                              (splay! T m)
                              ;; Since m is the maximum node, after splaying
                              ;; m's right child is nil.
                              (spnode-right-set! right))))))))))

        ;; returns the node being splayed
        (define splay!
          (lambda (T x)
            (let loop ([x x])
              (if (eq? x (sptree-root T))
                  x
                  (let ([p (spnode-parent x)])
                    (if (eq? p (sptree-root T))
                        (begin (is-right-child? x)
                               (rotate-left! p)
                               (rotate-right! p))
                        (if (is-right-child? p)
                            (if (is-right-child? x)
                                (begin (rotate-left! (spnode-parent p))
                                       (rotate-left! p))
                                (begin (rotate-right! p)
                                       ;; Note that in this triangle case after the
                                       ;; first rotation the parent of x is changed.
                                       (rotate-left! (spnode-parent x))))
                            (if (is-right-child? x)
                                (begin (rotate-left! p)
                                       (rotate-right! (spnode-parent x)))
                                (begin (rotate-right! (spnode-parent p))
                                       (rotate-right! p)))))))
              (loop x))))

        (define is-right-child?
          (lambda (x)
            (if (eq? (spnode-parent x) spnil)
                (errorf "is-right-child?" "argument is already the root")
                (eq? (spnode-right (spnode-parent x)) x))))
        (define rotate-left! sptree-rotate-left!-internal)
        (define rotate-right! sptree-rotate-right!-internal)
        (define sptree-print sptree-print-internal)
        (define sptree-to-dot sptree-to-dot-internal)

        )
