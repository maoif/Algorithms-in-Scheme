#|
AVL tree
AVL stands its inventors: Adelson-Velsky and Landis

;;; API
(make-avltree compare equal)
(avltree-insert! t k v)
(avltree-search t k)
(avltree-delete! t k)
(avltree-for-each t proc)

;;; Implementation
AVL tree is height balanced, for each node,
the heights of the left and right subtrees differ by at
most 1. The height balanced is maintained using tree rotation.

|#
(module (make-avltree avltree-search avltree-insert! avltree-delete! avltree-predecessor avltree-successor
                      avltree-leftmost avltree-rightmost avltree-print)
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

        (include "tree-api.ss")

        (define-tree-api "avl"
          avltree-root avlnode-key avlnode-value avlnode-parent avlnode-left avlnode-right avltree-compare avltree-equal
          avltree-root-set! avlnode-key-set! avlnode-value-set! avlnode-parent-set! avlnode-left-set! avlnode-right-set!
          avl-nil (lambda (k v p) (mk-avlnode p avl-nil avl-nil k v 1))) ;; default height is 1

        (define-record-type (avlnode mk-avlnode avlnode?)
          (fields (mutable parent) (mutable left) (mutable right)
                  (mutable key) (mutable value) (mutable height)))

        (define-record-type (avltree mk-avltree avltree?)
          (fields compare equal (mutable root)))

        (define avl-nil (mk-avlnode #f #f #f #f #f 0))

        (define BF (lambda (x) (- (avlnode-height (avlnode-left x))
                                  (avlnode-height (avlnode-right x)))))

        (define-who make-avltree
          (lambda (compare equal kvs)
            (let ([T (mk-avltree compare equal '())])
              (for-each (lambda (kv)
                          (if (pair? kv)
                              (avltree-insert! T (car kv) (cdr kv))
                              (errorf "make-avltree" "invalid key-value representation: ~a" kv)))
                        kvs)
              T)))
        (define avltree-search
          (lambda (T k)
            (let ([r (avltree-search-internal T k)])
              (if r
                  (cons (avlnode-key r) (avlnode-value r))
                  #f))))

        (define avltree-predecessor
          (lambda (T k)
            (let ([r (avltree-predecessor-internal T k)])
              (if r
                  (cons (avlnode-key r) (avlnode-value r))
                  #f))))
        (define avltree-successor
          (lambda (T k)
            (let ([r (avltree-successor-internal T k)])
              (if r
                  (cons (avlnode-key r) (avlnode-value r))
                  #f))))
        (define avltree-leftmost
          (lambda (T k)
            (let ([r (avltree-leftmost-internal T)])
              (if r
                  (cons (avlnode-key r) (avlnode-value r))
                  #f))))
        (define avltree-rightmost
          (lambda (T k)
            (let ([r (avltree-rightmost-internal T)])
              (if r
                  (cons (avlnode-key r) (avlnode-value r))
                  #f))))

        ;; Since we are not using recursive insertion, we have to travel up to
        ;; root and update the height along the way.
        ;; This is not time-consuming because the normal recursive insertion
        ;; also dives down and goes up.
        (define-who avltree-insert!
          (lambda (T k v)
            (define is-right-child?
              (lambda (x) (eq? x (avlnode-right (avlnode-parent x)))))
            (define-who insertion-fix!
              (lambda (x)
                (let ([comp (avltree-compare T)])
                  ;;(printf "~a: ~a root: ~a~n" who (avlnode-key x) (eq? x (avltree-root T)))
                  (let loop ([p (avlnode-parent x)]
                             [round 1])
                    (update-height! p)
                    ;;(printf "parent ~a height ~a round ~a~n" (avlnode-key p) (avlnode-height p) round)
                    (cond [(> (BF p) 1)
                           (if (comp k (avlnode-key (avlnode-left p)))
                               ;; If there's imbalance, it'll show up at the
                               ;; grandparent of x.
                               (rotate-right! T p)
                               (begin (rotate-left! T (avlnode-left p))
                                      (rotate-right! T p)))]
                          [(< (BF p) -1)
                           (if (comp k (avlnode-key (avlnode-right p)))
                               (begin (rotate-right! T (avlnode-right p))
                                      (rotate-left! T p))
                               (rotate-left! T p))])
                    (unless (eq? p (avltree-root T))
                      (loop (avlnode-parent p) (add1 round)))))
                ))
            ;;(printf "~a~n" who)
            (let ([new (avltree-insert!-internal T k v)])
              (when new
                (insertion-fix! new)))))

        (define-who avltree-delete!
          (lambda (T k)
            (define-who deletion-fix!
              (lambda (x)
                ;;(printf "~a: ~a~n" who (avlnode-key x))
                (let loop ([p (if (eq? x avl-nil)
                                  ;; When the node that replaced the deleted one
                                  ;; is not nil, we start from it, otherwise we
                                  ;; start from its parent.
                                  (avlnode-parent x)
                                  x)])
                  (update-height! p)
                  (cond [(> (BF p) 1) (if (>= (BF (avlnode-left p)) 0)
                                          (rotate-right! T p)
                                          (begin (rotate-left! T (avlnode-left p))
                                                 (rotate-right! T p)))]
                        [(< (BF p) -1) (if (<= (BF (avlnode-right p)) 0)
                                           (rotate-left! T p)
                                           (begin (rotate-right! T (avlnode-right p))
                                                  (rotate-left! T p)))])
                  (unless (eq? p (avltree-root T))
                    (loop (avlnode-parent p))))))
            ;;(printf "~a~n" who)
            (let ([old (avltree-delete!-internal T k)])
              (when old
                (deletion-fix! old)))))

        (define rotate-left!
          (lambda (T x)
            (let ([y (avlnode-right x)])
              (avltree-rotate-left!-internal T x)
              (update-height! x)
              (update-height! y))))
        (define rotate-right!
          (lambda (T x)
            (let ([y (avlnode-left x)])
              (avltree-rotate-right!-internal T x)
              (update-height! x)
              (update-height! y))))
        (define update-height!
          (lambda (x)
            (unless (eq? x avl-nil)
              (avlnode-height-set! x (add1 (max (avlnode-height (avlnode-left x))
                                                (avlnode-height (avlnode-right x))))))))

        (define avltree-print avltree-print-internal)
        ) ;; module
;; todo api for making trees, copy!
