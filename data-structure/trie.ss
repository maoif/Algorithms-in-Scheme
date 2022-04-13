#|
Trie

;;; API
(make-trie)
(make-trie kvs)
(trie-search k)
(trie-insert! T k v)
(trie-delete! T k)

;;; Implementation
A trie associates strings (keys) with values.

Each node is represented as a pair (value . children).
Here we use a hashtable to maintain the children of
each node. If children is '(), then the current node
is a leaf node.
|#

(define-record-type (trie mk-trie trie?)
    (fields root))

(define make-trie
  (case-lambda
   [() (mk-trie (make-eq-hashtable))]
   [(kvs) (let ([T (mk-trie (make-eq-hashtable))])
            (for-each (lambda (kv)
                        (if (pair? kv)
                            (if (string? (car kv))
                                (trie-insert! T (car kv) (cdr kv))
                                (errorf "make-trie" "key has to be string"))
                            (errorf "make-trie" "invalid representation of key-value")))
                      kvs)
            T)]))

(define trie-search
  (lambda (T k)
    (let ([key (string->list k)])
      (if (> (length key) 0)
          (let loop ([tree (trie-root T)]
                     [k* key])
            (if (hashtable-contains? tree (car k*))
                (if (null? (cdr k*))
                    ;; only return the value
                    (car (hashtable-ref tree (car k*) '()))
                    ;; the node may not have children
                    (if (cdr (hashtable-ref tree (car k*) #f))
                        (loop (cdr (hashtable-ref tree (car k*) #f))
                              (cdr k*))
                        #f))
                #f))
          (errorf "trie-search" "empty key")))))

(define trie-insert!
  (lambda (T k v)
    (let ([key (string->list k)])
      (if (> (length key) 0)
          (let loop ([tree (trie-root T)]
                     [k* key])
            (let ([end? (null? (cdr k*))])
              (if (hashtable-contains? tree (car k*))
                  (if end?
                      (hashtable-set! tree (car k*)
                                      ;; don't forget the children
                                      (cons v (hashtable-ref tree (car k*) #f)))
                      (loop (cdr (hashtable-ref tree (car k*) #f))
                            (cdr k*)))
                  ;; just set the value of the prefix node to #f
                  (begin (hashtable-set! tree (car k*)
                                         (cons (if end? v #f)
                                               (if end? #f (make-eq-hashtable))))
                         (unless end?
                           (loop (cdr (hashtable-ref tree (car k*) #f)) (cdr k*)))))))
          (errorf "trie-insert!" "empty key")))))

;; use recursion to back up
(define trie-delete!
  (lambda (T k)
    (call/cc (lambda (c)
               (let loop ([tree (trie-root T)]
                          [ks (string->list k)])
                 (if (hashtable-contains? tree (car ks))
                     (let ([node (hashtable-ref tree (car ks) #f)])
                       (if (null? (cdr ks))
                           (begin (set-car! node #f)
                                  (unless (cdr node)
                                    (hashtable-delete! tree (car ks))))
                           (if (cdr node)
                               ;; node has chhildren hashtable
                               (begin (loop (cdr node) (cdr ks))
                                      (when (= 0 (vector-length (hashtable-keys (cdr node))))
                                        (unless (car node)
                                          (hashtable-delete! tree (car ks)))))
                               (c #f))))
                     (c #f)))))))
