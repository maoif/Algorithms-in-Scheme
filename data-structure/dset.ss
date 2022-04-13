#|
Disjoint Set

;;; API
(make-dset ls)
Make a disjoint set from a list/vector ls of objects.

(dset-union! S x y)
Union the two sets containing x and y. Throws exception if
either x or y doesn't exist.

(dset-find S x)
Returns the set containing x, #f if x doesn't exist.

(dset=? S x y)
Determine whether the sets containing x and y are the same.

;;; Implementation
We use two optimizations, union-by-rank and path compression,
see below.

The disjoint set is a hashtable. Each element is a key in the table.
The value for each key is a pair (p . r), p is the parent of the key
in the table, r is the rank (the longest path from the key to its leaves).

Every element are from the outset in their own sets. As dset-union! is called,
they are merged in one set. After N - 1 (N is the number of elements), all elements
are in one big set.

Disjoint sets can represent transitive relations of objects. For example, if two
vertices are in the same dset, then they are in the same graph component.
|#

(module (make-dset dset-union! dset-find dset=?)

  (define make-dset
    (lambda (ls)
      (if (or (list? ls) (vector? ls))
          (let ([ls (if (list? ls) (list->vector ls) ls)]
                [ht (make-eq-hashtable)])
            (vector-for-each (lambda (x) (hashtable-set! ht x `(,x . 0))) ls)
            ht)
          (errorf "make-dset" "argument should be list/vector: ~a" ls))))

  (define dset-union!
    (lambda (S x y)
      (let ([rx (dset-find S x)]
            [ry (dset-find S y)])
        (if (and rx ry)
            ;; Union-by-rank:
            ;; Always attach a low-rank root to a high rank one.
            ;; If they are equal, choose the first.
            (unless (equal? rx ry)
              (let ([rank-x (get-rank S ry)]
                    [rank-y (get-rank S rx)])
                (if (< rank-x rank-y)
                    (set-parent! S ry rx)
                    (begin (set-parent! S rx ry)
                           (when (= rank-x rank-y)
                             (set-rank! S ry (add1 (get-rank S ry))))))))
            (errorf "dset-union" "invalid dset element: ~a or ~a" x y)))))

  (define dset-find
    (lambda (S x)
      (define root?
        (lambda (x)
          (equal? x (get-parent S x))))
      (if (hashtable-contains? S x)
          (if (root? x)
              x
              (begin
                ;; Path compression:
                ;; After we obtain the parent of x if it is not
                ;; a root, we update its parent to point to the root,
                ;; so the next dset-find on x becomes faster.
                (set-parent! S x (dset-find S (get-parent S x)))
                (get-parent S x)))
          #f)))

  (define dset=?
    (lambda (S x y)
      ;; Use equal? so that integers, booleans, chars and strings
      ;; are all comparable.
      (equal? (dset-find S x) (dset-find S y))))

  (define get-parent
    (lambda (S x)
      (car (hashtable-ref S x '(#f . 0)))))
  (define get-rank
    (lambda (S x)
      (cdr (hashtable-ref S x '(#f . 0)))))
  (define set-parent!
    (lambda (S x p)
      (set-car! (cdr (hashtable-cell S x #f)) p)))
  (define set-rank!
    (lambda (S x r)
      (set-cdr! (cdr (hashtable-cell S x #f)) r)))
  )
