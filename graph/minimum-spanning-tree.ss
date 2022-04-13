#|
Minimum Spanning Tree

;;; API
(graph-mst g)
Returns the minimum spanning tree of an undirected, connected, weighted graph g.

;;; Implementation
We implement three algorithms: Kruskal's algorithm, Prim's algorithm,
and Boruvka's algorithm. See below.
|#

(include "../data-structure/heap.ss")
(include "../data-structure/dset.ss")

(module (graph-mst)
  (define graph-mst
    (lambda (g select)
      ;; 1) put all edges in a min heap (or priority queue) order by their weights
      ;; 2) pop the heap, see if the two vertices in the edge are in the growing
      ;; tree, If yes, drop the edge, otherwise union the two vertices and
      ;; add the edge to the tree
      ;; 3) repeat until the min heap is empty
      (define kruskal
        (lambda (g)
          ;; e*: ((v1 x1 w1) (v2 x2 w2) (v3 x3 w3) ...)
          (let* ([e* (let ([e (graph-edges g)])
                       (if (cdr e)
                           (car e)
                           (errorf "graph-mst-kruskal" "graph not connected")))]
                 [edges (make-heap (lambda (x y) (< (caddr x) (caddr y))) e*)]
                 [mst '()]
                 [S (make-dset (graph-vertices g))]
                 [either (lambda (e) (car e))]
                 [other (lambda (e v) (if (= v (car e)) (cadr e) v))])
            (let loop ([h (heap-extract-top! edges)])
              (if h
                  (begin
                    (let* ([x (either h)]
                           [y (other h x)])
                      (if (dset=? S x y)
                          (loop (heap-extract-top! edges))
                          (begin
                            (dset-union! S x y)
                            (set! mst (cons h mst))
                            (loop (heap-extract-top! edges))))))
                  (make-graph mst #f #t))))))

      ;; Start with an arbirary vertex v (e.g. the 1st), scan all edges incident on it, pick the
      ;; one with the least weight to form the edge in the resulting MST, add others
      ;; to a min heap ordered by weight, mark v as #t.
      ;; Pop the heap, if one vertex in the edge is not marked, mark it and add the edge
      ;; If both are marked, continue. Repeat until the heap is empty.
      ;; In this way at any moment during the process we have in the heap all available
      ;; cross-edges to choose from. Since the heap is min heap by weight, we always choose
      ;; the lightest edge.
      ;;
      ;; However, for a tree edge v -- w with weight w, we can just store w along with
      ;; its parent v, with weight w: (v p w), ordered by w, and pop the heap as above,
      ;; in this way we're still choosing the lightest edge by popping heap.
      ;; For a tree edge e --> v, we set v's parent field to e, set v's weight field to
      ;; the weight of e -- v.
      (define prim
        (lambda (g)
          (let* ([in-heap (make-vector (graph-length g) #t)]
                 ;; element: (parent . weight)
                 [edges (make-vector (graph-length g) `(#f . ,(most-positive-fixnum)))]
                 [v* (graph-vertices g)]
                 [mst (make-graph (graph-order g) #f #t)]
                 [in-heap? (lambda (v) (vector-ref in-heap v))]
                 [renew-heap (lambda () (let ([h (make-heap (lambda (x y)
                                                              (< (cdr (vector-ref edges x))
                                                                 (cdr (vector-ref edges y))))
                                                            (graph-size g))])
                                          (for-each (lambda (v) (when (in-heap? v)
                                                                  (heap-insert! h v))) (cdr v*))
                                          h))]
                 [h (renew-heap)]
                 [pop-heap! (lambda () (let ([m (heap-extract-top! h)])
                                        (if m (begin (vector-set! in-heap m #f) m) #f)))])
            ;; choose the root vertex
            (heap-insert! h (car v*))
            (let loop ([e (pop-heap!)])
              (when e
                (for-each (lambda (v)
                            (when (and (in-heap? (car v)) (< (cdr v) (cdr (vector-ref edges (car v)))))
                              ;; update parent and weight
                              (vector-set! edges (car v) (cons e (cdr v)))
                              ;; make sure the heap is reordered (FIXME)
                              (set! h (renew-heap))))
                          (adjlist-w g e))
                (loop (pop-heap!))))
            (do ([i 0 (add1 i)])
                ((= i (vector-length edges)) mst)
              (let ([e (vector-ref edges i)])
                ;; avoid non-existant vertices and the root vertex
                (when (car e)
                  (if (= (cdr e) (most-positive-fixnum))
                      (errorf "graph-mst" "graph is not connected")
                      (graph-insert! mst i (car e) (cdr e)))))))))

      ;; Borůvka's algorithm is similar to Kruskal's. Actually Kruskal drew the inspiration from
      ;; Borůvka, see [Algorithms p.266].
      ;; Whereas Kruskal's uses a disjoint set to determine whether two vertices are in the
      ;; same component, Borůvka's algorithm computes the number of components of a graph derived from
      ;; the original graph but with no edges repeatedly and label the vertices with their components.
      ;; Then the algorithm go through the edges in the original graph to find in each
      ;; component a least-weighted edge for inclusion into the MST. The algorithm stops
      ;; when there's only one component, the MST.
      (define boruvka
        (lambda (g)
          (let ([mst (make-graph (graph-vertices g) #f #t)]
                ;; store the component number of each vertex
                [comp (make-vector (graph-length g) #f)]
                ;; edges for inclusion into components
                [safe-edge (make-vector (graph-order g) #f)]
                [t* '()]
                [edges (let ([e (graph-edges g)])
                         (if (cdr e)
                             (car e)
                             (errorf "graph-mst-kruskal" "graph not connected")))])
            (define get-comp (lambda (v) (vector-ref comp v)))
            (define set-comp! (lambda (v cc) (vector-set! comp v cc)))
            (define compute-cc
              (lambda ()
                (let ([cc 0]
                      [state (make-vector (graph-length mst) #f)])
                  (define dfs
                    (lambda (v)
                      (vector-set! state v #t)
                      (for-each (lambda (x)
                                  (unless (vector-ref state x)
                                    (set-comp! x cc)
                                    (dfs x)))
                                (adjlist mst v))))
                  (for-each (lambda (v)
                              (unless (vector-ref state v)
                                (set-comp! v cc)
                                (dfs v)
                                (set! cc (add1 cc))))
                            (graph-vertices mst))
                  cc)))
            (do ([count (compute-cc) (compute-cc)])
                ((= count 1) mst) ;; stop when only one component in mst
              ;; don't forget to reset this
              (set! safe-edge (make-vector (graph-order g) #f))
              (let loop ([e* edges])
                ;; e*: ((v1 n1 w1) (v1 n2 w2) (v3 n3 w3) ...)
                (if (null? e*)
                    ;; add collected edges to the tree
                    (do ([j 0 (add1 j)])
                        ((= j count))
                      (let ([e (vector-ref safe-edge j)])
                        (graph-insert! mst (car e) (cadr e) (caddr e))))
                    (let ([e (car e*)]
                          [comp-1 (get-comp (caar e*))]
                          [comp-2 (get-comp (cadar e*))]
                          [w (caddar e*)])
                      (unless (= comp-1 comp-2)
                        ;; update safe edges for components
                        (when (if (not (vector-ref safe-edge comp-1)) #t (< w (caddr (vector-ref safe-edge comp-1))))
                          (vector-set! safe-edge comp-1 e))
                        (when (if (not (vector-ref safe-edge comp-2)) #t (< w (caddr (vector-ref safe-edge comp-2))))
                          (vector-set! safe-edge comp-2 e)))
                      (loop (cdr e*)))))))))
      (if (and (not (graph-directed? g)) (graph-weighted? g))
          (case select
            [(k) (kruskal g)]
            [(p) (prim g)]
            [(b) (boruvka g)])
          (errorf "graph-mst" "graph needs to be undirected and weighted"))))

  )
