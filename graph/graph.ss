#|
Graph

;;; API
(make-graph ls [directed? [weighted?]])
Make a graph with vertices given in vector or list `ls`,
the elements must be natural numbers; no edges are created.

If the given `ls` contains pairs of the form (x . y) wherex and
y are natural numbers, then the procedure considers it to be a
vector/list of edges and create the graph with edges, too.

If directed?, the edges are added in one direction, otherwise
the edges are added both ways.

If weighted?, and ls are edges, then they must be of the form:
((x1 y1 w1) (x2 y2 w2) (x3 y3 w3) ...), w* being weights.

By default, make-graph creates undirected, unweighted graph.

(graph-insert! g x)
Add a vertex x into graph g.

(graph-insert! g x y)
Add an edge x -- y into graph g; non-existent vertices will be created first.

(graph-insert! g x y w)
Add an edge x -- y with weight w into graph g; non-existent vertices will be
created first. The graph has to be weighted of course.

(graph-remove! g v)
Remove the vertex from graph g.

(graph-remove! g x y)
Remove the edge x -- y from graph g.

(graph-cyclic? g)
Determines whether graph g contains cycles.

(dag? g)
Determines whether graph g is a directed acyclic graph.

(graph-vertices g)
Returns a list of vertices in graph g.

(graph-edges g)
Returns a pair whose car is the list of edges in the graph,
whose cdr indicates whether the graph is connected.
If the graph is weighted, the car is a list of lists of
the form (u v weight)

(graph-transpose g)
Returns the transpose of digraph g, i.e., the digraph
with all edges in g inverted

(graph-print g)
Print the graph.

(graph-to-dot g path)
Generate a dot file from graph g.

(adjlist g v)
Returns the adjacency list of v, for both weighted and unweighted graph.

(adjlist-w g v)
Returns the adjacency list of v, if weighed then with weight: (neighbor . weight),
otherwise the same as adjlist.

(graph-order g)
Number of vertices of graph g.
(graph-size g)
Number of edges of graph g.
(graph-length g)
Length of underlying vector.
(graph-resize! g)
Expand the graph vector.

;;; Implementation
Implementation follows that in [Manual].

If the value of your vertex itself is stored in the graph, then every time
a new edge is added, we have to do a search first to see if the incident
vectices already exist, when is slow.

Instead, we let the vertices be numbers, and it is the user's
job to maintain the mapping between numbers and the values they
actually represent.
|#

(library (graph)
  (export make-graph graph-insert! graph-remove! graph-print graph-to-dot
          graph-order graph-size graph-length graph-vertices graph-edges
          graph-cyclic? dag? adjlist vertex-exists?
          shortest-path topological-sort graph-mst graph-transpose graph-cc graph-scc)
  (import (chezscheme))

  (define-record-type (graph mkgraph graph?)
    (fields (mutable order) (mutable size) directed? weighted?
            ;; Vector of adjacency lists; element is #f if the vectex doesn't exist.
            ;; For weighted graph, each element in v* is: ((neighbor0 . w0) (neighbor1 . w1) ...)
            (mutable v*)
            ;; Degree of each vertex; length must be the same as v*.
            ;; For directed graph, every element of d* is of the form (in-degree . out-degree).
            (mutable d*)))

  (define make-graph
    (case-lambda
      [(ls) (cond
             [(vector? ls)
              (cond [(pair? (vector-ref ls 0))
                     (make-graph-from-edges (vector->list ls) #f #f)]
                    [(integer? (vector-ref ls 0))
                     (make-graph-from-vertices (vector->list ls) #f #f)]
                    [else (errorf "make-graph" "invalid argument")])]
             [(list? ls)
              (cond [(pair? (car ls))
                     (make-graph-from-edges ls #f #f)]
                    [(integer? (car ls))
                     (make-graph-from-vertices ls #f #f)]
                    [else (errorf "make-graph" "invalid argument")])]
             [(and (integer? ls) (> ls 0)) (make-graph-from-size ls #f #f)]
             [else (errorf "make-graph" "invalid argument")])]
      [(ls directed?) (cond
                       [(vector? ls)
                        (cond [(pair? (vector-ref ls 0))
                               (make-graph-from-edges (vector->list ls) directed? #f)]
                              [(integer? (vector-ref ls 0))
                               (make-graph-from-vertices (vector->list ls) directed? #f)]
                              [else (errorf "make-graph" "invalid argument")])]
                       [(list? ls)
                        (cond [(pair? (car ls))
                               (make-graph-from-edges ls directed? #f)]
                              [(integer? (car ls))
                               (make-graph-from-vertices ls directed? #f)]
                              [else (errorf "make-graph" "invalid argument")])]
                       [(and (integer? ls) (> ls 0)) (make-graph-from-size ls directed? #f)]
                       [else (errorf "make-graph" "invalid argument")])]
      [(ls directed? weighted?)
       (if (not weighted?)
           (cond
            [(vector? ls) (cond [(pair? (vector-ref ls 0))
                                 (make-graph-from-edges (vector->list ls) directed? #f)]
                                [(integer? (vector-ref ls 0))
                                 (make-graph-from-vertices (vector->list ls) directed? #f)]
                                [else (errorf "make-graph" "invalid argument")])]
            [(list? ls) (cond [(pair? (car ls))
                               (make-graph-from-edges ls directed? #f)]
                              [(integer? (car ls))
                               (make-graph-from-vertices ls directed? #f)]
                              [else (errorf "make-graph" "invalid argument")])]
            [(and (integer? ls) (> ls 0)) (make-graph-from-size ls directed? #f)]
            [else (errorf "make-graph" "invalid argument")])
           (cond
            [(vector? ls) (cond [(integer? (vector-ref ls 0))
                                 (make-graph-from-vertices (vector->list ls) directed? weighted?)]
                                [(and (list? (vector-ref ls 0)) (= (length (vector-ref ls 0)) 3))
                                 (make-graph-from-edges (vector->list ls) directed? weighted?)]
                                [else (errorf "make-graph" "invalid argument")])]
            [(list? ls) (cond [(integer? (car ls))
                               (make-graph-from-vertices ls directed? weighted?)]
                              [(and (list? (car ls)) (= (length (car ls)) 3))
                               (make-graph-from-edges ls directed? weighted?)]
                              [else (errorf "make-graph" "invalid argument")])]
            [(and (integer? ls) (> ls 0)) (make-graph-from-size ls directed? weighted?)]
            [else (errorf "make-graph" "invalid argument")]))]))

  (define make-graph-from-vertices
    (lambda (ls directed? weighted?)
      (let* ([m (apply max ls)]
             [v (make-vector (add1 m) #f)])
        (let loop ([ls ls])
          (unless (null? ls)
            ;; just initialize the slots of given vertives to '() (not 0!)
            (if (valid-vertex? (car ls))
                (begin (vector-set! v (car ls) '())
                       (loop (cdr ls)))
                (errorf "make-graph-internal" "invalid vertex number: ~a" (car ls)))))
        (mkgraph (length ls) 0 directed? weighted? v (make-vector (add1 m) (if directed? '(0 . 0) 0))))))

  ;; Undirected graph stores edges both ways,
  ;; if v doesn't exist(#f in the slot), override the slot with adjacency list,
  ;; if v has adjacency list, take it, cons the neighbor and the cdr of the pair,
  ;; and set the slot to the new list.
  (define make-graph-from-edges
    (lambda (ls directed? weighted?)
      ;; unweighted: ls = '((x1 . y1) (x2 . y2) (x3 . y3) ...)
      ;; weighted:   ls = '((x1 y1 w1) (x2 y2 w2) (x3 y3 w3) ...)
      (let* ([n (fxsrl (length ls) 1)]
             [g (mkgraph 0 0 directed? weighted? (make-vector n #f) (make-vector n (if directed? '(0 . 0) 0)))])
        (if weighted?
            (let loop ([ls ls])
              (if (null? ls)
                  g
                  (if (not (and (list? (car ls)) (= 3 (length (car ls)))))
                      (errorf "make-graph-internal" "invalid edge representation: ~a" (car ls))
                      (let ([x (caar ls)]
                            [y (cadar ls)]
                            [w (caddar ls)])
                        (graph-insert-edge! g x y w)
                        (loop (cdr ls))))))
            (let loop ([ls ls])
              (if (null? ls)
                  g
                  (if (not (pair? (car ls)))
                      (errorf "make-graph-internal" "invalid edge representation: ~a" (car ls))
                      (let ([x (caar ls)]
                            [y (cdar ls)])
                        (graph-insert-edge! g x y)
                        (loop (cdr ls))))))))))

  (define make-graph-from-size
    (lambda (ls directed? weighted?)
      (mkgraph 0 0 directed? weighted? (make-vector ls #f) (make-vector ls (if directed? '(0 . 0) 0)))))

  (define graph-insert!
    (case-lambda
      [(g v) (graph-insert-vertex! g v)]
      [(g x y) (if (graph-weighted? g)
                   (errorf "graph-insert!" "graph is weighted, missing weight")
                   (graph-insert-edge! g x y))]
      [(g x y w) (if (graph-weighted? g)
                     (graph-insert-edge! g x y w)
                     (errorf "graph-insert!" "graph is unweighted"))]))

  ;; if vertex is out of range, resize the graph first
  ;; if vertex doesn't exist, insert '() in its slot, so it comes into being
  ;; if vertex exists, do nothing
  (define graph-insert-vertex!
    (lambda (g v)
      (if (valid-vertex? v)
          (unless (vertex-exists? g v)
            ;; what if one resizing is not enough?
            (let loop ()
              (if (<= (graph-length g) v)
                  (begin (graph-resize! g) (loop))
                  (begin (vector-set! (graph-v* g) v '())
                         (graph-order-set! g (add1 (graph-order g)))))))
          (errorf "graph-insert-vertex!" "invalid vertex: ~a" v))))

  (define graph-insert-edge!
    (case-lambda
      [(g x y)
       (when (= x y)
         (errorf "graph-insert-edge!" "no loops allowed: ~a -- ~a" x y))
       (if (and (valid-vertex? x) (valid-vertex? y))
           (begin (unless (vertex-exists? g x) (graph-insert-vertex! g x))
                  (unless (vertex-exists? g y) (graph-insert-vertex! g y))
                  (unless (adjacent? (adjlist g x) y)
                    (if (graph-directed? g)
                        ;; only update x's list, then x's out-degree, y's in-degree
                        (begin  (vector-set! (graph-v* g) x (cons y (adjlist g x)))
                                ;; (make-vector n '(0 . 0)) makes '(0 . 0) shared across all slots,
                                ;; so we can't simply use set-car! and set-cdr! to update
                                ;; the in- and out-degrees.
                                (let* ([x-d (vector-ref (graph-d* g) x)]
                                       [y-d (vector-ref (graph-d* g) y)]
                                       [x-in (car x-d)] [x-out (cdr x-d)]
                                       [y-in (car y-d)] [y-out (cdr y-d)])
                                  (vector-set! (graph-d* g) x (cons x-in (add1 x-out)))
                                  (vector-set! (graph-d* g) y (cons (add1 y-in) y-out)))
                                (graph-size-set! g (add1 (graph-size g))))
                        ;; update both x's and y's lists, and degree
                        (let again ([x x]
                                    [y y]
                                    [d? (graph-directed? g)])
                          ;; update adjacency list
                          (vector-set! (graph-v* g) x (cons y (adjlist g x)))
                          ;; update degree
                          (vector-set! (graph-d* g) x (add1 (vector-ref (graph-d* g) x)))
                          ;; note the subtlety of updating the size
                          (if (not d?)
                              (again y x #t)
                              (graph-size-set! g (add1 (graph-size g))))))))
           (errorf "graph-insert-edge!" "invalid edge: ~a -- ~a" x y))]
      [(g x y w)
       (when (= x y)
         (errorf "graph-insert-edge!" "no loops allowed: ~a -- ~a" x y))
       (if (and (valid-vertex? x) (valid-vertex? y))
           (begin (unless (vertex-exists? g x) (graph-insert-vertex! g x))
                  (unless (vertex-exists? g y) (graph-insert-vertex! g y))
                  (unless (adjacent? (adjlist g x) y)
                    (if (graph-directed? g)
                        ;; only update x's list, then x's out-degree, y's in-degree
                        (begin  (vector-set! (graph-v* g) x (cons (cons y w) (adjlist-w g x)))
                                ;; (make-vector n '(0 . 0)) makes '(0 . 0) shared across all slots,
                                ;; so we can't simply use set-car! and set-cdr! to update
                                ;; the in- and out-degrees.
                                (let* ([x-d (vector-ref (graph-d* g) x)]
                                       [y-d (vector-ref (graph-d* g) y)]
                                       [x-in (car x-d)] [x-out (cdr x-d)]
                                       [y-in (car y-d)] [y-out (cdr y-d)])
                                  (vector-set! (graph-d* g) x (cons x-in (add1 x-out)))
                                  (vector-set! (graph-d* g) y (cons (add1 y-in) y-out)))
                                (graph-size-set! g (add1 (graph-size g))))
                        ;; update both x's and y's lists, and degree
                        (let again ([x x]
                                    [y y]
                                    [d? (graph-directed? g)])
                          ;; update adjacency list
                          (vector-set! (graph-v* g) x (cons (cons y w) (adjlist-w g x)))
                          ;; update degree
                          (vector-set! (graph-d* g) x (add1 (vector-ref (graph-d* g) x)))
                          ;; note the subtlety of updating the size
                          (if (not d?)
                              (again y x #t)
                              (graph-size-set! g (add1 (graph-size g))))))))
           (errorf "graph-insert-edge!" "invalid edge: ~a -- ~a" x y))]))

  (define graph-remove!
    (case-lambda
      [(g v) (graph-remove-vertex! g v)]
      [(g x y) (graph-remove-edge! g x y)]))

  ;; For undirected graph:
  ;; 1) iterate through v's list, for each neighbor w, remove v
  ;; from its list, decrement its degree
  ;; 2) mark v's slot as #f, set it's degree to 0, decrement order,
  ;; decrement size by the length of v's list
  ;;
  ;; For directed graph: we have out-edges v -- > w and in-edges v <-- w.
  ;; 1) scan v's list, decrement every neighbor's in-degree
  ;; 2) scan each vertex w's list, see if v is on their list, If yes,
  ;; remove it, decrement w's out-degree, decrement size. This is time-wasting
  ;; for huge graphs...
  ;; 3) mark v's slot as #f, set it's degree to '(0 . 0), decrement order,
  ;; decrement size by the length of v's list
  (define graph-remove-vertex!
    (lambda (g v)
      (define get-vertex
        (lambda (ls)
          (if (graph-weighted? g)
              (caar ls)
              (car ls))))
      (if (vertex-exists? g v)
          (if (graph-directed? g)
              (begin
                (for-each (lambda (x) (set-car! (vector-ref (graph-d* g) (get-vertex x))
                                                (sub1 (car (vector-ref (graph-d* g) (get-vertex x))))))
                          (adjlist-w g v))
                (for-each (lambda (x)
                            (unless (= x v)
                              (let loop ([yl (adjlist-w g x)]
                                         [new '()])
                                (unless (null? yl)
                                  (if (= v (get-vertex yl))
                                      (begin
                                        ;; update adjacency list
                                        (vector-set! (graph-v* g) (get-vertex x)
                                                     (cons new (cdr yl)))
                                        ;; update out-degree
                                        (set-cdr! (vector-ref (graph-d* g) (get-vertex x))
                                                  (sub1 (cdr (vector-ref (graph-d* g) (get-vertex x)))))
                                        ;; update graph size
                                        (graph-size-set! g (sub1 (graph-size g))))
                                      (loop (cdr yl) (append new (list (car yl)))))))))
                          (graph-vertices g))
                ;; update metadata
                (graph-size-set! g (- (graph-size g) (length (adjlist-w g v))))
                (vector-set! (graph-v* g) v #f)
                (vector-set! (graph-d* g) v '(0 . 0))
                (graph-order-set! g (sub1 (graph-order g))))
              ;; scan through v's list
              (let loop ([vl (adjlist-w g v)])
                  (if (null? vl)
                      ;; then update order, size, etc.
                      (begin (graph-size-set! g (- (graph-size g) (length (adjlist-w g v))))
                             (vector-set! (graph-v* g) v #f)
                             (vector-set! (graph-d* g) v 0)
                             (graph-order-set! g (sub1 (graph-order g))))
                      ;; remove v from its neighbors' lists, then update the degree
                      (let looop ([yl (adjlist-w g (get-vertex vl))]
                                  [new '()])
                        (unless (null? yl)
                          (if (= v (get-vertex yl))
                              (begin
                                ;; update adjacency list
                                (vector-set! (graph-v* g) (get-vertex vl)
                                             (cons new (cdr yl)))
                                ;; update degree
                                (vector-set! (graph-d* g) (get-vertex vl)
                                             (sub1 (vector-ref (graph-d* g) (get-vertex yl))))
                                (loop (cdr vl)))
                              (looop (cdr yl) (append new (list (car yl))))))))))
          (errorf "graph-remove-vertex!" "vertex doesn't exist: ~a" v))))

  ;; This is easier than removing a vertex.
  ;; For undirected graph, remove y in x's list, decrement x's degree;
  ;; remove x in y's list, decrement y's degree Decrement size.
  ;; For directed graph, just remove y from x's list, decrement x's out-degree
  ;; and y's in-degree. Decrement size.
  (define graph-remove-edge!
    (lambda (g x y)
      (define get-vertex
        (lambda (e)
          (if (graph-weighted? g)
              (caar e)
              (car e))))
      (if (and (vertex-exists? g x) (vertex-exists? g y))
          (if (graph-directed? g)
              (begin (let loop ([xl (adjlist-w g x)]
                                [new '()])
                       (unless (null? xl)
                         (if (= y (get-vertex xl))
                             (begin (vector-set! (graph-v* g) x (cons new (cdr xl)))
                                    ;; update x's out-degree
                                    (set-cdr! (vector-ref (graph-d* g) x)
                                              (cdr (sub1 (vector-ref (graph-d* g) x))))
                                    ;; update y's in-degree
                                    (set-car! (vector-ref (graph-d* g) y)
                                              (car (sub1 (vector-ref (graph-d* g) y)))))
                             (loop (cdr xl) (append new (list (car xl)))))))
                     (graph-size-set! g (sub1 (graph-size g))))
              (let again ([x x]
                          [y y]
                          [d? (graph-directed? g)])
                ;; scan through x's list
                (let loop ([xl (adjlist-w g x)]
                           [new '()])
                  (unless (null? xl)
                    (if (= y (get-vertex xl))
                        ;; remove y from x's list and do again if directed
                        (begin (vector-set! (graph-v* g) x (cons new (cdr xl)))
                               (vector-set! (graph-d* g) x (sub1 (vector-ref (graph-d* g) x)))
                               (vector-set! (graph-d* g) y (sub1 (vector-ref (graph-d* g) y)))
                               (if (not d?)
                                   (again y x #t)
                                   (graph-size-set! g (sub1 (graph-size g)))))
                        (loop (cdr xl) (append new (list (car xl)))))))))
          (errorf "graph-remove-edge!" "edge or vertex doesn't exist: ~a -- ~a" x y))))

  ;; find a smallest power of 2 that's greater that the current length plus 3
  (define graph-resize!
    (lambda (g)
      (let ([new (let ([n (+ 3 (graph-length g))]) ;; 3 is arbirary
                   (do ([i 1 (add1 i)])
                       ((> (expt 2 i) n) (expt 2 i))))])
        (let ([v* (make-vector new #f)]
              [d* (make-vector new (if (graph-directed? g) '(0 . 0) 0))]
              [v (graph-v* g)]
              [d (graph-d* g)])
          (do ([i 0 (add1 i)])
              ((= i (graph-length g)) (graph-v*-set! g v*) (graph-d*-set! g d*))
            (vector-set! v* i (vector-ref v i))
            (vector-set! d* i (vector-ref d i)))))))

  (define valid-vertex?
    (lambda (i)
      (and (integer? i) (>= 0))))
  (define vertex-exists?
    (lambda (g v)
      (and (valid-vertex? v) (> (graph-length g) v) (vector-ref (graph-v* g) v))))
  (define adjacent?
    (lambda (ls x)
      (exists (lambda (y) (= x y)) ls)))
  (define adjlist
    (lambda (g v)
      (if (vertex-exists? g v)
          (if (graph-weighted? g)
              (map car (vector-ref (graph-v* g) v))
              (vector-ref (graph-v* g) v))
          #f)))
  (define adjlist-w
    (lambda (g v)
      (if (vertex-exists? g v)
          (vector-ref (graph-v* g) v)
          #f)))
  (define graph-length
    (lambda (g)
      (vector-length (graph-v* g))))
  (define graph-vertices
    (lambda (g)
      (let ([res '()]
            [v* (graph-v* g)]
            [count 0])
        (vector-for-each (lambda (v) (when v
                                       (set! res (cons count res)))
                                 (set! count (add1 count)))
                         v*)
        res)))
  (define graph-edges
          (lambda (g)
            (let ([state (make-vector (graph-length g) 'unseen)]
                  [parents (make-vector (graph-length g) -1)]
                  [edges '()]
                  [w? (graph-weighted? g)])
              (define get-vertex
                (lambda (e)
                  (if w? (car e) e)))
              (define dfs
                (lambda (v)
                  (vector-set! state v 'seen)
                  (for-each (lambda (x) (case (vector-ref state (get-vertex x))
                                          [(unseen)
                                           (vector-set! parents (get-vertex x) v)
                                           (set! edges (cons (if w? (list v (car x) (cdr x)) (cons v x)) edges))
                                           (dfs (get-vertex x))]
                                          [(seen)
                                           ;; when undirected, v may point back to its parent, don't count this edge
                                           (unless (= (get-vertex x) (vector-ref parents v))
                                             (set! edges (cons (if w? (list v (car x) (cdr x)) (cons v x)) edges)))]
                                          [(done)
                                           ;; when directed, forward and cross-edges count
                                           (if (graph-directed? g)
                                               (set! edges (cons (if w? (list v (car x) (cdr x)) (cons v x)) edges)))]))
                            (adjlist-w g v))
                  (vector-set! state v 'done)))
              (let* ([v* (graph-vertices g)]
                     [connected? #t]
                     [e* (dfs (car v*))])
                ;; if connected, all vertices are visited through the first vertex
                (for-each (lambda (v) (when (eq? 'unseen (vector-ref state v)) (set! connected? #f) (dfs v)))
                          (cdr v*))
                (cons edges connected?)))))

  ;; Cycles in a graph are detected using depth-first search
  ;; if we find an edge (u, v), where u is being seen
  ;; and v is already seen but not done, then (u, v)
  ;; is a back edge, hence there is a cycle.
  (define graph-cyclic?
    (lambda (g)
      (call/cc (lambda (k)
                 (let ([state (make-vector (graph-length g) 'unseen)])
                   (define dfs
                     (lambda (v)
                       (when (eq? 'unseen (vector-ref state v))
                         (vector-set! state v 'seen)
                         (for-each (lambda (x) (if (eq? 'seen (vector-ref state x))
                                                   (k #t)
                                                   (dfs x)))
                                   (adjlist g v))
                         (vector-set! state v 'done))))
                   ;; for-each is for its side effetcs, returns void
                   ;; map is not, and map returns value
                   (for-each (lambda (v) (dfs v)) (graph-vertices g))
                   #f)))))

  (define dag?
    (lambda (g)
      (and (graph-directed? g) (not (graph-cyclic? g)))))

  (define graph-transpose
    (lambda (g)
      (if (graph-directed? g)
          (let ([new (make-graph (graph-length g) #t (graph-weighted? g))])
            (for-each (lambda (v)
                        (for-each (lambda (x)
                                    (if (graph-weighted? g)
                                        (graph-insert! new (car x) v (cdr x))
                                        (graph-insert! new x v)))
                                  (adjlist-w g v)))
                      (graph-vertices g))
            new)
          (errorf "graph-transpose" "cannot transpose undirected graph"))))

  ;; just print each vertex and its neighbors
  (define graph-print
    (lambda (g)
      (let ([v* (graph-v* g)]
            [weighed? (graph-weighted? g)])
        (do ([i 0 (add1 i)])
            ((= i (vector-length v*)))
          (when (vector-ref v* i)
            (printf "~a: ~a~n" i (vector-ref v* i)))))))

  (define graph-to-dot
    (lambda (g path)
      (if (= 0 (graph-order g))
          (printf "graph is empty, nothing written~n")
          (call-with-output-file path
            (lambda (p)
              (let* ([d? (graph-directed? g)]
                     [to (if d? "->" "--")])
                (put-string p (if d? "digraph {~n" "graph {"))
                (fresh-line p)
                (put-string p "node [fontname=monospace];")
                (fresh-line p)
                (for-each (lambda (e)
                            (if (weighted? g)
                                (format "~a ~a ~a[label=\"~a\"];~n" (car e) to (cadr e) (caddr e))
                                (format "~a ~a ~a;~n" (car e) to (cdr e))))
                          (car (graph-edges g)))
                (put-string p "}")))))))

  (include "shortest-path.ss")
  (include "topological-sort.ss")
  (include "minimum-spanning-tree.ss")
  (include "strongly-connected-components.ss")
  (include "connected-components.ss")
  )
