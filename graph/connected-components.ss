#|
Compute an undirected graph's connected components.

;;; API
(graph-cc g)
Returns a list of lists of vertices that are in the same component.

;;; Implementation
Run a DFS on a vertex v, every vertex that's reachable from
vertex v is in the same component as v. Then run DFS from the
next unvisited vertex.
|#

(define graph-cc
  (lambda (g)
    (if (not (graph-directed? g))
        (let ([state (make-vector (graph-length g) #f)]
              [scc '()]
              [temp '()])
          (define dfs
            (lambda (v)
              (unless (vector-ref state v)
                (vector-set! state v #t)
                (for-each (lambda (v) (unless (vector-ref state v)
                                        (dfs v)))
                          (adjlist g v))
                (set! temp (cons v temp)))))
          (for-each (lambda (v) (unless (vector-ref state v)
                                  (dfs v)
                                  (set! scc (cons temp scc))
                                  (set! temp '()))) (graph-vertices g))
          scc)
        (errorf "graph-cc" "graph needs to be undirected"))))
