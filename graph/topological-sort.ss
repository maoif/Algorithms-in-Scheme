#|
topological sort

;;; API
(topological-sort g)
    do a topological-sort on graph g, g has to be a DAG,
    returns a list of sorted vertices, or #f when unable
    to sort

;;; Implementation
topological sort uses depth-first search on a DAG

when each vertex is finished, it's pushed onto a stack, and
eventually the stack will contain the sort result

|#

(define topological-sort
  (lambda (g)
    (call/cc (lambda (k)
               (if (graph-directed? g)
                   (let ([result '()]
                         [state (make-vector (graph-length g) 'unseen)])
                     (define dfs
                       (lambda (v)
                         (when (eq? 'unseen (vector-ref state v))
                           (vector-set! state v 'seen)
                           (for-each (lambda (x) (if (eq? 'unseen (vector-ref state x))
                                                     (dfs x)
                                                     ;; already seen, cycle detected
                                                     ;; if v is 'done, doesn't matter, just continue
                                                     (when (eq? 'seen (vector-ref state x))
                                                       (k #f))))
                                     (adjlist g v))
                           (vector-set! state v 'done)
                           (set! result (cons v result)))))
                     (for-each (lambda (v) (dfs v)) (graph-vertices g))
                     result)
                   #f)))))
