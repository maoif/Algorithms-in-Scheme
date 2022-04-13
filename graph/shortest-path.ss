#|
shortest path

;;; API
(shortest-path g x y)
    finding a shortest path between two x and y in graph g,
    returns the path, or #f when no such path exists

;;; Implemenation
Starting from x, this algorithm uses breath-first search to explore
all ajdacent vertices of x, then explores all ajdacent vertices of the
ajdacent vertices of x, and so on until y appears.

When searching, the algorithm maintains a vector (parents) the same length as that
in the graph to record the parent of each explored vertex. If v is explored
from u, then parents[v] == u. In this way we can recover the path from x to y.

The nature of BFS guarantees that the path thus found using the parents vector
is the shortest. For proof, see [Cormen].
|#


(define shortest-path
  (lambda (g x y)
    (call/cc (lambda (k)
               (define get-path
                 (lambda (parents x y)
                   (let loop ([p (vector-ref parents y)]
                              [path `(,y)]
                              [length 0])
                     (if (eq? x p)
                         (cons p path)
                         (if (vector-ref parents p)
                             (loop (vector-ref parents p) (cons p path) (add1 length))
                             #f)))))

               (if (and (vertex-exists? g x) (vertex-exists? g y))
                   (let ([parents (make-vector (graph-length g) #f)]
                         [state (make-vector (graph-length g) 'unseen)]
                         [queue `(,x)])
                     (define enqueue!
                       (lambda (e)
                         (set! queue (append queue (list e)))))
                     (define dequeue! (lambda ()
                                        (if (null? queue)
                                            #f
                                            (let ([e (car queue)])
                                              (set! queue (cdr queue))
                                              e))))
                     (vector-set! state x 'seen)
                     (let loop ([v (dequeue!)])
                       (if v
                           (begin (for-each (lambda (xx)
                                              (when (eq? 'unseen (vector-ref state xx))
                                                (vector-set! state xx 'seen)
                                                (vector-set! parents xx v)
                                                (if (= xx y)
                                                    (k (get-path parents x y))
                                                    (enqueue! xx))))
                                            (adjlist g v))
                                  (loop (dequeue!)))
                           #f)))
                   #f)))))
