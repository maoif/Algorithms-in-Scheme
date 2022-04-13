#|
compute a digraph's strongly connected components

;;; API
(graph-scc g select)
Computes digraph g's strongly connected compoents,
returns a list of lists of vertices that are in the
same component, e.g., ((1 2 3) (4 5 6) (7 8 9)).

select should be 'k or 't, which instructs graph-scc
to use Kasaraju's algorithm and Tarjan's algorithm
respectively.

;;; Implementation
See below
|#

(define graph-scc
  (lambda (g select)
    ;; If we contract all SCCs in a digraph into vertices, then
    ;; the resulting "component graph" is actually a DAG, which
    ;; can be topologically sorted. As for this original graph,
    ;; even though we cannot do a topological sort, the vertices,
    ;; when ordered by their 'done time (finish time) after a DFS,
    ;; ARE sorted in a topological sense. Let's call this ordered
    ;; list of vertices 'topo', and suppose the vertex with the
    ;; longest 'done time, v, lies on the leftmost, and the vertex that
    ;; was first finished lies on the rightmost. It's not hard to prove that
    ;; v is the source vertex in the graph and all edges in the graph
    ;; now point from left to right in 'topo'. Moreover, vertices that
    ;; belong to the same SCC form a contiguous sublist in 'topo'.
    ;; If we let T be the transposed graph of g, i.e., inverting the
    ;; directions of all edges, then v will be the sink vertex. And now
    ;; all edges in this transposed graph point from right to left in 'topo'.
    ;;
    ;; The idea of Kasaraju's algorithm is to run DFS on the transposed graph
    ;; starting from the leftmost sink vertex in 'topo', then all vertex in the
    ;; DFS spanning tree belong to one SCC. Then select the next leftmost
    ;; unvisited vertex in 'topo', do DFS, find the next SCC, and repeat.
    (define kasaraju
      (lambda (g)
        (define find-topo
          (lambda ()
            (let ([state (make-vector (graph-length g) #f)]
                  [topo '()])
              (define dfs
                (lambda (v)
                  (unless (vector-ref state v)
                    (vector-set! state v #t)
                    (for-each (lambda (x) (dfs x)) (adjlist g v))
                    (set! topo (cons v topo)))))
              (for-each (lambda (v) (dfs v)) (graph-vertices g))
              topo)))
        (define find-scc
          (lambda (T v*)
            (let ([state (make-vector (graph-length T) #f)]
                  [temp '()]
                  [scc '()])
              (define dfs
                (lambda (v)
                  (unless (vector-ref state v)
                    (vector-set! state v #t)
                    (for-each (lambda (x) (dfs x)) (adjlist T v))
                    (set! temp (cons v temp)))))
              ;; need to check state since this selects the root vectices of each SCC
              (for-each (lambda (v) (unless (vector-ref state v)
                                      (dfs v)
                                      (set! scc (cons temp scc))
                                      (set! temp '())))
                        v*)
              scc)))
        (let* ([topo (find-topo)]
               [T (graph-transpose g)]
               ;; In fact, we don't have to transpose g, and
               ;; [scc (find-scc g (reverse topo))]
               ;; will work, too. Then the output will be
               ;; almost the same as Tarjan's.
               [scc (find-scc T topo)])
          scc)))

    ;; Tarjan's algorithm explores the graph down to the last vertex
    ;; in the last SCC, pushing every encountered vertex on an auxiliary
    ;; stack. Eventually we'll come to a vertex with no out-edges. Back out
    ;; and try another node, meanwhile update its low value.
    ;;
    ;; The low value is used to mark the root vertex in an SCC. Backing out
    ;; and updating the low value propagates the low value, so that vertices
    ;; with the same low value belong to the same SCC.
    ;;
    ;; Which vertex is the root? The one whose low value == its visit time. If
    ;; we encounter that a vertex (call it v), we know we've traversed all the
    ;; SCCs that this SCC (where v is the root) goes to, so we pop the stack till
    ;; we see v, and the popped elements are in the same SCC as v. We do the
    ;; comparison (v's low value == v's visit time)? every time we finish visiting the
    ;; neighbors of v.
    ;;
    ;; At one vertex v we either 1) go to the next unvisited vertex in the same SCC,
    ;; 2) or to the unvisited vertex in the next SCC in topological order,
    ;; 3) or to a vertex that's already visited and on the stack,
    ;; 4) or to a vertex that's already visited but not on the stack,
    ;; 5) or it has nowhere to go.
    ;; In case 1) and 2), just do DFS on the vertex. In case 3), we found the cycle, back
    ;; out and update v's low value. In case 4) that vertex belongs to an SCC
    ;; that was peeled off, so back out but do not update the low value, and
    ;; selects the next neighbor. In case 5) just back out. See the code for
    ;; how to update the low value.
    ;;
    ;; Tarjan's algorithm is used in ChezScheme compiler for
    ;;
    ;; Tarjan's paper: Depth-First Search and Linear Graph Algorithms (1972)
    ;; [Algorithms ch.6] also deals with this.
    (define tarjan
      (lambda (g)
        (let* ([scc '()]
               [stack '()]
               [count 0]
               ;; Yeah, so many vectors. But let's assume our memory is enough.
               [state (make-vector (graph-length g) #f)]
               [low   (make-vector (graph-length g) 0)]
               [visit   (make-vector (graph-length g) 0)] ;; todo this is extraneous?
               [on-stack (make-vector (graph-length g) #f)]
               [on-stack? (lambda (v) (vector-ref on-stack v))]
               [push! (lambda (v) (set! stack (cons v stack))
                              (vector-set! on-stack v #t))]
               [pop! (lambda () (let ([e (if (null? stack) #f (car stack))])
                                  (if e
                                      (begin (set! stack (cdr stack))
                                             (vector-set! on-stack e #f)
                                             e)
                                      e)))])
          (define dfs
            (lambda (v)
              (unless (vector-ref state v)
                (vector-set! state v #t)
                (push! v)
                (vector-set! visit v count)
                (vector-set! low v (vector-ref visit v))
                (set! count (add1 count))
                ;; if this for-each doesn't run, that's case 5)
                (for-each (lambda (x)
                            (if (vector-ref state x)
                                ;; when (on-stack? x) is #t, that's case 3), otherwise case 3)
                                (when (on-stack? x)
                                  (vector-set! low v (min (vector-ref visit x) (vector-ref low v))))
                                ;; case 1) and 2)
                                (begin (dfs x) (vector-set! low v (min (vector-ref low x) (vector-ref low v))))))
                          (adjlist g v))
                (when (= (vector-ref low v) (vector-ref visit v))
                  (set! scc (cons (let ([temp '()])
                                      (let f ([e (pop!)])
                                        (if (and e (= e v))
                                            (begin (set! temp (cons e temp)) temp)
                                            (if e
                                                (begin (set! temp (cons e temp)) (f (pop!)))
                                                temp))))
                                    scc))))))
          (for-each (lambda (v) (dfs v)) (graph-vertices g))
          scc)))
    (unless (graph-directed? g) (errorf "graph-scc" "graph needs to be directed"))
    (case select
      [(k) (kasaraju g)]
      [(t) (tarjan g)]
      [else (errorf "graph-scc" "invalid second argument: ~a" select)])))
