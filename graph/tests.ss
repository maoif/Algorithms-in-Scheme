(import (chezscheme) (graph))

;; order only controls the max number of vertices
(define random-graph
  (lambda (order size)
    (let ([g (make-graph order #f)])
      (do ([x (random order) (random order)]
           [y (random order) (random order)])
          ((= (graph-size g) size) g)
        (unless (= x y)
          (graph-insert! g x y))))))

(define random-digraph
  (lambda (order size)
    (let ([g (make-graph order #t)])
      (do ([x (random order) (random order)]
           [y (random order) (random order)])
          ((= (graph-size g) size) g)
        (unless (= x y)
          (if (even? (random 99999))
              (graph-insert! g x y)
              (graph-insert! g y x)))))))

(define random-weighted-graph
  (lambda (order size)
    #f))

;; graph.ss
(define test-graph
  (lambda ()
    (let ()
      (define g1 (make-graph '(2 3 5 13) #f)) ;; make from vertices
      (define g2 (make-graph '((2 . 4) (4 . 6) (8 . 15)) #t)) ;; make from edges
      (define dag1 (make-graph '((2 . 3) (2 . 7) (3 . 7) (3 . 9)
                                 (5 . 7) (5 . 9) (7 . 13) (9 . 11)) #t))
      ;; insert some vertices
      (graph-insert! g1 7)
      (graph-insert! g1 11)
      ;; insert some edges(vertex exists)
      (graph-insert! g1 2 3)
      (graph-insert! g1 5 13)
      ;; insert some edges(vertex non-existant)
      (graph-insert! g1 5 19)
      (graph-insert! g1 19 23)
      (graph-insert! g1 7 23)
      ;; others
      (graph-insert! g1 2 11)
      (graph-insert! g1 2 5)
      (graph-insert! g1 3 7)
      (graph-insert! g1 7 11)

      (assert (= (graph-order g1) 8))
      (assert (= (graph-size g1) 9))
      (printf "g1:~n~a~n" g1)
      (graph-print g1)

      (graph-insert! g2 12)
      (graph-insert! g2 17)
      (graph-insert! g2 2 12)
      (graph-insert! g2 4 12)
      (graph-insert! g2 6 8)
      (graph-insert! g2 6 10)
      (graph-insert! g2 8 10)
      (graph-insert! g2 12 15)
      (graph-insert! g2 15 17)

      (assert (= (graph-order g2) 8))
      (assert (= (graph-size g2) 10))
      (printf "g2:~n~a~n" g2)
      (graph-print g2)

      (graph-print dag1)
      ;; cycle
      (printf "g1 contains cycle? ~a~n" (graph-cyclic? g1))
      (printf "g2 contains cycle? ~a~n" (graph-cyclic? g2))
      (printf "dag1 contains cycle? ~a~n" (graph-cyclic? dag1))

      ;; remove vertex
      (graph-remove! g1 5)
      (graph-remove! g1 7)
      (assert (= (graph-order g1) 6))
      (assert (= (graph-size g1) 3))

      ;; remove edge


      ;; graph-vertices
      ;; graph-edges
      )))

(define test-cycle
  (lambda ()
    (define dag1 (make-graph 3 #t))
    (define dag2 (make-graph '((2 . 3) (2 . 7) (3 . 7) (3 . 9)
                               (5 . 7) (5 . 9) (7 . 13) (9 . 11)) #t))

    (graph-insert! dag1 0 1)
    (graph-insert! dag1 0 2)
    (printf "dag1 cyclic? ~a~n" (graph-cyclic? dag1))
    (graph-insert! dag1 1 0)
    (printf "dag1 cyclic after adding one edge? ~a~n" (graph-cyclic? dag1))

    (printf "dag2 cyclic? ~a~n" (graph-cyclic? dag2))
    (graph-insert! dag2 11 5)
    (printf "dag2 cyclic after adding one edge? ~a~n" (graph-cyclic? dag2))
    ))

(define test-topo
  (lambda ()
    (define dag1 (make-graph '((2 . 3) (2 . 7) (3 . 7) (3 . 9)
                               (5 . 7) (5 . 9) (7 . 13) (9 . 11)) #t))
    (define dag2 (make-graph '(8 6 3 7 9) #t))

    (printf "topological sort dag1: ~a~n" (topological-sort dag1))
    (graph-insert! dag1 13 3)
    (printf "topological sort dag1 after adding an edge: ~a~n" (topological-sort dag1))

    (graph-insert! dag2 8 6)
    (graph-insert! dag2 6 7)
    (graph-insert! dag2 6 3)
    (graph-insert! dag2 8 9)
    (graph-insert! dag2 9 3)
    (printf "topological sort dag2: ~a~n" (topological-sort dag2))))

;; shortest path.ss
(define test-shortest-path
  (lambda ()
    (define dag1 (make-graph '((2 . 3) (2 . 7) (3 . 7) (3 . 9)
                               (5 . 7) (5 . 9) (7 . 13) (9 . 11)) #t))
    (define g1 (make-graph '((2 . 3) (2 . 5) (5 . 6) (5 . 8)
                             (3 . 9) (3 . 10) (6 . 10) (8 . 10) (9 . 6)) #f))
    (graph-print dag1)
    (graph-print g1)
    (printf "shortest path from 2 to 13 in dag1: ~a~n" (shortest-path dag1 2 13))
    (printf "shortest path from 2 to 10 in g1: ~a~n" (shortest-path g1 2 10))
    ))

;; connected-components.ss
(define test-cc
  (lambda ()
    ;; [Sedgewick p.545]
    (define g1 (make-graph '((0 . 1) (0 . 5) (0 . 6) (0 . 2) (5 . 3)
                             (4 . 3) (5 . 4) (8 . 7) (4 . 6) (10 . 9) (9 . 12)
                             (9 . 11) (11 . 12)) #f))
    (printf "CC in g1: ~a~n" (graph-cc g1))
    ))


;; strongly-connected-components.ss
(define test-scc
  (lambda ()
    ;; [Algorithms p.238]
    (define g1 (make-graph '((1 . 2) (2 . 6) (3 . 8) (4 . 3) (5 . 6) (5 . 9)
                             (6 . 7) (6 . 12) (7 . 1) (7 . 3) (7 . 11) (8 . 4)
                             (8 . 12) (9 . 14) (10 . 13) (10 . 11) (11 . 8) (11 . 12)
                             (12 . 15) (12 . 16) (13 . 9) (14 . 10) (14 . 15) (15 . 11))
                           #t))
    ;; [Sedgewick p.589]
    (define g2 (make-graph '((0 . 1) (0 . 5) (2 . 0) (2 . 3) (3 . 2) (3 . 5)
                             (4 . 2) (4 . 3) (5 . 4) (6 . 0) (6 . 4) (6 . 9)
                             (7 . 8) (8 . 7) (8 . 9) (9 . 11) (9 . 10) (10 . 12)
                             (11 . 4) (11 . 12) (12 . 9))
                           #t))

    ;; [Manual p.182]
    (define g3 (make-graph '((1 . 2) (2 . 3) (2 . 4) (2 . 5) (3 . 1) (4 . 1)
                             (4 . 8) (4 . 6) (5 . 6) (6 . 7) (7 . 5) (8 . 6))
                           #t))

    ;; [Tarjan 1972]
    (define g4 (make-graph '((8 . 1) (8 . 7) (7 . 4) (7 . 6) (5 . 3) (1 . 2)
                             (5 . 6) (4 . 5) (3 . 4) (3 . 7) (2 . 8) (2 . 3)) #t))

    ;; As you can see from the output, the order of SCCs are
    ;; reversed in the two algorithms. It's left as an
    ;; exercise to answer why it is so ^_^.
    (printf "SCC using kasaraju: ~a~n" (graph-scc g1 'k))
    (printf "SCC using tarjan: ~a~n" (graph-scc g1 't))

    (printf "SCC using kasaraju: ~a~n" (graph-scc g2 'k))
    (printf "SCC using tarjan: ~a~n" (graph-scc g2 't))

    (printf "SCC using kasaraju: ~a~n" (graph-scc g3 'k))
    (printf "SCC using tarjan: ~a~n" (graph-scc g3 't))

    (printf "SCC using kasaraju: ~a~n" (graph-scc g4 'k))
    (printf "SCC using tarjan: ~a~n" (graph-scc g4 't))))

;; two-coloring.ss

;; minimum-spanning-tree.ss
(define test-mst
  (lambda ()
    ;; [Cormen p.635]
    (define g1 (make-graph '((1 2 4) (1 8 8) (2 3 8) (2 8 11) (3 9 2) (3 6 4)
                             (8 9 7) (7 8 1) (7 9 6) (4 3 7) (4 5 9) (6 4 14)
                             (6 5 10) (6 7 2))
                           #f #t))
    ;; [Algorithms p.264]
    (define g2 (make-graph '((1 2 4) (1 3 26) (1 4 14) (3 7 16) (4 3 30) (5 6 8)
                             (5 7 5) (6 7 10) (6 4 2) (7 4 3) (2 6 18) (2 4 12))
                           #f #t))

    (printf "MST1 edges K: ~a~n" (car (graph-edges (graph-mst g1 'k))))
    (printf "MST1 edges P: ~a~n" (car (graph-edges (graph-mst g1 'p))))
    (printf "MST1 edges B: ~a~n" (car (graph-edges (graph-mst g1 'b))))
    (printf "~n")
    (printf "MST2 edges K: ~a~n" (car (graph-edges (graph-mst g2 'k))))
    (printf "MST2 edges P: ~a~n" (car (graph-edges (graph-mst g2 'p))))
    (printf "MST2 edges B: ~a~n" (car (graph-edges (graph-mst g2 'b))))
    ))
