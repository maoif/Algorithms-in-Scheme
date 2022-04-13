#|
(heap-sort pred ls)

;;; Implemenation
a heap is already order by pred from top to bottom, what
we need to do is:
1. build a heap from the input
1. build a new vector of the same length(this would waste some space is the input is large)
2. recursively extract the top element from the heap into the vector
3. done
|#
(include "../data-structure/heap.ss")

(define heap-sort!
  (lambda (pred ls)
    (let ([h (heap-sort pred ls)])
      (do ([i 0 (add1 i)])
          ((= i (vector-length ls)))
        (vector-set! ls i (vector-ref h i))))))

(define heap-sort
  (lambda (pred ls)
    (let* ([h (make-heap pred ls)]
           [v (make-vector (heap-load h))])
      (let loop ([e (heap-extract-top! h)]
                 [i 0])
        (if e
            (begin (vector-set! v i e)
                   (loop (heap-extract-top! h) (add1 i)))
            v)))))
