#|
(merge-sort!  pred a)

;;; Implementation
Recursively split the input into halves until there's
only one element in each range, then back out and merge.

Merge works by comparing the elements of the two halves
from left to right, selecting the smaller one and puts into
the temp vector. If one side is copied out, then copy the other
side. Finally copy the temp vector to the corresponding range
in the original one.
|#
(library (merge)
  (export merge-sort!)
  (import (chezscheme))

  (define merge-sort!
    (lambda (pred ls)
      (define merge
        (lambda (s e m pred ls)
          (let ([i s]
                [j (add1 m)]
                [end (add1 e)]
                [temp (make-vector (add1 (- e s)))])
            (do ([k s (add1 k)])
                ((= k end)
                 ;; copy temp to ls
                 (do ([kk s (add1 kk)])
                     ((= kk end) ls)
                   (vector-set! ls kk (vector-ref temp (- kk s)))))
              (cond
               ;; when the right half is done, just copy the left halft
               [(> j e) (vector-set! temp (- k s) (vector-ref ls i)) (set! i (add1 i))]
               ;; when the left half is done, just copy the right halft
               [(> i m) (vector-set! temp (- k s) (vector-ref ls j)) (set! j (add1 j))]
               [(pred (vector-ref ls i)
                      (vector-ref ls j))
                (begin (vector-set! temp (- k s) (vector-ref ls i))
                       (set! i (add1 i)))]
               [else (vector-set! temp (- k s) (vector-ref ls j))
                     (set! j (add1 j))]))
            ls)))
      (define inner-sort
        (lambda (ls s e)
          (if (= s e)
              ls
              (let ([middle (fxsrl (+ s e) 1)])
                (inner-sort ls s middle)
                (inner-sort ls (add1 middle) e)
                (merge s e middle pred ls)))))
      (let ([n (vector-length ls)])
        (cond
         [(= n 1) ls]
         [(= n 2) (if (pred (vector-ref ls 0) (vector-ref ls 1))
                      ls
                      (vector (vector-ref ls 1) (vector-ref ls 0)))]
         [else (inner-sort ls 0 (fxsrl n 1))
               (inner-sort ls (add1 (fxsrl n 1)) (sub1 n))
               (merge 0 (sub1 n) (fxsrl n 1) pred ls)]))))
  )
