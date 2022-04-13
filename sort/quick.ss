#|
(quick-sort! pred ls)
Sort on list or vector a using quick-sort, returns sorted list or vector.
|#

(define quick-sort!
    (lambda (pred ls)
      (define partition
        (lambda (ls pred s e)
          (let ([pivot (vector-ref ls e)]
                [i (sub1 s)])
            (do ([j s (add1 j)])
                ;; note that when j==e-1(the element before the pivot),
                ;; the loop still needs to run
                ((= j e)
                 (swap! ls (add1 i) e)
                 (add1 i))
              (when (pred (vector-ref ls j) pivot)
                (set! i (add1 i))
                (swap! ls i j))))))
      (define swap!
        (lambda (ls a b)
          (let ([t (vector-ref ls a)])
            (vector-set! ls a (vector-ref ls b))
            (vector-set! ls b t))))
      (define inner-sort
        (lambda (ls s e)
          (when (< s e)
            (let ([p (partition ls pred s e)])
              (inner-sort ls s (sub1 p))
              (inner-sort ls (add1 p) e)))))
      (unless (vector? ls) (errorf "quick-sort" "second argument should be a vector"))
      (let ([n (vector-length ls)])
        (cond
         [(<= n 1) ls]
         [(= n 2) (if (pred (vector-ref ls 0) (vector-ref ls 1))
                      ls
                      (vector (vector-ref ls 1) (vector-ref ls 0)))]
         [else (begin (inner-sort ls 0 (sub1 n)) ls)]))))
