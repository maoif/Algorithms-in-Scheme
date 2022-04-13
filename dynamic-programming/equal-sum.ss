#|
Equal Sum

;;; Implementation
Question: given a set of numbers, determine whether it can be
parititoned into two subsets such that their sums are equal.

This is actually a 01 knapsack problem where numbers are weights
and their value is 1.
|#
(include "../data-structure/table.ss")
(include "../tools.ss")

(define equal-sum
  (lambda (ls)
    (let* ([S (apply + ls)]
           [S/2 (/ S 2)]
           [ls (list->vector (cons #f ls))])
      (if (even? S)
          ;; since S is the sum of two equal parts, it must be even
          (let ()
            ;; dp[i, j] means whether selecting numbers from ls[1] to ls[i]
            ;; can make their sum equal to j.
            (make-table dp #f (vector-length ls) (add1 S/2))
            ;; (make-table chosen #f (length ls) (add1 S/2))
            ;; the 1st column is #t, indicating the case when ls[i]=j
            (do ([i 0 (add1 i)])
                ((= i (vector-length ls)))
              (dp-set! i 0 #t))
            (do ([i 1 (add1 i)])
                ((= i (vector-length ls)))
              ;; scan from right to left in each row
              (do ([j S/2 (sub1 j)])
                  ((= j 0))
                (when (<= (vector-ref ls i) j)
                  (dp-set! i j (or (dp-get (sub1 i) j)
                                   (dp-get (sub1 i) (- j (vector-ref ls i))))))))
            (printf "~a~n" (dp-get (sub1 (vector-length ls)) S/2))
            ;; get the partition
            (when (dp-get (sub1 (vector-length ls)) S/2)
              (let loop ([res '()]
                         [i (sub1 (vector-length ls))]
                         [j S/2])
                (if (or (= i 0) (= j 0))
                    (printf "half the set: ~a~n" res)
                    (if (dp-get (sub1 i) j)
                        (loop res (sub1 i) j)
                        ;; No need to check whether (- j (vector-ref ls i)) gives
                        ;; a negative index since if the first case is #f, this case
                        ;; must be true, otherwise we wouldn't've got here.
                        (loop (cons (vector-ref ls i) res) (sub1 i) (- j (vector-ref ls i))))))))
          (printf "#f~n")))))

(equal-sum '(1 2 3 4))
(equal-sum '(1 1 3 4 7))
(equal-sum '(2 3 4 6))
(equal-sum '(2 3 4 6 1 8 1 4 5 0))
