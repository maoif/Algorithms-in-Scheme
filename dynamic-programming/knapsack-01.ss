#|
Knapsack (01)

;;; Implementation

We have a knapsack of capacity `cap`, and a bunch of objects with
values (v1, ..., vn) and weights (w1, ..., wn). The question is: how
to select objects to put into the knapsack so that the total weight
does not exceed `cap` and the total value is maximized. Every object
can only be selected once, or not selected at all, hence 01.

|#

(include "../tools.ss")
(include "../data-structure/table.ss")

(define knapsack-01
  (lambda (cap w* v*)
    (let* ([w* (list->vector (cons #f w*))]
           [v* (list->vector (cons #f v*))]
           [wl (vector-length w*)]
           [vl (vector-length v*)])
      (if (= wl vl)
          (let ()
            ;; dp[i, j]: the max value when capacity is j and
            ;; index of available objects are from i to j
            (make-table dp 0 wl (add1 cap))
            (do ([i 1 (add1 i)])
                ((= i wl))
              (do ([j 1 (add1 j)])
                  ((= j (add1 cap)))
                (if (<= (vector-ref w* i) j)
                  (let ([y (+ (vector-ref v* i)
                              (dp-get (sub1 i) (- j (vector-ref w* i))))]
                        [n (dp-get (sub1 i) j)])
                    (dp-set! i j (max y n)))
                  ;; When the current items's weight is bigger than j,
                  ;; we can only drop it and check the previous item.
                  (dp-set! i j (dp-get (sub1 i) j)))))
            (printf "~a~n" (dp-get (sub1 wl) cap))
            (printf "select items: ~a~n"
                    (let loop ([res '()]
                               [i (sub1 wl)]
                               [j cap])
                      (if (or (= i 0) (= j 0))
                          res
                          (if (= (dp-get i j) (dp-get (sub1 i) j))
                              (loop res (sub1 i) j)
                              (loop (cons i res) (sub1 i)
                                    (- j (vector-ref w* i))))))))
          (errorf "knapsack-01" "length of weights and values not equal")))))


(knapsack-01 10 '(1 2 3 8 7 4) '(20 5 10 40 15 25))
;; 60

(knapsack-01 50 '(10 20 30) '(60 100 120))
;; 220

(knapsack-01 12 '(5 4 6 3) '(10 40 30 50))
