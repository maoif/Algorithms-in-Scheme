#|
Binomial Coefficients
|#

(include "../data-structure/table.ss")

(define choose
  (lambda (n k)
    (if (and (integer? n) (integer? k) (<= k n))
        (let ()
          (make-table dp 0 (add1 n) (add1 n))
          (do ([i 0 (add1 i)])
              ((= i (add1 n)))
            (dp-set! i 0 1))
          (do ([i 1 (add1 i)])
              ((= i (add1 n)))
            (do ([j 1 (add1 j)])
                ((= j (add1 i)))
              ;; we are filling more cells than necessary
              (dp-set! i j (+ (dp-get (sub1 i) (sub1 j))
                              (dp-get (sub1 i) j)))))
          (dp-get n k))
        (errorf "choose" "invalid argument(s): ~a choose ~a" n k))))
;;(printf "4 choose 2: ~a~n" (choose 4 2))
;;(printf "5 choose 2: ~a~n" (choose 5 2))
;;(printf "10 choose 6: ~a~n" (choose 10 6))
