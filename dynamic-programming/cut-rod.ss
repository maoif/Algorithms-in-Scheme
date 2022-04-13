#|
Cut Rod

;;; Implementation
See [Cormen p.360]. Given a rod of length n and a table of prices of
each length, determine how we can cut the rod so that the sum of the
prices of the sub-rods are maximized.

|#

(include "../data-structure/table.ss")
(include "../tools.ss")

(define cut-rod
  (lambda (len prices)
    ;; dp[i]: largest price of cutting a rod of length i
    (make-table dp 0 (add1 len))
    (make-table cuts 0 (add1 len))
    (define get-price
      ;; if: xtra check to make sure when we don't have the price,
      ;; the rod cannot be cut in this way.
      (lambda (l) (let ([r (assoc l prices)]) (if r (cdr r) 0))))
    (dp-set! 1 (get-price 1))
    (if (and (integer? len) (< 0 len))
        (begin
          ;; compute the table bottom-up
          (do ([i 1 (add1 i)])
              ((= i (add1 len)))
            (let ([target (get-extreme (map (lambda (j)
                                              (let ([j (add1 j)])
                                                (cons (+ (get-price j) (dp-get (- i j)))
                                                      j)))
                                            (iota i))
                                       (lambda (x y) (< (car x) (car y))))])
              (dp-set! i (car target))
              (cuts-set! i (cdr target)))
            #|
            (let ([price 0]
                  [pos 1])
              (do ([j 1 (add1 j)])
                  ((= j (add1 i)))
                (when (< price (+ (get-price j) (dp-get (- i j))))
                  (set! price (+ (get-price j) (dp-get (- i j))))
                  (set! pos j)))
              (dp-set! i price)
              (cuts-set! i pos))
            |#
            )
          (printf "length: ~a~nprice: ~a~n" len (dp-get len))
          ;; todo you should not know that table cuts is a vector
          (vector-iterate cuts (lambda (i v)
                                   (unless (= i 0)
                                     (printf "Length ~a should be cut into ~a and ~a~n" i v (- i v))))))
        (errorf "cut-rod" "invalid length: ~a" len))))

(define prices
  '((1 . 1)  (2 . 5)  (3 . 8)  (4 . 9)  (5 . 10)
    (6 . 17) (7 . 17) (8 . 20) (9 . 24) (10 . 30)))

;;(cut-rod 1 prices)
;;(cut-rod 2 prices)
;;(cut-rod 3 prices)
;;(cut-rod 4 prices)
;;(cut-rod 5 prices)
(cut-rod 23 prices)
