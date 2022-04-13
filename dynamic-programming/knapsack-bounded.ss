#|
Knapsack (bounded)

;;; API
(knapsack-bounded cap w* v* n*)
cap: total capacity
w*: a list of weights
v*: a list of values
n*: a list of number of items

|#

(include "../tools.ss")
(include "../data-structure/table.ss")

(define knapsack-bounded
  (lambda (cap w* v* n*)
    (define pred
      (lambda (x y) (< (car x) (car y))))
    (let* ([w* (list->vector (cons #f w*))]
           [v* (list->vector (cons #f v*))]
           [n* (list->vector (cons #f n*))]
           [wl (vector-length w*)]
           [vl (vector-length v*)])
      (if (= wl vl)
          (let ()
            ;; dp[i, j] = (v . n)
            ;; v: the max value when capacity is j and
            ;; index of available objects are from i to j
            ;; n: number of item i
            (make-table dp 0 wl (add1 cap))
            (do ([i 1 (add1 i)])
                ((= i wl))
              (do ([j 1 (add1 j)])
                  ((= j (add1 cap)))
                (if (<= (vector-ref w* i) j)
                  (let ([y
                         ;; Add one item i at a time until their total weight
                         ;; exceeds j, then select the max value combination.
                         (let loop ([num 1]
                                    [v (vector-ref v* i)]
                                    [w (vector-ref w* i)]
                                    ;; ((v1 . 1) (v2 . 2) (v3 . 3) ...) reversed
                                    [choices '()])
                           ;; The only difference with unbounded case is we have
                           ;; to constrain the number of available items.
                           (if (or (= num (vector-ref n* i)) (> (+ w w) j))
                               (get-extreme choices pred)
                               (loop (add1 num) (+ v v) (+ w w)
                                     (cons (+ v (dp-get (sub1 i) (- j w))) num))))]
                        [n (cons (dp-get (sub1 i) j) 0)])
                    (dp-set! i j (get-extreme (list y n) pred)))
                  ;; When the current items's weight is bigger than j,
                  ;; we can only drop it and check the previous item.
                  (dp-set! i j (cons (dp-get (sub1 i) j) 0)))))
            (printf "~a~n" (dp-get (sub1 wl) cap))
            ;; ((item . times) ...)
            (printf "select items: ~a~n"
                    (let loop ([res '()]
                               [i (sub1 wl)]
                               [j cap])
                      (if (or (= i 0) (= j 0))
                          res
                          (if (= (car (dp-get i j)) (car (dp-get (sub1 i) j)))
                              (loop res (sub1 i) j)
                              (let ([x (dp-get i j)])
                                (loop (cons (cons i (cdr x)) res)
                                      (sub1 i)
                                      (- j (* (vector-ref w* i) (cdr x))))))))))
          (errorf "knapsack-bounded" "length of weights and values not equal")))))
