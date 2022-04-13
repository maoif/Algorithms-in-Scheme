#|
Matrix Chaining Multiplication Optimization

;;; API
(matrix-mul-op ms)
Returns the optimal parenthesization of matrices.
ms should be of the form '((r0 . c0) (r1 . c1) (r2 . c2) ...),
where r0 and c0 are #row and #column of the first matrix, and
so on.

;;; Implementation
See [Cormen p.376]

|#
(include "../tools.ss")
(include "../data-structure/table.ss")

(define matrix-mul-order
  (lambda (ms)
    (let* ([ms (list->vector (cons #f ms))]
           [n (vector-length ms)])
      ;; m[i, j]: the min cost of multiplying matrices Ai to Aj
      (make-table m 0 n n)
      ;; s[i, j]: the position of split in matrices Ai to Aj
      (make-table s 0 n n)
      (define get-row (lambda (i) (car (vector-ref ms i))))
      (define get-col (lambda (i) (cdr (vector-ref ms i))))
      (define find-min
        (lambda (L R)
          (if (= L R)
              0
              (if (< 0 (m-get L R))
                  (m-get L R)
                  (let* ([target (get-extreme
                                 ;; returns (min-cost . split-spot)
                                 (map (lambda (i)
                                        (let ([i (+ i L)])
                                          (cons (+ (find-min L i)
                                                   (find-min (add1 i) R)
                                                   (* (get-row L) (get-col i) (get-col R)))
                                                i)))
                                      (iota (- R L)))
                                 (lambda (x y) (> (car x) (car y))))]
                         [cost (car target)])
                    (m-set! L R (car target))
                    (s-set! L R (cdr target))
                    (m-get L R))))))
      (define get-res
        (lambda (L R)
          (cond [(= L R) L]
                [(= (add1 L) R) (list L R)]
                [else (let ([s (s-get L R)])
                        (cond [(= s L) (list L (get-res (add1 s) R))]
                              [(= (add1 s) R) (list (get-res L s) R)]
                              [else (list (get-res L s)
                                          (get-res (add1 s) R))]))])))
      (do ([i 1 (add1 i)])
          ((= i n))
        (find-min 1 i))
      (printf "min mul: ~a~n" (m-get 1 (sub1 n)))
      (printf "~a~n" (get-res 1 (sub1 n))))))

;; todo actually we only need one dimension for each matrix
(matrix-mul-order '((30 . 35) (35 . 15) (15 . 5) (5 . 10) (10 . 20) (20 . 25)))

(matrix-mul-order '((4 . 10) (10 . 3) (3 . 12) (12 . 20) (20 . 7)))
