#|
Counting Sort

;;; API
(counting-sort! pred ls)

;;; Implemenation
1. find the min and max of the input vector
2. create a new vector aux of length (max-min+1), each element of
   index i stores the number of occurence of (i+min) in the
   input vector
3. scan through the input vector and update each element's
   number of occurence in aux
4. scan through aux and place the numbers back into the input vector

|#
(define counting-sort!
    (lambda (pred ls)
      ;; 1.
      (let-values ([(min max) (let ([min 0]
                                    [max 0])
                                (vector-for-each
                                 (lambda (x)
                                   (when (pred x min)
                                     (set! min x))
                                   (when (pred max x)
                                     (set! max x)))
                                 ls)
                                (values min max))])
        ;; 2.
        (let* ([n (vector-length ls)]
               [aux (make-vector (- max min -1))])
          ;; 3.
          (do ([i 0 (add1 i)])
              ((= i n))
            (vector-set! aux
                         (- (vector-ref ls i) min)
                         (add1 (vector-ref aux (- (vector-ref ls i) min)))))
          ;; 4.
          (let ([k 0])
            (do ([i 0 (add1 i)])
                ((= i (vector-length aux)))
              (let loop ([count (vector-ref aux i)])
                (when (> count 0)
                  (vector-set! ls k (+ i min))
                  (set! k (add1 k))
                  (loop (sub1 count))))))
          ls))))
