(define random-vector
  (lambda (size range)
    (let ([v (make-vector size)])
      (do ([i 0 (add1 i)])
          ((= i (vector-length v)))
        (vector-set! v i (random range)))
      v)))

;; iteration:
;; (proc index element)

(define iterate
  (lambda (ls proc)
    (do ([i 0 (add1 i)]
         [ls ls (cdr ls)])
        ((null? ls))
      (proc i (car ls)))))

(define vector-iterate
  (lambda (ls proc)
    (do ([i 0 (add1 i)])
        ((= i (vector-length ls)))
      (proc i (vector-ref ls i)))))

(define vector-iterate-rev
  (lambda (ls proc)
    (do ([i (sub1 (vector-length ls))
            (sub1 i)])
        ((= i -1))
      (proc i (vector-ref ls i)))))

;; end exclusive
(define vector-iterate-range
  (lambda (ls proc start end)
    (do ([i start (add1 i)])
        ((= i end))
      (proc i (vector-ref ls i)))))

(define string-iterate
  (lambda (s proc)
    (do ([i 0 (add1 i)])
        ((= i (string-length s)))
      (proc i (string-ref s i)))))

(define get-extreme
  (lambda (ls pred)
    (cond [(list? ls) (let ([max (car ls)])
                        (for-each (lambda (x) (when (pred max x)
                                                (set! max x)))
                                  ls)
                        max)]
          [(vector? ls) (let ([max (vector-ref ls 0)])
                          (vector-for-each (lambda (x) (when (pred max x)
                                                         (set! max x)))
                                           ls)
                          max)]
          [else (errorf "get-max" "not list nor vector: ~a" ls)])))
