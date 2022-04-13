#|
Fisher-Yates Shuffle Algorithm

;;; Implementation
Walk the vector from end to start, for each element,
swap it with a previous element whose index is generated
randomly.
|#

(define shuffle!
  (lambda (ls)
    (define swap!
      (lambda (x y)
        (let ([s (vector-ref ls x)])
          (vector-set! ls x (vector-ref ls y))
          (vector-set! ls y s))))
    (unless (vector? ls) (errorf "shuffle!" "not a vector: ~a" ls))
    (cond [(>= (vector-length ls) 3)
           (do ([i (sub1 (vector-length ls)) (sub1 i)])
               ((= i 1))
             (swap! i (modulo (random (most-positive-fixnum)) i)))]
          [(= (vector-length ls) 2) (swap! 0 1)])))
