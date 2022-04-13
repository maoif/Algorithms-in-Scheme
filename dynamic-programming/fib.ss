#|
Fibonacci sequence using dynamic programming

we use three local variables f, g and next to
store the intermediate results to avoid O(n^2) recursion
|#

(define (fib n)
  (let ([f 0]
        [g 1]
        [next 0])
    (do ([i 0 (add1 i)])
        ((= i n) g)
      (set! next (+ f g))
      (set! f g)
      (set! g next))))
