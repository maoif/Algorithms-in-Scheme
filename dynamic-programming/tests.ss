;;; fib.ss
(define test-fib
  (lambda ()
    (include "fib.ss")
    (define fibs '(1 1 2 3 5 8 13))
    (define f '())
    (do ([i 0 (add1 i)])
        ((= i 7))
      (set! f (append f (list (fib i)))))
    (assert (equal? fibs f))))
