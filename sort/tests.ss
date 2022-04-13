(import (chezscheme) (sort))

(define sorted?
  (lambda (pred v)
    (call/cc (lambda (k)
               (do ([i 0 (add1 i)])
                   ((= i (sub1 (vector-length v))) #t)
                 (unless (pred (vector-ref v i) (vector-ref v (add1 i)))
                   (k #f)))))))

(define random-vector
  (lambda (size range)
    (let ([v (make-vector size)])
      (do ([i 0 (add1 i)])
          ((= i (vector-length v)))
        (vector-set! v i (random range)))
      v)))

#|
quick sort
size: xxx range: xxx
(time ...)
size: xxx range: xxx
(time ...)
size: xxx range: xxx
(time ...)

merge sort
size: xxx range: xxx
(time ...)
size: xxx range: xxx
(time ...)
size: xxx range: xxx
(time ...)

...
|#

;; quick.ss
(let ()
  (define v1 (random-vector 50 999))
  (define v2 (random-vector 500 9999))
  (define v3 (random-vector 5000 99999))

  (printf "quick-sort~n")
  (printf "~a~n" (sorted? <= (quick-sort! < v1)))
  (printf "~a~n" (sorted? <= (quick-sort! < v2)))
  (printf "~a~n" (sorted? <= (quick-sort! < v3))))


;; merge.ss
(let ()
  (define v1 (random-vector 50 999))
  (define v2 (random-vector 500 9999))
  (define v3 (random-vector 5000 99999))

  (printf "merge-sort~n")
  (printf "~a~n" (sorted? <= (merge-sort! < v1)))
  (printf "~a~n" (sorted? <= (merge-sort! < v2)))
  (printf "~a~n" (sorted? <= (merge-sort! < v3))))

;; counting.ss
(let ()
  (define v1 (random-vector 50 999))
  (define v2 (random-vector 500 9999))
  (define v3 (random-vector 5000 99999))
  (define v4 (random-vector 5000 99999))

  (printf "counting-sort~n")
  (printf "~a~n" (sorted? <= (counting-sort! < v1)))
  (printf "~a~n" (sorted? <= (counting-sort! < v2)))
  (printf "~a~n" (sorted? <= (counting-sort! < v3)))

  (time (counting-sort! < v4)))

;; heap.ss
(let ()
  (define v1 (random-vector 50 999))
  (define v2 (random-vector 500 9999))
  (define v3 (random-vector 5000 99999))
  (define v4 (random-vector 5000 99999))

  (printf "heap-sort~n")
  (printf "~a~n" (sorted? <= (heap-sort < v1)))
  (printf "~a~n" (sorted? <= (heap-sort < v2)))
  (printf "~a~n" (sorted? <= (heap-sort < v3)))

  (time (heap-sort! < v4)))
