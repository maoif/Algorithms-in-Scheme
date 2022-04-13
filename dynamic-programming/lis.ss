#|
Longest Increasing Subsequence

;;; API
(LIS pred ls)
Given a list ls and a comparison procedure pred,
return the longest subsequence s of ls such that
s is ordered by pred.

As you can see the result may not only be increasing
if the given pred is, say, >.
|#
(include "../tools.ss")
(include "../data-structure/table.ss")
(define LIS
  (lambda (pred ls)
    (define find-next
      (lambda (i e)
        (call/cc
         (lambda (k)
           (let ([next #f])
             (when (< i (vector-length ls))
               (vector-iterate-range ls (lambda (j ee)
                                          (when (pred e (vector-ref ls j))
                                            (k j)))
                                     i (vector-length ls)))
             next)))))
    (if (procedure? pred)
        (if (vector? ls)
            (let ([n (vector-length ls)])
              ;; longest length from ls[i]
              (make-table L 1 n)
              ;; NEXT[i]: index of element after ls[i] in the longest sequence
              (make-table NEXT #f n)
              (vector-iterate-rev ls
                                  (lambda (i e)
                                    ;; check every element "bigger" that ls[i]
                                    (let loop ([next (find-next i e)])
                                      (when next
                                        (when (<= (L-get i) (L-get next))
                                          (L-set! i (add1 (L-get next)))
                                          (NEXT-set! i next))
                                        (loop (find-next (add1 next) e))))))
              (let loop ([e (vector-ref NEXT 0)]
                         [res (list (vector-ref ls 0))])
                (if e
                    (loop (vector-ref NEXT e) (cons (vector-ref ls e) res))
                    (printf "length: ~a: ~a~n" (length res) (reverse res)))))
            (errorf "LIS" "second argument should be a vector: ~a" ls))
        (errorf "LIS" "first argument should be a procedure"))))

(LIS < '#(2 3 9 0 6 8 1 5))
(LIS < '#(2 4 3 5 1 7 6 9 8))
