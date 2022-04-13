#|
Longest Common Subseqence

;;; Implementation

This is actually a special case of edit distance. Since we are looking
for the longest common subsequence, we are not allowed to substitute,
only match, insertion and deletion are allowed. We compute the dp
table, and find the letters that are matched from bottom-right corner.

|#

(include "../tools.ss")
(include "../data-structure/table.ss")

(define lcs
  (lambda (s1 s2)
    (if (and (string? s1) (string? s2))
        ;; #f: so that the 1st row and column can be ingnored
        (let* ([v1 (list->vector (cons #f (string->list s1)))]
               [v2 (list->vector (cons #f (string->list s2)))]
               [n1 (vector-length v1)]
               [n2 (vector-length v2)])
          ;; todo optimize the space
          (make-table dp 0 n1 n2)
          (make-table edits 0 n1 n2)
          (do ([i 1 (add1 i)])
              ((= i n1))
            (dp-set! i 0 i)
            (edits-set! i 0 'del))
          (do ([i 1 (add1 i)])
              ((= i n2))
            (dp-set! 0 i i)
            (edits-set! 0 i 'ins))

          (do ([i 1 (add1 i)])
              ((= i n1))
            (do ([j 1 (add1 j)])
                ((= j n2))
              (let ([ins (add1 (dp-get i (sub1 j)))]
                    [del (add1 (dp-get (sub1 i) j))]
                    [match (if (char=? (vector-ref v1 i)
                                             (vector-ref v2 j))
                                     (dp-get (sub1 i) (sub1 j))
                                     ;; here we effectively disallow substitution
                                     (most-positive-fixnum))])
                (let loop ([count 0]
                           [target (min ins del match)]
                           [res (list ins del match)]
                           [ops (list 'ins 'del 'match)])
                  (if (= target (list-ref res count))
                      (begin (dp-set! i j target) (edits-set! i j (list-ref ops count)))
                      (loop (add1 count) target res ops))))))
          (printf "LCS between \"~a\" and \"~a\": ~a~n" s1 s2
                  (let get-lcs ([res '()]
                                [i (sub1 n1)]
                                [j (sub1 n2)])
                    (if (and (= i 0) (= j 0))
                        res
                        (let ([op (edits-get i j)])
                          (case op
                            [(ins) (get-lcs res i (sub1 j))]
                            [(del) (get-lcs res (sub1 i) j)]
                            [(match) (get-lcs (cons (vector-ref v1 i) res) (sub1 i) (sub1 j))]))))))
        (errorf "lcs" "not string: ~a ~a" s1 s2))))

(lcs "algorithm" "altruistic")
(lcs "insertion" "deletion")
(lcs "scheme" "")
(lcs "" "scheme")
(lcs "scratch" "scheme")
(lcs "thou-shalt-not" "you-should-not")
