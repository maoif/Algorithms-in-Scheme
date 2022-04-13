#|
Edit Distance

;;; Implementation
See [Manual p.280]
|#

(include "../tools.ss")
(include "../data-structure/table.ss")

(define edit-distance
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
              ;; We are trying to count the minimum edit distance from v1[1:i] to v2[1:j].
              ;; If we just append (insert) v2[j] to v1, then we just have to look at the
              ;; distance from v1[1:i] to v2[1:j-1].
              ;; If we delete v1[i], then we have to look  at the distance from v1[1:i-1] to v2[1:j].
              ;; If v1[i] and v2[j], then no edits are needed, we simply proceed to v1[i-1] and v2[j-1];
              ;; otherwise we can do substitution, or changing v1[i] into v2[j].
              ;; Each type of edit costs one, so we add1.
              (let ([ins (add1 (dp-get i (sub1 j)))]
                    [del (add1 (dp-get (sub1 i) j))]
                    [subst/match (if (char=? (vector-ref v1 i)
                                             (vector-ref v2 j))
                                     (dp-get (sub1 i) (sub1 j))
                                     (add1 (dp-get (sub1 i) (sub1 j))))])
                (let loop ([count 0]
                           [target (min ins del subst/match)]
                           [res (list ins del subst/match)]
                           [ops (list 'ins 'del 'subst/match)])
                  (if (= target (list-ref res count))
                      (begin (dp-set! i j target) (edits-set! i j (list-ref ops count)))
                      (loop (add1 count) target res ops))))))
          (printf "edit distance between \"~a\" and \"~a\": ~a~nedits: ~a~n" s1 s2
                  (dp-get (sub1 n1) (sub1 n2))
                  (let get-ops ([res '()]
                                [i (sub1 n1)]
                                [j (sub1 n2)])
                    (if (and (= i 0) (= j 0))
                        res
                        (let ([op (edits-get i j)])
                          (case op
                            [(ins) (get-ops (cons op res) i (sub1 j))]
                            [(del) (get-ops (cons op res) (sub1 i) j)]
                            [(subst/match) (get-ops (cons (if (char=? (vector-ref v1 i)
                                                                      (vector-ref v2 j))
                                                              'match
                                                              'subst)
                                                          res)
                                                    (sub1 i) (sub1 j))]))))))
        (errorf "edit-distance" "not string: ~a ~a" s1 s2))))

;; remember that 'match doesn't count in edits
(edit-distance "algorithm" "altruistic")
(edit-distance "insertion" "deletion")
(edit-distance "scheme" "")
(edit-distance "" "scheme")
(edit-distance "scratch" "scheme")
(edit-distance "thou-shalt-not" "you-should-not")
