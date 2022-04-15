#|
Infix Calculator

;;; Implementation
This procedure uses the Shunting-Yard algorithm to evaluate arithmetic
expressions.

Permitted operations are: + - * / ^(exponent) !(factorial), %(mod),
parentheses are allowed.

When encountering '-' as negation, we add a '0' in the operand stream.

Numbers cannot start with '0' except for 0 itself.

Whitespace is ignored.

TODO
1. more error handling, e.g., "3+-3", "32 54 + 3 3" (error or feature?)
2. decimal point

|#

(include "../tools.ss")
(include "../data-structure/stack.ss")

(define calc
  (lambda (s)
    (if (and (string? s) (not (string=? "" s)))
        (let ([s* (string->list s)])
          ;; (op priority arity)
          (define operators '((#\( 0 0) (#\) 5 0) (#\+ 1 2) (#\- 1 2) (#\* 2 2)
                              (#\/ 2 2) (#\% 2 2) (#\^ 3 2) (#\! 4 1)))
          (define numbers '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
          ;; cddr: exclude parens
          (define is-op? (lambda (c) (memq c (map car (cddr operators)))))
          (define is-num? (lambda (c) (memq c numbers)))
          (define get-prio (lambda (c) (cadr (assoc c operators))))
          (define get-arity (lambda (c) (caddr (assoc c operators))))
          (define valid-chars?
            (lambda ()
              (let ([c* `(,@(map car operators) ,@numbers #\space)])
                (iterate s* (lambda (i c)
                              (unless (memq c c*)
                                (errorf "calc" "invalid char ~a at position ~a in ~a" c i s)))))))
          (define valid-parens?
            (lambda ()
              (let ([p* (make-stack char=?)])
                (iterate s* (lambda (i c)
                              (case c
                                [(#\() (stack-push! p* (cons i c))]
                                [(#\)) (if (stack-empty? p*)
                                           (errorf "calc" "unmatched right parenthesis at position ~a in ~a" i s)
                                           (stack-pop! p*))])))
                (unless (stack-empty? p*)
                  (errorf "calc" "unmatched left parenthesis at position ~a in ~s" (car (stack-pop! p*)) s)))))
          (define evaluate
            (lambda (ls)
              (let ([stk (make-stack)])
                (for-each (lambda (x)
                            (if (is-op? x)
                                (let ([ar (get-arity x)])
                                  (case ar
                                    [(1) (let ([x1 (stack-pop! stk)])
                                           (stack-push! stk
                                                        (case x
                                                          [(#\!)
                                                           (if (and (integer? x1) (>= x1 0))
                                                               (let fact ([res 1]
                                                                          [n x1])
                                                                 (if (= n 0)
                                                                     res
                                                                     (fact (* res n) (sub1 n))))
                                                               (errorf "calc" "factorial cannot work on ~a" n))]
                                                          [else (errorf "calc" "unknown op: ~a" x)])))]
                                    [(2) (let ([x1 (stack-pop! stk)]
                                               [x2 (stack-pop! stk)])
                                           (stack-push! stk
                                                        (case x
                                                          [(#\+) (+ x1 x2)]
                                                          [(#\-) (- x1 x2)]
                                                          [(#\*) (* x1 x2)]
                                                          [(#\/) (/ x1 x2)]
                                                          [(#\%) (modulo x1 x2)]
                                                          [(#\^) (expt x1 x2)]
                                                          [else (errorf "calc" "unknown op: ~a" x)])))]
                                    [else (errorf "calc" "unknown arity for ~a" x)]))
                                (stack-push! stk x)))
                          ls)
                (if (= (stack-load stk) 1)
                    (stack-pop! stk)
                    (errorf "calc" "bad stack after evaluation: ~a" (stack-pop!-all stk))))))
          (define to-RPN
            (lambda ()
              (let ([ops (make-stack char=?)])
                (let loop ([i 0]
                           [in s*]
                           [out '()]
                           [num ""])
                  (printf "i: ~a ~nin: ~a~nout: ~a~nnum: ~a~nstack: ~a~n" i in out num ops)
                  (if (= i (length s*))
                      ;; If num is "", then (string->number num) -> #f, get rid of them using filter.
                      ;; This occurs when there are layered parens and the first is processed,
                      ;; or after processing unary operator like !.
                      (filter (lambda (x) x)
                              (append (append out `(,(string->number num))) (stack-pop!-all ops)))
                      (let ([c (car in)])
                        (cond
                         [(char=? c #\space)
                          (printf "1~n~n")
                          (loop (add1 i) (cdr in) out num)]
                         [(is-num? c)
                          (printf "2: ~a~n~n" c)
                          (loop (add1 i) (cdr in) out (string-append num (list->string `(,c))))]
                         ;; cannot be preceded by numbers
                         [(char=? c #\()
                          (printf "3: ~a~n~n" c)
                          (assert (string=? num ""))
                          (stack-push! ops c)
                          (loop (add1 i) (cdr in) out num)]
                         [(char=? c #\))
                          (printf "4: ~a~n~n" c)
                          (loop (add1 i) (cdr in)
                                (append (append out `(,(string->number num))) (stack-pop!-unti ops #\())
                                "")]
                         [(is-op? c)
                          (printf "5: ~a~n" c)
                          (if (and (char=? c #\-)
                                   (or (stack-empty? ops) (and (char=? #\( (stack-peek ops))
                                                               (string=? "" num))))
                              ;; When - appears at the beginning or immediately after a
                              ;; left paren, it's negation, we explicitly put a zero to avoid
                              ;; using a different symbol for it.
                              ;; In (3-5), when at '-', the stack top is '(',  so add 0 only
                              ;; when num is "".
                              (begin (stack-push! ops c)
                                     (loop (add1 i) (cdr in) (append out '(0)) ""))
                              (let* ([p (get-prio c)]
                                     [pp (stack-peek ops)]
                                     ;; if stack is empty, just return 0
                                     [ppp (if pp (get-prio pp) 0)])
                                (if (> p ppp)
                                    (begin
                                      (stack-push! ops c)
                                      (loop (add1 i) (cdr in) (append out `(,(string->number num))) ""))
                                    ;; ops on stack have higher or equal precedence, pop them
                                    ;; first, then push the current one on the stack
                                    (let ([op* (let pop ([popped (cons (stack-pop! ops) '())])
                                                 ;; prio('(')=0, so it won't be popped
                                                 (if (or (stack-empty? ops) (< (get-prio (stack-peek ops)) p))
                                                     (reverse popped)
                                                     (pop (cons (stack-pop! ops) popped))))])
                                      (stack-push! ops c)
                                      (loop (add1 i) (cdr in)
                                            (append (append out `(,(string->number num))) op*) "")))))])))))))
          (valid-chars?)
          (valid-parens?)
          (evaluate (to-RPN)))
        (errorf "calc" "invalid expression: ~a" s))))

;;(printf "1*2+3 = ~a~n" (calc "1*2+3"))
;; (1 2 * 3 +)
(assert (= (calc "1*2+3") 5))

;;(printf "-(3+5) = ~a~n" (calc "-(3+5)"))
;; (0 3 5 + -)
(assert (= (calc "-(3+5)") -8))

;;(printf "-(3+5)!+9 = ~a~n" (calc "-(3+5)!+9"))
;; (0 3 5 + ! - 9 +)
(assert (= (calc "-(3+5)!+9") -40311))

;;(printf "-(3+5)!*9 = ~a~n" (calc "-(3+5)!*9"))
;; (0 3 5 + ! 9 * -)
(assert (= (calc "-(3+5)!*9") -362880))

;;(printf "-(3+5)!*9 = ~a~n" (calc "-(3+5)!*(-9)"))
;;(0 3 5 + ! 0 9 - * -)
(assert (= (calc "-(3+5)!*(-9)") 362880))

;;(printf "9*8^2 = ~a~n" (calc "9*8^2"))
;;(9 8 2 ^ *)
(assert (= (calc "9*8^2") 576))

;;(printf "5!^3*2-1 = ~a~n" (calc "5!^3*2-1"))
;;(5 ! 3 ^ 2 * 1 -)
(assert (= (calc "5!^3*2-1") 3455999))
(assert (= (calc "5! ^ 3 *  2  -1") 3455999))
