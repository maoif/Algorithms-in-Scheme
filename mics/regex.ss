#|
Simple Regex Matcher

;;; API
(make-regex p)
Returns a regex object (essentially an ɛ-NFA) for the follwing procedures.

(regex-match r text)
Returns a pair, whose car is a list of the index range of the matched
strings in `text` by regex object `r`, whose cdr is a list of the
matched substrings.

(regex-contains? r text)
Determines whether string `text` has patterns decribed by regex `r`.

(regex-to-dot r path)
Converts regex object `r` to a dot file for Graphviz visualization.

;;; Implementation

This implementation simply illustrates the mechanism of regex.
There is no back reference, numbered matches and other fancy features.

We use two passes to convert a regex string into an ɛ-NFA. The first pass
(tokenize) transforms the regex string into a tree that happens to
resemble SRE (see https://srfi.schemers.org/srfi-115/srfi-115.html),
which ficilitates the next pass, (parse), which transforms the SRE-like
representation into ɛ-NFA according to the rules as described in:
Automata and Computability——A Programmer’s Perspective by Ganesh
Lalitha Gopalakrishnan and Introduction to Automata Theory, Languages,
and Computation (3rd Ed.) by John E. Hopcroft, Rajeev Motwani, Jeffrey
D. Ullman.

Things like [a-z], a+, a? can be reduced to (a|b|...|z), aa*, (ɛ|a),
so we only need three types of NFA-building procedures as you can see
in (build-NFA).

TODO
'NUM, 'LETTER, etc., in the NFA.
Merge duplicated nodes as in ab|abc|abcd.

|#

(include "../tools.ss")
(include "../data-structure/stack.ss")

(define-syntax debug
  (syntax-rules ()
    [(_ x ...) '()]))
#|
(define-syntax debug
(syntax-rules ()
[(_ x ...) (begin x ...)]))
|#

(define gen-state
  (let ([n 1]) (lambda () (let ([r n]) (set! n (add1 n)) r))))

(define-record-type NFA
  (fields regex (mutable init) (mutable final)
          ;; '((s0 . ((label0 . t0) (label1 . t1) ...)) ...)
          (mutable states)))

(define NFA-add-edge!
  (lambda (n src label target)
    (let* ([s* (NFA-states n)]
           [src-delta (assoc src s*)]
           [delta (cdr src-delta)])
      (set-cdr! src-delta (cons (cons label target) delta)))))

(define NFA-eliminate-epsilon-internal!
  (lambda (n)
    (let ([states (NFA-states n)])
      ;; Returns a list of parents of i, if p is in the list,
      ;; include it in the result only when p has other edges to i (parallel edges).
      ;; Returns '() if no parents found.
      (define find-input
        (lambda (i p)
          (debug (printf "find input for ~a  parent: ~a~n" i p))
          (let ([res '()])
            (for-each (lambda (s)
                        (if (= (car s) p)
                            ;; we could have parallel edges as in a?
                            (when (< 1 (length (filter (lambda (to) (= i (cdr to))) (cdr s))))
                              (set! res (cons (car s) res)))
                            (for-each (lambda (to)
                                        ;; In fact we can terminate this loop
                                        ;; once a case is found.
                                        (when (and (= i (cdr to))
                                                   (not (memq (car s) res)))
                                          (set! res (cons (car s) res))))
                                      (cdr s))))
                      states)
            res)))
      (define get-epsilons
        (lambda (i)
          (let loop ([res '()]
                     [edges (cdr (assoc i states))])
            (if (null? edges)
                res
                (if (eq? (caar edges) 'epsilon)
                    (loop (cons (car edges) res) (cdr edges))
                    (loop res (cdr edges)))))))
      ;; 1. merge `from`'s outputs to `to`'s
      ;; 2. delete `from` from `to`'s output list
      ;; 3. make all edges that originally pointed to `from` point to `to`
      ;; 4. update init and/or final states
      ;; 5. remove `from` from NFA
      (define transplant-simple!
        (lambda (to from)
          (let* ([from-out* (cdr (assoc from states))]
                 [to-state (assoc to states)])
            (debug (printf "from-out*: ~a~n" from-out*)
                   (printf "to-state: ~a~n" to-state))
            (set-cdr! to-state
                      (append (remp (lambda (x) (= from (cdr x))) (cdr to-state))
                              from-out*))
            ;; e.g., in a|b|c
            (when (= (NFA-final n) from) (NFA-final-set! n to))
            ;; e.g., in (ab)*, where star is top-level
            (when (= (NFA-init n) from) (NFA-init-set! n to))
            (NFA-states-set! n (remp (lambda (x) (= from (car x))) states))
            (set! states (NFA-states n))
            (debug (printf "after transplant-simple!: ~a~n" n)))))
      (define transplant-hard!
        (lambda (to from p*)
          (let* ([p*-state* (filter (lambda (x) (memq (car x) p*)) states)]
                 [from-out* (cdr (assoc from states))]
                 [to-state (assoc to states)])
            (set-cdr! to-state from-out*)
            (for-each (lambda (ps)
                        ;; Parallel edges could occur e.g., in a|b|c, filter
                        ;; takes care of all of them.
                        (for-each (lambda (y) (set-cdr! y to))
                                  (filter (lambda (x) (= from (cdr x))) (cdr ps))))
                      p*-state*)
            (when (= (NFA-final n) from) (NFA-final-set! n to))
            (when (= (NFA-init n) from) (NFA-init-set! n to))
            (NFA-states-set! n (remp (lambda (x) (= from (car x))) states))
            (set! states (NFA-states n))
            (debug (printf "after transplant-hard!: ~a~n" n)))))
      ;; prevent self-loops
      (let ([visited (make-eq-hashtable)])
        (let loop ([p (NFA-init n)])
          (when (and (not (hashtable-contains? visited p))
                     ;; p may be deleted as in a?
                     (memp (lambda (x) (= p (car x))) states))
            (let* ([c* (get-epsilons p)]
                   [s* (map cdr c*)]
                   [input* (map (lambda (x) (find-input x p)) s*)])
              (debug (printf "epsilons of ~a: ~a~n" p c*))
              (for-each (lambda (s pp)
                          (if (null? pp)
                              (transplant-simple! p s)
                              ;; When the parent has more than one out edges,
                              ;; we cannot transplant since in the case where
                              ;; s also has inputs, the transplanted automaton
                              ;; is messed up.
                              (if (= (length (cdr (assoc p states))) 1)
                                  (transplant-hard! p s pp)
                                  (hashtable-set! visited p 'visited))))
                        s* input*)
              ;; p may inherit some eliminable epsilons from its deleted child(dren)
              (unless (null? (get-epsilons p)) (loop p))
              (hashtable-set! visited p 'visited)
              (let ([ps (assoc p states)])
                ;; p could disappear in a loop: ((0 . (b . 2)) (2 . (epsilon . 0)))
                ;; as in ab*, so check ps
                (debug (printf "ps: ~a~nstates: ~a~n" ps states))
                (when ps (for-each loop (map cdr (cdr ps))))))))))))

(define NFA-eliminate-epsilon
  (lambda (n)
    ;; Run twice in case a node p inherits some eliminable epsilons
    ;; but also has some ineliminable ones, otherwise the former
    ;; ones are not eliminated.
    (NFA-eliminate-epsilon-internal! n)
    (NFA-eliminate-epsilon-internal! n)
    n))

(define make-regex
  (lambda (p)
    (define metas '(#\( #\) #\[ #\] #\? #\. #\+ #\* #\|))
    (define escapable? (lambda (c) (memv c metas)))
    (define check-chars
      (lambda ()
        (string-iterate p (lambda (i c)
                            (unless (or (char-alphabetic? c) (char-numeric? c) (char=? c #\space)
                                        (memq c '(#\( #\) #\[ #\] #\? #\. #\+ #\* #\| #\- #\\)))
                              (errorf "make-regex" "invalid char ~a at position ~a" c i))))))
    (define check-parens
      (lambda ()
        (let ([parens (make-stack char=?)]
              [brackets (make-stack char=?)])
          (string-iterate p (lambda (i c)
                              ;; Don't check things like \(bla* or haha\), whose
                              ;; parens are unmatched but they are valid regex.
                              (when (or (= i 0) (not (char=? (string-ref p (sub1 i)) #\\)))
                                (case c
                                  [(#\() (stack-push! parens (cons i c))]
                                  [(#\[) (stack-push! brackets (cons i c))]
                                  [(#\)) (if (stack-empty? parens)
                                             (errorf "make-regex" "unmatched right parenthesis at position ~a in ~a" i p)
                                             (stack-pop! parens))]
                                  [(#\]) (if (stack-empty? brackets)
                                             (errorf "make-regex" "unmatched right bracket at position ~a in ~a" i p)
                                             (stack-pop! brackets))]))))
          (unless (stack-empty? parens)
            (errorf "make-regex" "unmatched left parenthesis at position ~a in ~s" (car (stack-pop! parens)) p))
          (unless (stack-empty? brackets)
            (errorf "make-regex" "unmatched left bracket at position ~a in ~s" (car (stack-pop! brackets)) p)))))
    ;; regex is tokenized into:
    ;; - '(CON R S) if regex is RS
    ;; - '(OR R S) if regex is R|S
    ;; - '(STAR R) if regex is R*
    ;; - '(CON R (STAR R)) if regex is R+, hence RR*
    ;; - '(OR epsilon R) if regex is R?
    ;; - 'DOT if regex is .
    ;; - 'NUM if regex is [0-9]
    ;; - 'LOWER if regex is [a-z]
    ;; - 'UPPER if regex is [A-Z]
    ;; - 'ALPHA if regex is [a-zA-Z] or [A-Za-z]
    ;; - 'ALPHANUM if regex is a permutation of a-z, A-Z and 0-9 in []
    ;; - '(OR x ... y) if regex is [x-y] where y is alphanumerically larger than x
    ;; - '(OR x y z ...) if regex is [xyz], where y is not -
    ;; All other patterns that occur in [] will be treated as alternative literals.
    (define tokenize
      (lambda ()
        (define build-chars-range
          (lambda (s e)
            (let* ([s (char->integer s)]
                   [e (char->integer e)]
                   [len (- e s)])
              (let loop ([i 0] [res '()])
                (if (= i (add1 len))
                    (reverse res)
                    (loop (add1 i) (cons (integer->char (+ s i)) res)))))))
        ;; process forms like [0-9] and [a-zA-Z]
        (define tokenize-range
          (lambda (s e)
            ;; [0-9a-z]
            ;; s123456e
            (let ([len (- e s)])
              (cond [(and (= len 4)
                          (char=? (string-ref p (+ s 2)) #\-))
                     (let ([L (string-ref p (+ s 1))]
                           [R (string-ref p (+ s 3))])
                       ;; This is not perfect since we are permitting things
                       ;; like [0-z] which ranges from ASCII 48 to 122,
                       ;; which makes the NFA huge.
                       (if (char<=? L R)
                           `(OR ,@(build-chars-range L R))
                           (errorf "make-regex" "invalid range: [~a-~a]" L R)))]
                    [(and (= len 7)
                          (char=? (string-ref p (+ s 2)) #\-)
                          (char=? (string-ref p (+ s 5)) #\-))
                     (let ([L1 (string-ref p (+ s 1))]
                           [R1 (string-ref p (+ s 3))]
                           [L2 (string-ref p (+ s 4))]
                           [R2 (string-ref p (+ s 6))])
                       (if (and (char<=? L1 R1) (char<=? L2 R2))
                           `(OR ,@(build-chars-range L1 R1) ,@(build-chars-range L2 R2))
                           (errorf "make-regex" "invalid range: [~a-~a] or [~a-~a]" L1 R1 L2 R2)))]
                    [(and (= len 10)
                          (char=? (string-ref p (+ s 2)) #\-)
                          (char=? (string-ref p (+ s 5)) #\-)
                          (char=? (string-ref p (+ s 8)) #\-))
                     (let ([L1 (string-ref p (+ s 1))]
                           [R1 (string-ref p (+ s 3))]
                           [L2 (string-ref p (+ s 4))]
                           [R2 (string-ref p (+ s 6))]
                           [L3 (string-ref p (+ s 7))]
                           [R3 (string-ref p (+ s 9))])
                       (if (and (char<=? L1 R1) (char<=? L2 R2) (char<=? L3 R3))
                           `(OR ,@(build-chars-range L1 R1) ,@(build-chars-range L2 R2) ,@(build-chars-range L3 R3))
                           (errorf "make-regex" "invalid range: [~a-~a] or [~a-~a] or [~a-~a]" L1 R1 L2 R2 L3 R3)))]
                    [else `(OR ,@(string->list (substring p (add1 s) e)))]))))
        (define clean-OR
          (lambda (tok)
            (if (list? tok)
                (let ([seen '()])
                  (for-each (lambda (t)
                              (unless (memv t seen)
                                (set! seen (cons t seen))))
                            (cdr tok))
                  (cons 'OR (reverse seen)))
                tok)))
        ;; transform singletons like (... (CON/OR R) ...) into (... R ...)
        (define flatten
          (lambda (ls)
            (let ([head (car ls)] [tail (cdr ls)])
              (if (= 1 (length tail))
                  (if (eq? head 'STAR)
                      (if (list? (car tail))
                          `(STAR ,(flatten (car tail)))
                          `(STAR ,(car tail)))
                      (if (list? (car tail))
                          (flatten (car tail))
                          (car tail)))
                  (let loop ([res '()]
                             [ls tail])
                    (if (null? ls)
                        `(,head ,@(reverse res))
                        (let ([x (car ls)])
                          (if (list? x)
                              (loop (cons (flatten x) res) (cdr ls))
                              (loop (cons x res) (cdr ls))))))))))
        (define find-matching-bracket
          (lambda (i)
            (let ([s (make-stack char=?)])
              (let loop ([i i])
                (when (>= i (string-length p))
                  (errorf "make-regex" "unable to find matching bracket from position ~a" i))
                (cond [(char=? (string-ref p i) #\[) (stack-push! s #\[) (loop (add1 i))]
                      [(char=? (string-ref p i) #\]) (stack-pop! s)
                       (if (stack-empty? s) i (loop (add1 i)))]
                      [else (loop (add1 i))])))))

        (let ([out (make-stack eqv?)]
              ;; Like the Shunting-Yard, if we encounter '|',
              ;; pop all elements into a list, add the list to aux*,
              ;; and continue. To honor left paren, 'LP is also push onto it.
              [aux* (make-stack)]
              [n (string-length p)])
          (let loop ([i 0])
            (debug (printf "i: ~a~n~nout: ~a~naux*: ~a~n" i out aux*))
            (if (= i n)
                (if (stack-empty? aux*)
                    (flatten `(CON ,@(reverse (stack-pop!-all out))))
                    ;; we have a top-level OR
                    (let lp ([res '()] [tt* (stack-pop!-all aux*)])
                      ;;(printf "aux* not empty~n")
                      (if (null? tt*)
                          (flatten `(OR ,@res (CON ,@(reverse (stack-pop!-all out)))))
                          (lp (cons (cons 'CON (car tt*)) res) (cdr tt*)))))
                (let ([c (string-ref p i)])
                  (case c
                    [(#\.) (stack-push! out 'DOT) (loop (add1 i))]
                    ;; +, *, ? cannot follow empty string or left paren
                    ;; '(' could be pushed onto stack if it's escaped, so we use 'LP
                    [(#\+) (if (or (stack-empty? out) (eq? 'LP (stack-peek out)))
                               (errorf "make-regex" "invalid use of + at position ~a" i)
                               (let ([last (stack-pop! out)])
                                 (stack-push! out `(CON ,last (STAR ,last)))
                                 (loop (add1 i))))]
                    [(#\?) (if (or (stack-empty? out) (eq? 'LP (stack-peek out)))
                               (errorf "make-regex" "invalid use of ? at position ~a" i)
                               (let ([last (stack-pop! out)])
                                 (stack-push! out `(OR epsilon ,last))
                                 (loop (add1 i))))]
                    [(#\*) (if (or (stack-empty? out) (eq? 'LP (stack-peek out)))
                               (errorf "make-regex" "invalid use of * at position ~a" i)
                               (let ([last (stack-pop! out)])
                                 (stack-push! out `(STAR ,last))
                                 (loop (add1 i))))]
                    [(#\[) (let ([j (find-matching-bracket i)])
                             (stack-push! out (clean-OR (tokenize-range i j)))
                             (loop (add1 j)))]
                    [(#\() (stack-push! out 'LP) (stack-push! aux* 'LP) (loop (add1 i))]
                    [(#\))
                     ;; Add CON in the front of every element from aux*, since there
                     ;; is nothing among them. Then add OR, since aux* contains the
                     ;; elements that  need to be ORed.
                     (let ([c* (reverse (stack-pop!-unti out 'LP))]
                           [t* (let lp ([res '()] [tt* (stack-pop!-unti aux* 'LP)])
                                 (if (null? tt*)
                                     res
                                     (lp (cons (cons 'CON (car tt*)) res) (cdr tt*))))])
                       (if (null? t*)
                           (stack-push! out `(CON ,@c*))
                           (stack-push! out `(OR ,@t* (CON ,@c*)))))
                     (loop (add1 i))]
                    [(#\|) (if (or (stack-empty? out) (eq? 'LP (stack-peek out)))
                               (errorf "make-regex" "invalid use of * at position ~a" i)
                               (stack-push! aux* (reverse (if (stack-contains? out 'LP)
                                                              (stack-pop!-to out 'LP)
                                                              (stack-pop!-all out)))))
                     (loop (add1 i) )]
                    [(#\\) (if (= (add1 i) n)
                               (errorf "make-regex" "invalid escape at position ~a" i)
                               (if (escapable? (string-ref p (add1 i)))
                                   (begin (stack-push! out (string-ref p (add1 i)))
                                          (loop (+ i 2)))
                                   (errorf "make-regex" "invalid escape at position ~a" i)))]
                    [else (stack-push! out c) (loop (add1 i))])))))))
    (define parse
      (lambda (tok*)
        (define build-NFA
          (lambda (tok)
            (define con-NFA!
              (lambda (n1 n2)
                (debug (printf "con-NFA!~nn1: ~a~nn2: ~a~n" n1 n2))
                (let ([old-final (NFA-final n1)]
                      [new-final (NFA-final n2)]
                      [n2-init (NFA-init n2)])
                  (NFA-final-set! n1 new-final)
                  (NFA-states-set! n1 (append (NFA-states n2) (NFA-states n1)))
                  (NFA-add-edge! n1 old-final 'epsilon n2-init))))
            (define or-NFA!
              (lambda (n1 n2)
                (debug (printf "or-NFA!~nn1: ~a~nn2: ~a~n" n1 n2))
                (let ([n1-init (NFA-init n1)]
                      [n2-init (NFA-init n2)]
                      [n1-final (NFA-final n1)]
                      [n2-final (NFA-final n2)])
                  (NFA-states-set! n1 (append (NFA-states n2) (NFA-states n1)))
                  (NFA-add-edge! n1 n1-init 'epsilon n2-init)
                  (NFA-add-edge! n1 n2-final 'epsilon n1-final))))
            (define star-NFA
              (lambda (n)
                (debug (printf "star-NFA~n"))
                (let* ([init (gen-state)]
                       [final (gen-state)]
                       [NFA (make-NFA p init final `((,init . ((epsilon . ,final)))
                                                     (,final . ())))])
                  (let ([n-init (NFA-init n)]
                        [n-final (NFA-final n)])
                    (NFA-states-set! NFA (append (NFA-states n) (NFA-states NFA)))
                    (NFA-add-edge! NFA init 'epsilon n-init)
                    (NFA-add-edge! NFA n-final 'epsilon init)
                    NFA))))
            (debug (printf "build-NFA: ~a~n" tok))
            (if (list? tok)
                (case (car tok)
                  [(CON)
                   (let* ([NFAs (map (lambda (x) (build-NFA x)) (cdr tok))]
                          [NFA (car NFAs)])
                     (let loop ([rest (cdr NFAs)])
                       (if (null? rest)
                           NFA
                           (begin (con-NFA! NFA (car rest))
                                  (loop (cdr rest))))))]
                  [(OR)
                   (let ([NFAs (map (lambda (x) (build-NFA x)) (cdr tok))]
                         [NFA (let ([init (gen-state)] [final (gen-state)])
                                (make-NFA p init final `((,init . ()) (,final . ()))))])
                     (let loop ([rest NFAs])
                       (if (null? rest)
                           NFA
                           (begin (or-NFA! NFA (car rest))
                                  (loop (cdr rest))))))]
                  [(STAR) (star-NFA (build-NFA (cadr tok)))]
                  [else (errorf "parse" "unknown regex type: ~a" (car tok))])
                (let ([init (gen-state)]
                      [final (gen-state)])
                  (make-NFA p init final `((,init . ((,tok . ,final)))
                                           (,final . ())))))))
        (build-NFA tok*)))
    (check-chars)
    (check-parens)
    (let* ([tok (tokenize)]
           [nfa (parse tok)])
      (debug (printf "pattern: ~a~n" p)
             (printf "tok: ~a~n" tok)
             (printf "NFA: ~a~n~n" nfa))
      nfa)))

(define regex-match
  (lambda (r text)
    (if (NFA? r)
        (let ([res '()]
              [n (string-length text)])
          ;; If no match is found, return #f.
          ;; If the current states is not final and no char is available, return #f.
          ;; Given the current state and the next char, if no available transitions
          ;; and epsilon transitions, return #f.
          ;; Returning #f is done by try-next!, when no alternatives are available.
          ;; If the current states contains the final state and we still have available
          ;; transitions, go for it (good for star).
          ;; If the current states contains the final state and no other transitions
          ;; are available, a match is found, return the index. Then search begins
          ;; from this index.
          (define run-NFA
            (lambda (i)
              (call/cc (lambda (return)
                         (let ([states (NFA-states r)]
                               [final (NFA-final r)]
                               ;; Use a stack to store alternative transitions if
                               ;; the current state is a dead end.
                               ;; '((cont1 . (s1 s2 ...)) (cont2 . (s3 s4 ...)) ...)
                               [alternatives (make-stack)])
                           (define add-next!
                             (lambda (n) (stack-push! alternatives n)))
                           (define try-next!
                             (lambda ()
                               (debug (printf "try-next!: ~a~n" alternatives))
                               (if (stack-empty? alternatives)
                                   (return #f)
                                   (let ([top (stack-peek alternatives)])
                                     (if (null? (cdr top))
                                         (begin (stack-pop! alternatives) (try-next!))
                                         (let ([next (cadr top)])
                                           (set-cdr! top (cddr top))
                                           ((car top) next)))))))
                           (let loop-nfa ([i i] [curr (NFA-init r)])
                             ;; Get all available next states.
                             ;; Partition them so we always try non-ɛ transitions first.
                             ;; next*: '((DOT . s1) (c . s2) (ɛ . s3) ...)
                             (let-values ([(L R) (partition (lambda (x) (not (eqv? (car x) 'epsilon)))
                                                            (filter (lambda (e)
                                                                      (or (eqv? (car e) 'epsilon)
                                                                          (if (= i n)
                                                                              #f
                                                                              (or (eqv? (car e) 'DOT)
                                                                                  (eqv? (car e) (string-ref text i))))))
                                                                    (cdr (assoc curr states))))])
                               (debug (printf "L: ~a~n" L))
                               (debug (printf "R: ~a~n" R))
                               (if (null? (append L R))
                                   (if (= curr final)
                                       (return i)
                                       (try-next!))
                                   (if (= i n)
                                       (if (= curr final)
                                           (return i)
                                           (if (null? R)
                                               (try-next!)
                                               (let ([next (call/cc (lambda (k)
                                                                      (add-next! (cons k (cdr R)))
                                                                      (car R)))])
                                                 (loop-nfa i (cdr next)))))
                                       ;; If we have transitions and i is not the end,
                                       ;; always transition, don't check (= i n), so that
                                       ;; star works correctly.
                                       (let ([next* (append L R)])
                                         (let ([next (call/cc (lambda (k)
                                                                (add-next! (cons k (cdr next*)))
                                                                (car next*)))])
                                           (if (eqv? (car next) 'epsilon)
                                               (loop-nfa i (cdr next))
                                               (loop-nfa (add1 i) (cdr next))))))))))))))
          (debug (printf "~nregex-match===============================~n~a~n" r))
          (let loop ([i 0])
            (debug (printf "match res: ~a~n" res))
            (if (>= i (string-length text))
                (let ([res (reverse res)])
                  (cons res (map (lambda (x) (substring text (car x) (cdr x))) res)))
                (let ([m (run-NFA i)])
                  ;; Is this necessary?
                  (if (and m (not (= i m)))
                      (begin (set! res (cons (cons i m) res))
                             (loop m))
                      (loop (add1 i)))))))
        (errorf "regex-match" "not a regex object: ~a" r))))

(define regex-contains?
  (lambda (r text) (not (null? (regex-match r text)))))

(define regex-to-dot
  (lambda (r path)
    (if (NFA? r)
        (let ([states (NFA-states r)]
              [init (NFA-init r)]
              [final (NFA-final r)])
          (call-with-output-file path
            (lambda (p)
              (put-string p "digraph {")
              (fresh-line p)
              (put-string p "labelloc=\"t\"; fontname=\"monospace\";")
              (put-string p (format "label=\"~a\";~n" (NFA-regex r)))
              (put-string p "node [fontname=monospace]; rankdir=LR;")
              (fresh-line p)
              (put-string p "S [shape=point];")
              (put-string p (format "S -> ~a;" init))
              (for-each (lambda (from)
                          (for-each (lambda (to)
                                      (put-string p (format "~a -> ~a[label=\"~a\"];~n"
                                                            (car from) (cdr to) (let ([l (car to)])
                                                                                  (if (eq? l 'epsilon) "ɛ" l)))))
                                    (cdr from)))
                        states)
              (put-string p (format "~a[peripheries=2];" final))
              (put-string p "}")))
          (printf "regex-to-dot: written ~a~n" path))
        (errorf "regex-to-dot" "not a regex object: ~a" r))))

#|
(make-regex "a")
(make-regex "ab")
(make-regex "abc")

(make-regex "a|b")
(make-regex "a|b|c")
(make-regex "abc|def|(ha)+|yes")
(make-regex "[a-zA-Z0-9]")

(make-regex "[0-9]")
(make-regex "[c-h]")
(make-regex "[-xyz]")

(make-regex "(ab)c")
(make-regex "(a|b)")
(make-regex "ab|(cd|e)")
(make-regex "a?")
(make-regex "a*")
(make-regex "a+")
(make-regex ".")
(make-regex ".+")
;;(make-regex ".+?")
(make-regex "\\(scheme\\)")
(make-regex "[a-g]*")
(make-regex "[a-g]+")
(make-regex "[a-g]?")
(make-regex "(ab)*")
(make-regex "(ab)*(cd)+")
(make-regex "ab|([ch]at)*")
|#

;; Uncomment the following to get a list of NFA diagrams.
#|
(define count 0)
(for-each (lambda (x)
            (regex-to-dot x (format "nfa-~a.dot" count))
            (set! count (add1 count)))
          `(,(NFA-eliminate-epsilon (make-regex "a"))
            ,(NFA-eliminate-epsilon (make-regex "ab"))
            ,(NFA-eliminate-epsilon (make-regex "abc"))
            ,(NFA-eliminate-epsilon (make-regex "a|b"))
            ,(NFA-eliminate-epsilon (make-regex "a|b|c"))
            ,(NFA-eliminate-epsilon (make-regex "[a-zA-Z0-9]"))
            ,(NFA-eliminate-epsilon (make-regex "[0-9]"))
            ,(NFA-eliminate-epsilon (make-regex "[c-h]"))
            ,(NFA-eliminate-epsilon (make-regex "[-xyz]"))
            ,(NFA-eliminate-epsilon (make-regex "(ab)c"))
            ,(NFA-eliminate-epsilon (make-regex "(a|b)"))
            ,(NFA-eliminate-epsilon (make-regex "ab|(cd|e)"))
            ,(NFA-eliminate-epsilon (make-regex "a*"))
            ,(NFA-eliminate-epsilon (make-regex "a+"))
            ,(NFA-eliminate-epsilon (make-regex "."))
            ,(NFA-eliminate-epsilon (make-regex ".+"))
            ,(NFA-eliminate-epsilon (make-regex "\\(scheme\\)"))
            ,(NFA-eliminate-epsilon (make-regex "[a-g]*"))
            ,(NFA-eliminate-epsilon (make-regex "[a-g]+"))
            ,(NFA-eliminate-epsilon (make-regex "(ab)*"))
            ,(NFA-eliminate-epsilon (make-regex "(ab)*(cd)+"))
            ,(NFA-eliminate-epsilon (make-regex "abc|def|(ha)+|yes"))
            ,(NFA-eliminate-epsilon (make-regex "a?"))
            ,(NFA-eliminate-epsilon (make-regex "[a-g]?"))
            ,(NFA-eliminate-epsilon (make-regex "ab|([ch]at)*")))
          )
|#

;;(regex-match (make-regex "a") "a")
;;(regex-match (make-regex "a|b") "abcda")
;;(regex-match (NFA-eliminate-epsilon (make-regex "a*.d")) "aabcdaaaacd")
;;(regex-match (NFA-eliminate-epsilon (make-regex "ab*")) "aabbbcdaabaacd")
;;(regex-match (NFA-eliminate-epsilon (make-regex "b*")) "bbba")
;;(printf "~a~n" (regex-match (NFA-eliminate-epsilon (make-regex "(ab)*")) "abb1abbbababab53a"))
;;(printf "~a~n" (regex-match (NFA-eliminate-epsilon (make-regex "abc|def|(ha)+|yes")) "abyesabchadlsdefhahaokey"))
;;(printf "~a~n" (regex-match (NFA-eliminate-epsilon (make-regex "(ab)?")) "abb1abbbababab53a"))
;;(printf "~a~n" (regex-match (NFA-eliminate-epsilon (make-regex "jk")) "jiukujkhsj"))
