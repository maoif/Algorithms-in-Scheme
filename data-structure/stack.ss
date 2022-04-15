#|
Stack, LIFO

;;; API
(make-stack)
(make-stack ls)
(make-stack equal)
(make-stack ls equal)
Returns a stack object.
`ls` has to a be a list or a vector, in this case
the elements of ls are pushed from head to tail.
`equal` is a equal predicate that determines whether two elements in
the stack are equal, this is used in (stack-pop!-until), by default
it is eq?.

(stack-push! s v)
Push `v` onto stack `s`.

(stack-pop! s)
Pop the stack.

(stack-pop!-until s e)
Pops the elements until `e` is encountered. If no element is equal to `e`, returns #f.
Returns the list of popped elements where the top element is the leftest.
In the case where stack in not emptied, the last element is included.

(stack-pop!-unti s e)
Similar to above, however the last element is popped but not included.

(stack-pop!-all s)
Pop all elements of s into a list.

(stack-peek s)
Return the stack top but not remove it.

(stack-load s)
Returns the number of elements currently in the stack.

(stack-empty? s)

Note that currently we don't have type check.

|#

(module (make-stack stack-empty? stack-load stack-push! stack-pop! stack-pop!-until
                    stack-pop!-unti stack-pop!-all stack-peek)
        (define-record-type (stack mk-stack stack?)
          (fields pred (mutable stk)))

        (define make-stack
          (case-lambda
           [() (mk-stack eq? '())]
           [(arg) (if (procedure? arg)
                      (mk-stack arg '())
                      (cond [(list? arg) (mk-stack eq? (reverse arg))]
                            [(vector? arg) (mk-stack eq? (reverse (vector->list arg)))]
                            [else (errorf "make-stack" "invalid argument: ~a" arg)]))]
           [(ls pred) (let ([ls (cond [(list? ls) (reverse ls)]
                                      [(vector? ls) (reverse (vector->list ls))]
                                      [else (errorf "make-stack" "not list nor vector: ~a" ls)])])
                        (if (procedure? pred)
                            (mk-stack pred ls)
                            (errorf "make-stack" "not a procedure: ~a" pred)))]))

        (define stack-empty? (lambda (s) (null? (stack-stk s))))
        (define stack-load (lambda (s) (length (stack-stk s))))

        (define stack-push!
          (lambda (s v) (stack-stk-set! s (cons v (stack-stk s)))))

        (define stack-pop!
          (lambda (s)
            (if (stack-empty? s)
                #f
                (let* ([stk (stack-stk s)]
                       [e (car stk)]
                       [rest (cdr stk)])
                  (stack-stk-set! s rest)
                  e))))

        (define stack-pop!-until
          (lambda (s e)
            (let ([stk (stack-stk s)]
                  [pred (stack-pred s)])
              (if (null? stk)
                  #f
                  (let loop ([res '()]
                             [stk stk])
                    (if (null? stk)
                        #f
                        (let ([v (car stk)])
                          (if (pred e v)
                              (begin (let ([res (reverse (cons v res))])
                                       (stack-stk-set! s (cdr stk))
                                       res))
                              (loop (cons v res) (cdr stk))))))))))

        (define stack-pop!-unti
          (lambda (s e)
            (let ([stk (stack-stk s)]
                  [pred (stack-pred s)])
              (if (null? stk)
                  #f
                  (let loop ([res '()]
                             [stk stk])
                    (if (null? stk)
                        #f
                        (let ([v (car stk)])
                          (if (pred e v)
                              (begin (let ([res (reverse res)])
                                       (stack-stk-set! s (cdr stk))
                                       res))
                              (loop (cons v res) (cdr stk))))))))))

        (define stack-pop!-all
          (lambda (s) (let ([stk (stack-stk s)])
                        (stack-stk-set! s '())
                        stk)))

        (define stack-peek
          (lambda (s)
            (if (stack-empty? s)
                #f
                (car (stack-stk s)))))
        )
