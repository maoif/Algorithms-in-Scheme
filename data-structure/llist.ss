#|
lists are inherent in Lisp:
empty list: '()
list with some elements: '(1 2 3 #f "hello" ('di . 'da))
take the first element: (car '(3 2 1)) -> 3
take the rest: (cdr '(3 2 1)) -> (2 1)
construct a list: (list 1 2 3) -> (1 2 3)
or, you can use cons: (cons 1 '(2 3)) -> (1 2 3), note that only when
the second argument is a list will you get a list, otherwise you'll
get a pair: (cons 1 2) -> (1 . 2)
add an element to the tail: (append '(1 2 3) 4) -> (1 2 3 4)

you can use recursion to iterate through the list, using null? to
control termination:
(let loop ([ls '(1 2 3)])
  (if (null? ls)
       'done
       (loop (cdr ls))))
BTW, this is a named let, whose name is "loop"

other operations:
(list->vector l)
    converts a list l into a vector, which has constant time access
(list-ref l k)
    returns the (k+1)th element

for more list operations such as map, fold, etc., see csug and rnrs

unfortunately, Lisp lists are singly-linked, non-circular, here we use records
to implement a doubly-linked, circular list: dclist
|#

(define-record-type llist
  (fields (mutable val) (mutable prev) (mutable next)))

(define llist-for-each
  (lambda (ll proc)
    ))
