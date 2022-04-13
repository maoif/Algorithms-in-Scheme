#|
first-in-first-out storage

;;; API
(make-queue [type?])
Makes a queue. If type? is given, it's used as a type verifier,
such that when enqueueing a element of wrong type, an exception
is raised.

(enqueue q e)
Adds e to the end of queue q.

(dequeue q)
Removes the oldest element in the queue and retuens it.
|#

(library (data-structure queue)
  (export make-queue enqueue! dequeue!)
  (import (chezscheme))

  (define make-queue
    (case-lambda
      [() (mkqueue (lambda (x) #t) '())]
      [(p) (mkqueue p '())]))

  (define enqueue!
    (lambda (q e)
      (if ((queue-pred q) e)
          (queue-q*-set! q (append queue-q* q (list e)))
          (errorf "enqueue" "wrong type of element: ~a" e))))

  (define dequeue!
    (lambda (q)
      (if (null? q)
          #f
          (let* ([old (queue-q* q)]
                 [e (car old)])
            (queue-q*-set! q (cdr old))
            e))))

  (define-record-type (queue mkqueue queue?)
    (fields pred (mutable q*)))
  )
