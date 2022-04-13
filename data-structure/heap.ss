#|
Binary Heap

;;; API
(make-heap pred n)
Make an empty binary heap object of size n; elements are ordered by pred,
which must return #t or #f given the two elements from the heap.

(make-heap pred a)
Build a heap object using a, which could be a list or a vector;
elements are ordered by pred.

(heap-insert! h e)
Insert element e in the heap h.

(heap-top h)
Return the top element in h.

(heap-extract-top! h)
Return the top element in h, and delete it from h. Returns #f when no elements left.

(heap-reorder! h)
If the pred relies on external data, use this to reorder the heap.

(heap-size h)
Max number of elements h can store.

(heap-load h)
Numbers of elements currently in the heap.

;; internal
(heap-parent h e)
the parent of element e
(heap-left h e)
the left child of element e
(heap-right h e)
the right child of element e
(heap-ref h i)
(heap-set! h i v)

;;; Example
(define max-heap (make-heap > 50))
(define min-heap (make-heap < 100))

(do ([i 0 (add1 i 1)])
((= i 10))
(heap-insert max-heap i))

(heap-top max-heap)
-> 10
(heap-extract-top max-heap)
-> 10
(heap-top max-heap)
-> 9
(heap-size)
-> 50
(heap-load)
-> 9

;;; Implementation
binary heap is implemented using a vector. Every element in the
heap(vector) has a parent and two children, except for the the element,
which is the root, hence no parent...or it's its own parent

the parent-children relation is encoding as such:
for each element of index i(zero-indexed),
the index of its parent is floor((i-1)/2),
the index of its left child is 2i+1,
the index of its right child is 2i+2

the elements are ordered by pred, such that if a is the parent of b,
then pred(a, b) is true. When pred is <, the heap is a min heap. When
pred >, the heap is a max heap. When pred is...

|#

(module (make-heap heap-insert! heap-top heap-extract-top! heap-size heap-load heap-print)

  (define make-heap
    (lambda (pred arg)
      (cond [(number? arg) (make-heap-by-size pred arg)]
            [(or (vector? arg) (list? arg)) (make-heap-from pred arg)]
            [else (errorf "make-heap" "second argument should be number/list/vector")])))

  (define make-heap-by-size
    (lambda (p s)
      (mkheap p 0 (make-vector s))))

  (define make-heap-from
    (lambda (p ls)
      (let* ([ls (if (vector? ls) (vector-copy ls) (list->vector ls))]
             [n (vector-length ls)]
             [s (get-parent (sub1 n))]
             [h (mkheap p n ls)])
        ;; for a vector of length n, the parent index of the last element
        ;; is (parent (sub1 n)), all elements after this parent are leaves,
        ;; hence we heapify the elements backwards from this parent
        (do ([i s (sub1 i)])
            ((= i -1) h) ;; return h
          (heapify! h i)))))

  (define-record-type (heap mkheap heap?)
    (fields pred (mutable load) (mutable h*)))
  (define (get-parent x)
    (fxsrl (sub1 x) 1))
  (define (get-left x)
    (+ (* x 2) 1))
  (define (get-right x)
    (+ (* x 2) 2))
  (define heap-set!
    (lambda (h i v)
      (vector-set! (heap-h* h) i v)))
  (define heap-ref
    (lambda (h i)
      (vector-ref (heap-h* h) i)))
  (define heap-size
    (lambda (h)
      (vector-length (heap-h* h))))
  (define swap!
    (lambda (h i j)
      (let ([t (heap-ref h i)])
        (heap-set! h i (heap-ref h j))
        (heap-set! h j t))))

  ;; given index i, we assume its two sub-heaps are ordered
  ;; by pred, but i itself may violate pred, so we
  ;; recursively compare i and its two children and
  ;; send it down when pred is violated
  (define heapify!
    (lambda (h i)
      (let ([p (heap-pred h)]
            [load (heap-load h)])
        (let loop ([i i])
          (let* ([l (get-left i)]
                 [r (get-right i)]
                 [bigger (if (and (< l load)
                                  (not (p (heap-ref h i) (heap-ref h l))))
                             l
                             i)]
                 [bigger (if (and (< r load)
                                  (not (p (heap-ref h bigger) (heap-ref h r))))
                             r
                             bigger)])
            (unless (= i bigger)
              (swap! h i bigger)
              (loop bigger)))))))

  (define heap-insert!
    (lambda (h v)
      ;; if load is full, allocate a new vector twice the size
      (when (>= (add1 (heap-load h)) (heap-size h))
        (let ([load (heap-load h)]
              [new (make-vector (* 2 (heap-size h)))])
          (do ([i 0 (add1 i)])
              ((= i load))
            (vector-set! new i (heap-ref h i)))
          (heap-h*-set! h new)))
      (let ([load (heap-load h)]
            [pred (heap-pred h)])
        (heap-set! h load v)
        (heap-load-set! h (add1 load))
        ;; swap the new value with the parent if it violates the order
        (let loop ([i (sub1 (heap-load h))]
                   [p (get-parent (heap-load h))])
          (unless (< i 1)
            (when (not (pred (heap-ref h p) (heap-ref h i)))
              (swap! h i p)
              (loop p (get-parent p))))))))

  (define heap-top
    (lambda (h)
      (heap-ref h 0)))

  (define heap-extract-top!
    (lambda (h)
      (if (< (heap-load h) 1)
          #f
          (let ([t (heap-top h)])
            (heap-set! h 0 (heap-ref h (sub1 (heap-load h))))
            (heap-load-set! h (sub1 (heap-load h)))
            (heapify! h 0)
            t))))

  (define heap-print
    (lambda (h)
      (let ([v (heap-h* h)])
        (printf "~a~n" v)
        (printf "~a [0]~n" (heap-ref h 0))
        (tree-print h 1 '() #f)
        (tree-print h 2 '() #t))))

  (define tree-print
    (lambda (h i indent last?)
      (define (ind indent last?)
        (let loop ([ls indent])
          (unless (null? ls)
            (if (car ls)
                (begin (printf "~a" "│  ")
                       (loop (cdr ls)))
                (begin (printf "~a" "   ")
                       (loop (cdr ls))))))
        (if last?
            (printf "└──")
            (printf "├──")))
      (let ([l (get-left i)]
            [r (get-right i)])
        (ind indent last?)
        (printf "~a [~a]~n" (heap-ref h i) i)
        ;; (append ...): if the current node is the last of its parent,
        ;; don't print the vertical bar below it
        (when (< l (heap-load h))
          (tree-print h l (append indent `(,(not last?))) (>= r (heap-load h))))
        (when (< r (heap-load h))
          (tree-print h r (append indent `(,(not last?))) #t)))))

  )
