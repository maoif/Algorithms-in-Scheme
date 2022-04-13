#|
Multidimensional Table

;;; API
(make-table t d1 ...)
(make-table t d1 d2...)
Create a table named t of dimension n, n equal to the number of
dimension arguments.

Then you can use (t-get d1 ...) to get the value,
or (t-set! d1 ... v) to set the value of t[d1...] to v.
|#

(define-syntax make-table
  (lambda (x)
    (syntax-case x ()
      [(k table init d0 ...)
       ;; check whether `table` is a valid symbol
       (identifier? #'table)
       (with-syntax ([table-get-func (datum->syntax #'k
                                                    (string->symbol (format "~a-get" (syntax->datum #'table))))]
                     [table-get (datum->syntax #'k
                                               (format "~a-get" (syntax->datum #'table)))]
                     [table-set!-func (datum->syntax #'k
                                                     (string->symbol (format "~a-set!" (syntax->datum #'table))))]
                     [table-set! (datum->syntax #'k
                                                (format "~a-set!" (syntax->datum #'table)))]
                     ;; dimensions
                     [d*  #'(d0 ...)]
                     [args (generate-temporaries #'(d0 ...))]
                     [val (datum->syntax #'k (gensym "v")) ])
                    (with-syntax ([set-arg (append #'args (list #'val))])
                                 #`(module (table-get-func table-set!-func table)
                                           (define mk-table
                                             (lambda (d . arg*)
                                               (let ([v (make-vector d init)])
                                                 (unless (null? arg*)
                                                   (let loop ([v v]
                                                              [a* arg*])
                                                     (do ([i 0 (add1 i)]
                                                          [vv (make-vector (car a*) init)
                                                              (make-vector (car a*) init)])
                                                         ((= i (vector-length v)))
                                                       (unless (null? (cdr a*))
                                                         (loop vv (cdr a*)))
                                                       (vector-set! v i vv))))
                                                 v)))
                                           (define table (mk-table d0 ...))

                                           (define table-get-func
                                             (lambda args
                                               #,(let loop ([a* #'args]
                                                            [ds #'d*]
                                                            [index 0])
                                                   #`(if (<= 0 #,(car a*) (sub1 #,(car ds)))
                                                         #,(if (null? (cdr ds))
                                                               (let build-refs ([as (reverse #'args)])
                                                                 #`(vector-ref #,(if (null? (cdr as))
                                                                                     #'table
                                                                                     (build-refs (cdr as)))
                                                                               #,(car as)))
                                                               (loop (cdr a*) (cdr ds) (add1 index)))
                                                         (errorf table-get "invalid index for dimension ~a: ~a"
                                                                 #,(datum->syntax #'k index) #,(car a*))))))

                                           (define table-set!-func
                                             (lambda set-arg
                                               #,(let loop ([a* #'args]
                                                            [ds #'d*]
                                                            [index 0])
                                                   #`(if (<= 0 #,(car a*) (sub1 #,(car ds)))
                                                         #,(if (null? (cdr ds))
                                                               #`(vector-set!
                                                                  ;; the innermost vector takes the first argument
                                                                  ;; cdr: vector-set! has taken one arg off
                                                                  #,(let build-refs ([as (cdr (reverse #'args))])
                                                                      (if (null? as)
                                                                          ;; 1D case
                                                                          #'table
                                                                          #`(vector-ref #,(if (null? (cdr as))
                                                                                              #'table
                                                                                              (build-refs (cdr as)))
                                                                                        #,(car as))))
                                                                  #,(car (reverse #'args))
                                                                  val)
                                                               (loop (cdr a*) (cdr ds) (add1 index)))
                                                         (errorf table-set! "invalid index for dimension ~a: ~a"
                                                                 #,(datum->syntax #'k index) #,(car a*)))))))))])))


;; (define table-for-each
;;   (lambda ()))
;; (define table-map
;;   (lambda ()))
;; (define table-iterate
;;   (lambda ()))
