#|
Huffman encoding

;;; API
(huffman-gen text)
Analyzes the text, constructs a dictionary, and returns it.

(huffman-encode e text)
Encodes the text using the given dictionary, returns a list of
encoded binary strings, e.g., ("1010" "10101" "1101").

(huffman-decode e sec)
Encode the given binary string sec using e as dictionary, returns the
original text.

;;; Implementation
The dictionary constructed by huffman-gen is pair, whose car is a hashtable
for encoding, whose cdr is a binary tree for decoding.

|#

;; todo decoder tree, dict is not right

(define huffman-gen
  (lambda (text)
    ;; returns the index of the element with the least frequency
    (define find-min
      (lambda (v)
        (let ([n (most-positive-fixnum)]
              [index 0])
          (do ([i 0 (add1 i)])
              ((= i (vector-length v)) (if (= n (most-positive-fixnum)) #f index))
            (when (vector-ref v i)
              (when (< (car (vector-ref v i)) n)
                (set! index i)
                (set! n (car (vector-ref v i)))))))))
    (define gen-table
      (lambda (t)
        ;; Here we build the encoder and decoder at the same time.
        ;; Encoder is a hashtable whose key is a letter and the value is the encoded 01 string.
        ;; Decoder is a pair tree whereby getting into the car is 1 and getting into the cdr is 0.
        (let ([encoder (make-eq-hashtable)]
              [decoder '(#f . #f)])
          (let loop ([code '()]
                     [dec decoder]
                     [tree t])
            (if (char? (cdr tree))
                (begin (hashtable-set! encoder (cdr tree) (list->string (reverse code)))
                       (set! dec (cdr tree)))
                (begin (set-car! dec (loop (cons #\1 code) (cons #f #f) (cadr tree)))
                       (set-cdr! dec (loop (cons #\0 code) (cons #f #f) (cddr tree)))))
            dec)
          (cons encoder decoder))))
    (let ([ht (make-eq-hashtable)])
      (string-for-each (lambda (x) (hashtable-update! ht x add1 0)) text)
      (let* ([f (hashtable-cells ht)]
             [freq (vector-map (lambda (x) (cons (cdr x) (car x))) f)])
        ;; now freq is of the form: #((n1 . a) (n2 . b) (n3 . c) ...)
        (let loop ()
          (let* ([i1 (find-min freq)]
                 [t1 (vector-ref freq i1)])
            ;; Since this loop marks one element in freq at a time,
            ;; it is guaranteed that i1 won't be #f; only i2 might
            ;; (unless i1 (printf "bang!~n")).
            (vector-set! freq i1 #f)
            (let ([i2 (find-min freq)])
              (if i2
                  (begin (vector-set! freq i2 (cons (+ (car t1) (car (vector-ref freq i2)))
                                                    (cons t1 (vector-ref freq i2))))
                         (loop))
                  (gen-table t1)))))))))

(define huffman-encode
  (lambda (e text)
    (let ([dict (car e)]
          [res '()])
      (unless (hashtable? dict) (errorf "huffman-encode" "invalid Huffman dictionary: ~a" e))
      (string-for-each (lambda (c) (set! res (cons (hashtable-ref dict c #f) res))) text)
      (let ([r ""])
        (for-each (lambda (s) (set! r (string-append r s))) (reverse res))
        r))))

(define huffman-decode
  (lambda (e sec)
    (if (and (pair? e) (hashtable? (car e)) (pair? (cdr e)))
        (let ([dec (cdr e)]
              [res '()])
          (let loop ([s (string->list sec)]
                     [d dec])
            (if (null? s)
                (list->string
                 ;; If we arrive at the end, then, if the secret text does
                 ;; come from the given encoder, d must a char.
                 (reverse (if (char? d)
                              (cons d res)
                              #f)))
                (if (char? d)
                    (begin (set! res (cons d res)) (loop s dec))
                    (if (char=? (car s) #\1)
                        (loop (cdr s) (car d))
                        (loop (cdr s) (cdr d)))))))
        (errorf "huffman-code" "invalid decoder"))))

(define huffman-print
  (lambda (e)
    (unless (and (pair? e) (hashtable? (car e)) )
      (errorf "huffman-print" "invalid Huffman dictionary: ~a" e))
    (printf "encoder:~n")
    (vector-for-each (lambda (x) (printf "~a~n" x)) (hashtable-cells (car e)))
    (printf "decoder:~n")
    (tree-print (cdr e))))

(define tree-print
  (lambda (t)
    (define f
      (lambda (t dir)
        (let loop ([t t]
                   [dir dir]
                   [code '()])
          (if (char? t)
              (printf "(~a . ~a)~n" t (list->string (reverse (cons dir code))))
              (begin (loop (car t) #\1 (cons dir code))
                     (loop (cdr t) #\0 (cons dir code)))))))
    (f (car t) #\1)
    (f (cdr t) #\0)))

(define eg
  "Chez Scheme is both a general-purpose programming language and an implementation of
that language, with supporting tools and documentation. As a superset of the language
described in the Revised6 Report on Scheme (R6RS), Chez Scheme supports all stan-
dard features of Scheme, including first-class procedures, proper treatment of tail calls,
continuations, user-defined records, libraries, exceptions, and hygienic macro expansion.
Chez Scheme supports numerous non-R6RS features. A few of these are local and top-level
modules, local import, foreign datatypes and procedures, nonblocking I/O, an interactive
top-level, compile-time values and properties, pretty-printing, and formatted output.")

(define d (huffman-gen eg))
(huffman-print d)
(printf "Chez Scheme:~n~a~n" (huffman-encode d "Chez Scheme"))
(printf "decode: ~a~n" (huffman-decode d (huffman-encode d "Chez Scheme")))

