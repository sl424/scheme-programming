#!/usr/bin/gsi-script

(load "chp14-macro")

(define-macro (delayed-list-cons expr del-list)
              `(cons ,expr (delay ,del-list)))

(define the-null-delayed-list '())
(define delayed-list-null? null?)
(define delayed-list-car car)
(define delayed-list-cdr (compose force cdr))

(define random-delayed-list
  (lambda (n)
    (if (zero? n)
      the-null-delayed-list
      (delayed-list-cons
        (+ 2 (random 11))
        (random-delayed-list (sub1 n))))))

(define list->delayed-list
  (letrec ((helper 
             (lambda (ls)
                (if (null? ls)
                  the-null-delayed-list
                  (delayed-list-cons
                    (car ls)
                    (helper (cdr ls)))))))
    (lambda (lst)
      (helper lst))))

(define dl (list->delayed-list '(1 2 3 4 5)))

(define sum-until-first-7
  (letrec
    ((local-sum
       (lambda (delayed-list sum count)
         (if (delayed-list-null? delayed-list)
           (writeln "A seven was not found: sum= " sum " and count = " count)
           (let ((next (delayed-list-car delayed-list)))
             (if (= next 7)
                (writeln "A seven was found: sum= " sum " and count = " count)
                (local-sum 
                  (delayed-list-cdr delayed-list)
                  (+ next sum)
                  (add1 count))))))))
    (lambda (rand-delayed-list)
      (local-sum rand-delayed-list 0 0))))

(define delayed-list->list
  (lambda (delayed-list)
    (if (delayed-list-null? delayed-list)
      '()
      (cons (delayed-list-car delayed-list)
            (delayed-list->list (delayed-list-cdr delayed-list))))))

(define stream-car car)
(define stream-cdr (compose force cdr))

(define-macro (stream-cons expr stream)
              `(cons ,expr (delay ,stream)))

(define random-stream-generator
  (lambda ()
    (stream-cons (+ 2 (random 11)) (random-stream-generator))))

(define random-stream (random-stream-generator))

(define the-end-of-stream-tag "end of stream")

(define the-null-stream
  (stream-cons the-end-of-stream-tag the-null-stream))

(define list->stream
  (lambda (ls)
    (if (null? ls)
      the-null-stream
      (stream-cons (car ls) (list->stream (cdr ls))))))
(define end-of-stream?
  (lambda (x)
    (eq? x the-end-of-stream-tag)))
(define stream-null? (compose end-of-stream? stream-car))

(define stream->list
  (lambda (strm n)
    (if (or (zero? n) (stream-null? strm))
      '()
      (cons (stream-car strm)
            (stream->list (stream-cdr strm) (sub1 n))))))

(define finite-stream->list
  (lambda (finite-stream)
    (stream->list finite-stream -1)))

(define build-stream
  (lambda (seed proc)
    (letrec
      ((stream-builder
         (lambda (x)
           (stream-cons x (stream-builder (proc x))))))
      (stream-builder seed))))

(define positive-integers
  (build-stream 1 add1))

(define even-positive-integers
  (build-stream 2 (lambda (x) (+ x 2))))

(define powers-of-2
  (build-stream 1 (lambda (x) (* x 2))))

(define random-stream-generator
  (lambda ()
    (build-stream (+ 2 (random 11)) (lambda (x) (+ 2 (random 11))))))

(define factorials
  (letrec
    ((stream-builder
       (lambda (x n)
         (stream-cons x (stream-builder (* x n) (add1 n))))))
    (stream-builder 1 1)))

(define stream-map
  (lambda (proc strm)
    (if (stream-null? strm)
      the-null-stream
      (stream-cons
        (proc (stream-car strm))
        (stream-map proc (stream-cdr strm))))))

(define odd-positive-integers
  (stream-map sub1 even-positive-integers))

(define stream-apply-to-both
  (lambda (proc)
    (letrec
      ((str-app
         (lambda (s1 s2)
           (stream-cons
             (proc (stream-car s1) (stream-car s2))
             (str-app (stream-cdr s1) (stream-cdr s2))))))
      str-app)))

(define stream-plus (stream-apply-to-both +))
(define stream-times (stream-apply-to-both *))

(define stream-filter-out
  (lambda (test?)
    (letrec
      ((helper
         (lambda (strm)
           (let ((a (stream-car strm)))
             (if (test? a)
               (helper (stream-cdr strm))
               (stream-cons a (helper (stream-cdr strm))))))))
      helper)))

(define factorials
  (stream-cons 1 (stream-times factorials positive-integers)))

(define divides-by
  (lambda (n)
    (lambda (k)
      (zero? (remainder k n)))))

(define sieve (compose stream-filter-out divides-by))

(define prime-numbers
  (letrec
    ((primes
       (lambda (s)
         (stream-cons
           (stream-car s)
           (primes ((sieve (stream-car s)) (stream-cdr s)))))))
    (primes (stream-cdr positive-integers))))

(define odd-primes-builder
  (lambda (n)
    (if (has-prime-divisor? n)
      (odd-primes-builder (+ n 2))
      (stream-cons n (odd-primes-builder (+ n 2))))))

(define prime-numbers (stream-cons 2 odd-primes))

(define odd-primes (stream-cons 3 (odd-primes-builder 5)))

(define has-prime-divisor?
  (lambda (n)
    (let ((max-value (sqrt n)))
      (letrec 
        ((try (lambda (primes)
                (and (<= (stream-car primes) max-value)
                     (or (zero? (remainder n (stream-car primes)))
                         (try (stream-cdr primes)))))))
        (try odd-primes)))))

(define integers-from
  (lambda (n)
    (build-stream n add1)))


(define multiples-of
  (lambda (n)
    (stream-map (lambda (x) (* x n)) (stream-cons 0 positive-integers))))


(define squares-of-integers 
  (stream-map (lambda (x) (expt x 2)) positive-integers))

(define stream-ref
  (lambda (strm n)
    (letrec
      ((helper
         (lambda (strm n)
           (if (zero? n)
             (stream-car strm)
             (helper (stream-cdr strm) (sub1 n))))))
      (if (< n 0)
        (error "positive value only")
        (helper strm n)))))

(define stream-member?
  (lambda (a strm n)
    (letrec
      ((helper
         (lambda (a strm n)
           (if (zero? n)
             #f
             (if (= a (stream-car strm))
               #t 
               (helper a (stream-cdr strm) (sub1 n)))))))
      (if (< n 0)
        (error "positive value only")
        (helper a strm n)))))

(define prime?
  (lambda (n)
    (if (has-prime-divisor? n)
      #f
      #t)))

(define positive-rationals
  (lambda (strm) 
    (if (stream-null? strm)
      the-null-stream
      (stream-cons 
        (list 1 (stream-car strm))
        (stream-cons
          (list (stream-car strm) 1)
          (positive-rationals (stream-cdr strm)))))))

(define pr (stream-cdr (positive-rationals positive-integers)))

(define diagonal
  (lambda (i)
    (letrec 
      ((helper
         (lambda (a b)
           (if (zero? a)
             the-null-stream
             (stream-cons 
               (list a b)
               (helper (sub1 a) (add1 b)))))))
      (helper i 1))))

(define stream-append/delay
  (lambda (finite-stream stream)
    (cond
      ((stream-null? finite-stream) stream)
      (else (stream-cons
              (stream-car finite-stream)
              (stream-append/delay (stream-cdr finite-stream) stream))))))

(define-macro (stream-append finite-stream stream)
              `(stream-append/delay ,finite-stream (delay ,stream)))

(define int-pairs-generator
  (lambda (i)
    (stream-append (diagonal i) (int-pairs-generator (add1 i)))))

(define int-pairs (int-pairs-generator 1))
(define first-10-int-pairs (stream->list int-pairs 10))

(define flipflop
  (lambda (str)
    (let ((str-list (string->list str)))
      (let ((new-list 
              (map (lambda (c)
                   (if (char-upper-case? c) 
                     (char-downcase c)
                     (char-upcase c))) str-list)))
        (list->string new-list)))))

(define hash-function
  (lambda (m)
    (lambda (strn)
      (let ((cl (string->list strn)))
        (modulo (reduce + (map char->integer cl)) m)))))

(define port-in (open-input-file "input1.dat"))

(define output-test
  (lambda ()
    (let ((port-out (open-output-file "output.dat")))
      (display "this is an output test" port-out)
      (newline port-out)
      (close-output-port port-out))))

(define file->stream
  (lambda (filename)
    (let ((port-in (open-input-file filename)))
      (letrec
        ((build-input-stream
           (lambda ()
             (let ((ch (read-char port-in)))
               (if (eof-object? ch)
                 (begin
                   (close-input-port port-in)
                   the-null-stream)
                 (stream-cons ch (build-input-stream)))))))
        (build-input-stream)))))

(define stream->file
  (lambda (filename stream)
    (let ((port-out (open-output-file filename)))
      (letrec ((write-stream
                 (lambda (str)
                   (if (not (stream-null? str))
                     (begin
                       (display (stream-car str) port-out)
                       (write-stream (stream-cdr str)))))))
        (write-stream stream)
        (close-output-port port-out)))))

(define file->stream
  (lambda (filename)
    (let ((port-in (open-input-file filename)))
      (letrec
        ((build-input-stream
           (lambda ()
             (let ((ch (read port-in)))
               (if (eof-object? ch)
                 (begin
                   (close-input-port port-in)
                   the-null-stream)
                 (stream-cons ch (build-input-stream)))))))
        (build-input-stream)))))

(define running-sum
  (lambda (input-file output-file)
    (letrec ((get-stream 
               (lambda (sum strm)
                 (let ((n (stream-car strm)))
                   (if (equal? n "end of stream")
                     the-null-stream
                     (let ((s (+ sum n)))
                     (stream-cons n (stream-cons #\tab 
                         (stream-cons s (stream-cons #\newline 
                            (get-stream s (stream-cdr strm))))))))))))
    (stream->file output-file 
                  (get-stream 0
                    (file->stream input-file))))))

(define apply-procedures
  (lambda (procedures)
    (if (null? procedures)
      (lambda (x) x)
      (compose
        (car procedures) 
        (apply-procedures (cdr procedures))))))

(define main
  (lambda ()
    (display ((apply compose-many (list add1 add1 add1)) 0))(newline)
    (running-sum "i-int.dat" "o-int.dat")
    (display ((hash-function 26) "Hello"))(newline)
    (display (flipflop "Hello World"))(newline)
    (display first-10-int-pairs)(newline)
    (display (finite-stream->list (diagonal 5)))(newline)
    (display (stream->list pr 4))(newline)
    (display (map prime? '(37 35 51 57 100000007)))(newline)
    (display (stream-member? 26 positive-integers 25))(newline)
    (display (stream-ref positive-integers 1010))(newline)
    (display (stream->list squares-of-integers 4))(newline)
    (display (stream->list (multiples-of 5) 10))(newline)
    (display (stream->list (integers-from 123) 10))(newline)
    (display (stream->list (odd-primes-builder 5) 10))(newline)
    (display (stream->list prime-numbers 10))(newline)
    (display (stream->list factorials 10))(newline)
    (display (delayed-list->list (random-delayed-list 20)))(newline)
    (display (delayed-list->list (random-delayed-list 20)))(newline)
    (display (delayed-list->list dl))(newline)
    (display (stream->list random-stream 20))(newline)
    (display (stream->list random-stream 25))(newline)
    (writeln "test1" "test2")
    ))
