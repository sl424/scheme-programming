#!/usr/bin/gsi-script

(load "chp3-data_abst_num")

(define subst
  (lambda (item key ls)
    (cond
      ((null? ls) '())
      ((eqv? key (car ls)) (cons item (subst item key (cdr ls))))
      (else (cons (car ls) (subst item key (cdr ls)))))))

(define subst-all
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((equal? old (car ls))
       (cons new (subst-all new old (cdr ls))))
      ((pair? (car ls))
       (cons (subst-all new old (car ls)) (subst-all new old (cdr ls))))
      (else (cons (car ls) (subst-all new old (cdr ls)))))))

(define count-bg-all
  (lambda (item ls)
    (cond
      ((null? ls) 0)
      ((not (pair? (car ls)))
       (if (not (eqv? item (car ls)))
         (+ 1 (count-bg-all item (cdr ls)))
         (+ 0 (count-bg-all item (cdr ls)))))
      (else (+ (count-bg-all item (car ls)) (count-bg-all item (cdr ls)))))))

(define hsum
  (lambda (n sum)
    (if (zero? n)
      sum
      (hsum (sub1 n) (+ sum (/ 1 n))))))

(define fib
  (lambda (n)
    (writeln "n = " n)
    (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

(define occurs
  (lambda (key ls)
    (cond
      ((null? ls) 0)
      ((eq? key (car ls)) (+ 1 (occurs key (cdr ls))))
      (else (+ 0 (occurs key (cdr ls)))))))

(define occurs-it
  (lambda (key ls acc)
    (cond
      ((null? ls) acc)
      ((eq? key (car ls)) (occurs-it key (cdr ls) (+ 1 acc)))
      (else (occurs-it key (cdr ls) (+ 0 acc))))))

(define remove
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) item) (remove item (cdr ls)))
      (else (cons (car ls) (remove item (cdr ls)))))))

(define main
  (lambda ()
    (display (occurs 'a '(a b a c a d)))(newline)
    (display (occurs-it 'a '(a b a c a d) 0))(newline)
    (display (fib 4))(newline)
    (display "hsum of n = 5") (newline)
    (display (hsum 5 0)) (newline)
    (display "hsum of n = 4") (newline)
    (display (hsum 4 0)) (newline)
    (display "logn") (newline)
    (display (log 5))(newline)
    (display "hello world") (newline)
    (display (count-bg-all 'a '((((b (((a)) c))))))) (newline)
    (display (subst-all 0 '(1) '(((1) (0))))) (newline)
    (display (subst 0 1 '())) (newline)
    ))
