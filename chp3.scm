#!/usr/bin/gsi-script 

(load "chp2")

(define add1
  (lambda (int)
    (+ int 1)))

(define sub1
  (lambda (int)
    (- int 1)))

(define harmonic-sum
  (lambda (n)
    (cond
      ((zero? n) 0)
      (else (+ (/ 1 n) (harmonic-sum (sub1 n)))))))

(define loz
  (lambda (n)
    (if (zero? n)
      '()
      (cons '0 (loz (sub1 n))))))

(define =length=
  (lambda (ls)
    (if (null? ls)
      0
      (add1 (=length= (cdr ls))))))

(define reset
  (lambda ()
    ((escaper
       (lambda ()
         (writeln "reset invoked"))))))

(define error
  (lambda args
    (for-each (lambda (value) (display " ") (display value)) args)
    (newline)
    ))

(define =list-ref=
  (lambda (ls n)
    (cond
      ((<= (length ls) n) (error "out of range"))
      (else (list-ref-helper ls n)))))

(define list-ref-helper
  (lambda (ls n)
    (cond
      ((zero? n) (car ls))
      (else (list-ref-helper (cdr ls) (sub1 n))))))

(define sum
  (lambda (ls)
    (cond
      ((null? ls) 0)
      (else (+ (car ls) (sum (cdr ls)))))))

(define mult-by-n
  (lambda (num tuple)
    (cond
      ((null? tuple) '())
      (else (cons (* num (car tuple)) (mult-by-n num (cdr tuple)))))))

(define index
  (lambda (key ls)
    (index-helper key ls 0)))

(define index-helper
  (lambda (key ls i)
    (cond
      ((null? ls) -1)
      ((eqv? key (car ls)) i)
      (else (index-helper key (cdr ls) (add1 i))))))

(define make-list
  (lambda (num item)
    (cond
      ((zero? num) '())
      (else (cons item (make-list (sub1 num) item))))))

(define sum-of-odds
  (lambda (n)
    (soo-helper 1 n)))

(define soo-helper
  (lambda (i n)
    (if (zero? n)
      0
      (+ i (soo-helper (+ 2 i) (sub1 n))))))

(define tuple-2-int
  (lambda (tuple)
    (if (null? tuple)
      (error " bad argument ")
      (t2i-helper tuple ))))

(define t2i-helper
  (lambda (tuple)
    (if (null? tuple) 
      0
      (+ (* (car tuple) (expt 10 (length (cdr tuple))))
         (t2i-helper (cdr tuple))))))

(define make-ratl 
  (lambda (a b)
      (define common (gcd a b))
      (cons (/ a common) (cons (/ b common) '())) ))

(define main
  (lambda ()
    (display (make-ratl -10 15)) (newline)
    (display (tuple-2-int '(2 2 2))) (newline)
    (display (sum-of-odds 4)) (newline)
    (display (make-list 5 'no)) (newline)
    (display (length (make-list 0 'any))) (newline)
    (display (index 3 '(1 2 3 4 5 6))) (newline)
    (display (mult-by-n 3 '(1 2 3 4 5))) (newline)
    (display (sum '(1 2 3 4 5))) (newline)
    (display (=list-ref= '(a b c d e f) 3)) (newline)
    (display (sub1 7)) (newline)
    (display (harmonic-sum 5)) (newline)
    (display (loz 4)) (newline)
    (display (=length= '(a b c)))
    ))
