#!/usr/bin/gsi-script

(load "chp4")

(define fib
  (lambda (n)
    (letrec ((fib-it
               (lambda (n s1 s2)
                 (if (= n 1)
                   s2
                   (fib-it (sub1 n) s2 (+ s1 s2))))))
      (fib-it n 0 1))))

(define the-zero-poly '((0 0)))

(define degree
  (lambda (poly)
    (caar poly)))

(define leading-coef
  (lambda (poly)
    (cadar poly)))

(define rest-of-poly
  (lambda (poly)
    (if (null? (cdr poly))
      the-zero-poly
      (cdr poly))))

(define poly-cons
  (lambda (deg coef poly)
    (let ((deg-p (degree poly)))
      (cond
        ((and (zero? deg) (equal? poly the-zero-poly))
         (list (list 0 coef)))
        ((>= deg-p deg)
         (error "poly-cons: Degree too high in" poly))
        ((zero? coef) poly)
        (else (cons (list deg coef) poly))))))

(define make-term
  (lambda (deg coef)
    (poly-cons deg coef the-zero-poly)))

(define leading-term
  (lambda (poly)
    (make-term (degree poly) (leading-coef poly))))

(define zero-poly?
  (lambda (poly)
    (and (zero? (degree poly)) (zero? (leading-coef poly)))))

(define p+
  (lambda (poly1 poly2)
    (cond
      ((zero-poly? poly1) poly2)
      ((zero-poly? poly2) poly1)
      (else 
        (let ((n1 (degree poly1))
              (n2 (degree poly2)))
          (cond
            ((> n1 n2) 
             (let ((a1 (leading-coef poly1)) 
                   (rest1 (rest-of-poly poly1))) 
               (poly-cons n1 a1 (p+ rest1 poly2))))
            ((< n1 n2) 
             (let ((a2 (leading-coef poly2)) 
                    (rest2 (rest-of-poly poly2)))
               (poly-cons n2 a2 (p+ poly1 rest2))))
            (else 
              (let (
                (a1 (leading-coef poly1))
                (a2 (leading-coef poly2))
                (rest1 (rest-of-poly poly1))
                (rest2 (rest-of-poly poly2)))
              (poly-cons n1 (+ a1 a2) (p+ rest1 rest2))))))))))

(define p*
  (letrec
    ((t* (lambda (trm poly)
           (if (zero-poly? poly)
             the-zero-poly
             (poly-cons 
               (+ (degree trm) (degree poly))
               (* (leading-coef trm) (leading-coef poly))
               (t* trm (rest-of-poly poly)))))))
    (lambda (poly1 poly2)
      (letrec
        ((p*-helper (lambda (p1)
                      (if (zero-poly? p1)
                        the-zero-poly
                        (p+ (t* (leading-term p1) poly2)
                            (p*-helper (rest-of-poly p1)))))))
        (p*-helper poly1)))))

(define negative-poly
  (lambda (poly)
    (let ((poly-negative-one (make-term 0 -1)))
      (p* poly-negative-one poly))))

(define p-
  (lambda (poly1 poly2)
    (p+ poly1 (negative-poly poly2))))

(define p/
  (letrec ((p/-helper
    (lambda (poly1 poly2 result)
      (let ((n1 (degree poly1))
            (n2 (degree poly2))
            (a1 (leading-coef poly1))
            (a2 (leading-coef poly2)))
  ;poly 1 has higher degree or higher coefficient with equal degree
        (cond 
          ((>= n1 n2)
           (let ((term (make-term (- n1 n2) (/ a1 a2))))
            ;new numerator for the next recursive call
            (p/-helper (p- poly1 (p* term poly2)) poly2 (p+ term result))))
            ;(p+ term result)
          ;else return result
          (else result))))))
    (lambda (poly1 poly2)
      (p/-helper poly1 poly2 the-zero-poly))))


(define main
  (lambda ()
    (display (fib 6)) (newline)
    (let ((poly1 '((2 1) (1 -3) (0 -10)))
          (poly2 '((1 1) (0 2)))
          (q '((0 0))))
      (display (p/ poly1 poly2)) (newline))
    ))
