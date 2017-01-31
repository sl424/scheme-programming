#!/usr/bin/gsi-script

(load "chp12")

(define unif-rand-var-0-1
  (let ((big 1000000))
    (lambda ()
      (/ (+ 1 (random big)) big))))

(define exponential-random-variable
  (lambda (mean)
    (* mean (- (log (unif-rand-var-0-1))))))

(define normal-random-variable
  (lambda (mean std-dev)
    (letrec ((compute
               (lambda (i)
                 (if (zero? i)
                   0
                   (+ (- (unif-rand-var-0-1) 0.5)
                      (compute (sub1 i)))))))
      (+ mean (* std-dev (compute 12))))))

(define gallons-generator
  (lambda ()
    (max 1 (round (normal-random-variable 12 4)))))

(define arrival-time-generator
  (lambda (av-arr-time)
    (+ 1 (round (exponential-random-variable (- av-arr-time 1))))))

(define odds-evens
  (let ((odd-counter (counter-maker 0 add1))
        (even-counter (counter-maker 0 add1)))
    (lambda (n)
      (letrec ((tossing 
                 (lambda (n-rem)
                   (if (not (zero? n-rem))
                     (begin
                       (if (or (and (> (unif-rand-var-0-1) 0.5)
                                    (< (unif-rand-var-0-1) 0.5))
                               (and (< (unif-rand-var-0-1) 0.5)
                                    (> (unif-rand-var-0-1) 0.5)))
                         (send even-counter 'update!)
                         (send odd-counter 'update!))
                       (tossing (sub1 n-rem)))))))
        (tossing n))
      (cons (cons 'odd (send odd-counter 'show))
            (cons 'even (send even-counter 'show))))))

(define car-license
  (let ((record (bucket-maker)))
    (lambda (n)
      (letrec ((tracking 
                 (lambda (n-watch)
                   (if (not (zero? n-watch))
                     (let ((tmp (round (* 100 (unif-rand-var-0-1)))))
                       (display tmp) (display " ")
                       (send record 'update! tmp add1 (lambda (s) 1))
                       (tracking (sub1 n-watch))))))
               (listing (lambda (s)
                          (let ((count (send record 'lookup s (lambda (v) v)
                                            (lambda () 0))))
                            (if (< s 0)
                              '()
                              (if (>=  count 2)
                                (cons (cons s count) (listing (sub1 s)))
                                (listing (sub1 s))))))))
        (begin 
          (tracking n)
          (listing 100))))))

(define estimate-pi
  (lambda (N)
    (let ((counter (counter-maker 0 add1)))
      (letrec ((throw (lambda (N-rem)
                        (if (not (zero? N-rem))
                        (let ((x (- 1 (/ 
                                        (* 10 (unif-rand-var-0-1)) 5)))
                              (y (- 1 (/ 
                                        (* 10 (unif-rand-var-0-1)) 5))))
                          (if (> 1 (+ (expt x 2) (expt y 2)))
                            (begin
                              (send counter 'update!)
                              (throw (sub1 N-rem)))
                            (throw (sub1 N-rem))))))))
        (throw N)
        (* 4 (/ (send counter 'show) N))))))

(define main
  (lambda ()
    (display (exact->inexact (estimate-pi 10000)))(newline)
    (display (car-license 20))(newline)
    (display (odds-evens 1000))(newline)
    ))
