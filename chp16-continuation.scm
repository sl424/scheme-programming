#!/usr/bin/gsi-script

(load "chp15-stream")
;;; one line comment

#| 
multiline comment
|#

(define receiver-1
  (lambda (proc)
    (proc (list 1))))

(define receiver-2
  (lambda (proc)
    (proc (list (proc (list 2))))))

(define receiver-3
  (lambda (proc)
    (proc (list (proc (list 3 proc))))))


(define result "any value")
(define resultcc "any value")

(define writeln/return
  (lambda (x)
    (writeln x)
    x))
(define answer-maker
  (lambda (x)
    (cons 'answer-is (writeln/return x))))
(define call
  (lambda (receiver) 
    (receiver writeln/return)))

(define test-e (let ((r (lambda (continuation)
                        (if (zero? (random 2))
                          (+ 1000 6)
                          (continuation 6)))))
               (+ (* (+ (call/cc r) 3) 8)
                  (* (+ (call/cc r) 3) 8))))

(define deep "any continuation")

(define map-sub1
  (lambda (ls)
    (if (null? ls)
      (let ((receiver (lambda (k)
                        (set! deep k)
                        '())))
        (call/cc receiver))
      (cons (sub1 (car ls)) (map-sub1 (cdr ls))))))

(define *escape/thunk* "any continuation")

(define escaper
  (lambda (proc)
    (lambda (x)
      (*escape/thunk* 
        (lambda () 
          (proc x))))))

(define receiver-4
  (lambda (continuation)
    (set! *escape/thunk* continuation)
    (*escape/thunk* (lambda () (writeln "escaper is defined")))))
((call/cc receiver-4))

(define *reset/thunk* "continuation")

(define reset
    (lambda ()
      (*reset/thunk* (lambda () (writeln "reset invoked")))))

(define reset-receiver
  (lambda (continuation)
    (set! *reset/thunk* continuation)
    (*reset/thunk* (lambda () (writeln "reset is defined")))))

(define new-escaper "proc")
(let ((receiver (lambda (continuation)
                     (set! new-escaper 
                       (lambda (proc)
                         (lambda args
                           (continuation
                             (lambda ()
                               (apply proc args))))))
                     (lambda () (writeln "new-escaper is defined")))))
  ((call/cc receiver)))

(define escaper2
  (lambda (proc)
    (lambda (x)
      (new-escaper (lambda () (proc x))))))

(define *-and-count-maker
  (lambda ()
    (let ((local-counter 0))
      (lambda (n1 n2)
        (set! local-counter (+ local-counter 1))
        (writeln "number of multiplicaiton = " local-counter)
        (* n1 n2)))))

(define product+
  (lambda (n nums *-proc)
    (let ((receiver
            (lambda (exit-on-zero)
              (letrec
                ((product (lambda (nums)
                            (cond
                              ((null? nums) 1)
                              ((zero? (car nums)) (exit-on-zero 0))
                              (else (*-proc (car nums)
                                            (product (cdr nums))))))))
                (+ n (product nums))))))
      (call/cc receiver))))


(define main
  (lambda ()
    (let ((counter (*-and-count-maker)))
      (product+ 5 '(2 3 0 8) counter))
    (display (set! result (answer-maker (call receiver-1))))
    (display result)
    (newline)
    (display (set! resultcc (answer-maker (call/cc receiver-1))))
    (display resultcc)
    (newline)
    (display (set! result (answer-maker (call receiver-2))))
    (display result)
    (newline)
    (display (set! resultcc (answer-maker (call/cc receiver-2))))
    (display resultcc)
    (newline)
    (display (set! result (answer-maker (call receiver-3))))
    (display result)
    (newline)
    (display (let ((r (lambda (continuation) (+ 1000 6)))) (* (+ (call/cc r) 3) 8)))
    (display test-e)(newline)
    ))
