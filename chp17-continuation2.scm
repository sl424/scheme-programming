#!/usr/bin/gsi-script

(load "chp16-continuation")

(define the-continuation "proc")

(define test
  (let ((i 0))
    (call/cc (lambda (k) (set! the-continuation k)))
    (set! i (+ i 1))
    i))

(define countdown
  (lambda (n)
    (writeln "this only appears once")
    (let ((pair (message "exit" (attempt (message "Enter" n)))))
      (let ((v (1st pair))
            (returner (2nd pair)))
        (writeln "     The non-negative-number: " v)
        (if (positive? v)
          (returner (list (sub1 v) returner))
          (writeln "blastoff"))))))

(define message
  (lambda (direction value)
    (writeln "    " direction "ing attempt with value: " value)
    value))

(define attempt
  (lambda (n)
    (let ((receiver (lambda (proc) (list n proc))))
      (call/cc receiver))))

(define get-back "any proc")

(define break
  (lambda (x)
    (let ((break-receiver
            (lambda (continuation)
              (set! get-back (lambda (x) (continuation x)))
              (any-action x))))
      (call/cc break-receiver))))

(define flatten-number-list
  (lambda (s)
    (cond
      ((null? s) '())
      ((number? s) (list (break s)))
      (else
        (let ((flatcar (flatten-number-list (car s))))
          (append flatcar (flatten-number-list (cdr s))))))))
(define any-action
  (lambda (x)
    ((new-escaper (lambda () x)))
    (get-back)))

(define coroutine-maker
  (lambda (proc)
    (let ((saved-continuation "any continuation"))
      (let ((update-continuation! 
              (lambda (v)
                (set! saved-continuation v))))
        (let ((resumer (resume-maker update-continuation!))
              (first-time #t))
          (lambda (value)
            (if first-time
              (begin
                (set! first-time #f)
                (proc resumer value))
              (saved-continuation value))))))))

(define resume-maker
  (lambda (update-proc!)
    (lambda (next-coroutine value)
      (let ((receiver (lambda (continuation)
                        (update-proc! continuation)
                        (next-coroutine value))))
        (call/cc receiver)))))

(define ping
  (let ((ping-proc (lambda (resume v)
                     (display "ping-")
                     (resume pong 'ignored-ping))))
    (coroutine-maker ping-proc)))

(define pong
  (let ((pong-proc (lambda (resume v)
                     (display "pong")
                     (newline)
                     (resume ping 'ignored-pong))))
    (coroutine-maker pong-proc)))
