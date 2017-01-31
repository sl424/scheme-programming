#!/usr/bin/gsi-script

(load "chp11")

(define for-effect
  (lambda (item-ignored)
    "unspecified value"))

(define color-code
  (lambda (color)
    (case color
      ((red) 1)
      ((blue) 2)
      ((green) 3)
      (else 0))))

(define box-maker
  (lambda (init-value)
    (let ((contents init-value))
      (lambda msg
        (case (1st msg)
          ((type) "box")
          ((show) contents)
          ((update!) (for-effect (set! contents (2nd msg))))
          ((swap!) (let ((ans contents))
                     (set! contents (2nd msg))
                     ans))
          ((reset!) (for-effect (set! contents init-value)))
          (else (delegate base-object msg)))))))

(define delegate
  (lambda (obj msg)
    (apply obj msg)))

(define base-object
  (lambda msg
    (case (1st msg)
      ((type) "base-object")
      (else invalid-method))))

(define invalid-method "unknown")

(define send
  (lambda args
    (let ((object (car args)) (msg (cdr args)))
      (let ((try (apply object msg)))
        (if (eq? invalid-method try)
          (error "Bad method name: " (car msg) " sent to object of " 
                 (object 'type) " type.")
          try)))))

(define counter-maker
  (lambda (init-value unary-proc)
    (let ((total (box-maker init-value)))
      (lambda msg
        (case (1st msg)
          ((type) "counter")
          ((update!) (send total 'update! (unary-proc (send total 'show))))
          ((show reset) (delegate total msg))
          (else (delegate base-object msg)))))))

(define count (counter-maker 10 (lambda (x) (add1 x))))
    (define box-a (box-maker 5))

(define accumulator-maker
  (lambda (init-value proc)
    (let ((total (box-maker init-value)))
      (lambda msg
        (case (1st msg)
          ((type) "accumulator")
          ((update!) (send total 'update! (proc (send total 'show) (2nd msg))))
          ((swap!) (delegate base-object msg))
          (else (delegate total msg)))))))

(define restrict-accumulator-maker
  (lambda (init-value proc pred)
    (let ((total (box-maker init-value)))
      (lambda msg
        (case (1st msg)
          ((type) "accumulator")
          ((update!) (let ((ans (proc (send total 'show) (2nd msg))))
                       (if (pred ans)
                         (send total 'reset!)
                         (send total 'update! ans))))
          ((swap!) (delegate base-object msg))
          (else (delegate total msg)))))))

(define acc (accumulator-maker 100 -))

(define gauge-maker
  (lambda (init proc-up proc-down)
    (let ((total (box-maker init)))
      (lambda msg
        (case (1st msg)
          ((type) "gauge")
          ((up!) (send total 'update! (proc-up (send total 'show))))
          ((down!) (send total 'update! (proc-down (send total 'show))))
          ((show) (send total 'show))
          ((reset!) (send total msg))
          (else (send base-object msg)))))))

(define g (gauge-maker 10 add1 sub1))

(define acc-max (accumulator-maker 0 max))

(define test-acc
  (lambda (obj args)
    (if (not (null? args))
      (begin
        (send obj 'update! (car args))
        (display (send obj 'show))
        (display " ")
        (test-acc obj (cdr args))))))

(define hour (restrict-accumulator-maker 0 + (lambda (n) (>= n 12))))

(define modified-restrict-accumulator-maker
  (lambda (init-value proc pred reset-proc)
    (let ((total (box-maker init-value)))
      (lambda msg
        (case (1st msg)
          ((type) "accumulator")
          ((update!) (let ((ans (proc (send total 'show) (2nd msg))))
                       (if (pred ans)
                         (reset-proc total ans)
                         (send total 'update! ans))))
          ((swap!) (delegate base-object msg))
          (else (delegate total msg)))))))

(define clock-maker
  (lambda (init)
    (letrec ((hour (accumulator-maker (cadr init) +))
          (minute (modified-restrict-accumulator-maker 
                    (car init) + (lambda (n) (>= n 60))
                    (lambda (obj ans)
                      (send obj 'reset!)
                      (send obj 'update! (remainder ans 60))
                      (send hour 'update! 1)))))
      (lambda msg
        (case (1st msg)
          ((type) "clock")
          ((show) (list (send hour 'show) ":" (send minute 'show)))
          ((update!) (let ((h (car (2nd msg))) (m (cadr (2nd msg))))
                    (send minute 'update! m)
                    (send hour 'update! h)))
          (else (send base-object msg)))))))

(define clock (clock-maker '(0 0)))

(define stack-maker
  (lambda ()
    (let ((stk '()))
      (lambda msg
        (case (1st msg)
          ((type) "stack")
          ((empty?) (null? stk))
          ((push!) (for-effect (set! stk (cons (2nd msg) stk))))
          ((top) (if (null? stk)
                   (error "top: the stack is empty")
                   (car stk)))
          ((pop!) (for-effect
                       (if (null? stk)
                         (error "pop! stack is empty")
                         (set! stk (cdr stk)))))
          ((size) (length stk))
          ((print) (display "top: ")
                   (for-each (lambda (x) (display x) (display " ")) stk) 
                   (newline))
          (else (delegate base-object msg)))))))

(define paren-check?
  (let ((open-stack (stack-maker))
        (close-stack (stack-maker)))
    (lambda (ls-all)
      (letrec
        ((helper 
          (lambda (ch)
            (if (or (equal? "(" ch) (equal? "[" ch) (equal? "{" ch))
              (send open-stack 'push! ch)
              (if (or (equal? ")" ch) (equal? "]" ch) (equal? "}" ch))
                (if (or (and (equal? ")" ch) 
                            (equal? "(" (send open-stack 'top)))
                       (and (equal? "]" ch) 
                            (equal? "[" (send open-stack 'top)))
                       (and (equal? "}" ch) 
                            (equal? "{" (send open-stack 'top))))
                (send open-stack 'pop!)
                (send close-stack 'push! ch))
                )))))
        (for-each helper ls-all)
        (if (and (send open-stack 'empty?)
                 (send close-stack 'empty?))
          #t
          #f)))))

(define queue-maker
  (lambda ()
    (let ((q (cons '() '())))
      (let ((rear q))
        (lambda msg
          (case (1st msg)
            ((type) "queue")
            ((empty?) (eq? rear q))
            ((enqueue!) (for-effect
                          (let ((list-of-item (cons (2nd msg) '())))
                            (set-cdr! rear list-of-item)
                            (set! rear list-of-item))))
            ((enqueue-list!) (for-effect
                               (let* ((new-list (2nd msg))
                                      (new-last (last-pair new-list)))
                                 (set-cdr! new-last '())
                                 (set-cdr! rear new-list)
                                 (set! rear new-last))))
            ((enqueue-many!) (for-effect
                               (let* ((new-list (cdr msg))
                                      (new-last (last-pair new-list)))
                                 (set-cdr! new-last '())
                                 (set-cdr! rear new-list)
                                 (set! rear new-last))))

            ((front) (if (eq? rear q)
                       (error "front: the queue is empty.")
                       (car (cdr q))))
            ((dequeue!) (for-effect
                          (if (eq? rear q)
                            (error "dequeue!: the queue is empty.")
                            (let ((front-cell (cdr q)))
                              (set-cdr! q (cdr front-cell))
                              (if (eq? front-cell rear)
                                (set! rear q))))))
            ((size) (length (cdr q)))
            ((print) (display "FRONT: ")
                     (for-each (lambda (x)
                                 (display x)
                                 (display " ")) (cdr q)) 
                     (newline))
            (else (delegate base-object msg))))))))

(define q (queue-maker))

(define queue-list
  (lambda (q)
    (let ((marker (list '()))
          (outlist '()))
      (send q 'enqueue! marker)
      (letrec ((helper
                (lambda (item)
                  ;(display outlist)
                  ;(display item)
                  (if (eq? item marker)
                    (begin
                      (send q 'dequeue!)
                      outlist)
                    (begin
                      (set! outlist (cons item outlist))
                      (send q 'enqueue! item)
                      (send q 'dequeue!)
                      (helper (send q 'front)))))))
        (helper (send q 'front))))))

(define circular-list-maker
  (lambda ()
    (let ((marker '())
          (size-gauge (gauge-maker 0 add1 sub1)))
      (lambda msg
        (case (1st msg)
          ((type) "circular-list")
          ((empty?) (null? marker))
          ((insert!) (send size-gauge 'up!)
                     (for-effect
                       (if (null? marker)
                         (begin
                           (set! marker (cons (2nd msg) '()))
                           (set-cdr! marker marker))
                         (set-cdr! marker (cons (2nd msg) (cdr marker))))))
          ((head) (if (null? marker)
                    (error "head: the list is empty")
                    (car (cdr marker))))
          ((delete!) (for-effect
                       (if (null? marker)
                         (error "delete!: the list is empty")
                         (begin
                           (send size-gauge 'down!)
                           (if (eq? marker (cdr marker))
                             (set! marker '())
                             (set-cdr! marker (cdr (cdr marker))))))))
          ((move!) (for-effect
                     (if (null? marker)
                       (error "move!: the list is empty")
                       (set! marker (cdr marker)))))
          ((size) (send size-gauge 'show))
          ((print) (if (not (null? marker))
                     (let ((next (cdr marker)))
                       (set-cdr! marker '())
                       (for-each (lambda (x) (display x) (display " ")) next)
                       (set-cdr! marker next)))
                   (newline))
          ((reverse!) (for-effect
                        (begin
                        (if (> (send size-gauge 'show) 2)
                          (let ((cur (cdr marker))
                                (next (cdr (cdr marker))))
                            (letrec ((helper (lambda ()
                                              (if (not (eq? marker next))
                                                (begin
                                                  (set-cdr! cur marker)
                                                  (set! marker cur)
                                                  (set! cur next)
                                                  (set! next (cdr next))
                                                  (helper))
                                                (begin 
                                                  (set! marker cur))))))
                              (helper))))
                        (if (= (send size-gauge 'show) 2)
                          (set! marker (cdr marker))))))
          (else (delegate base-object msg)))))))

(define clist (circular-list-maker))

(define bucket-maker
  (lambda ()
    (let ((table '()))
      (lambda msg
        (case (1st msg)
          ((type) "bucket")
          ((lookup) (let ((key (2nd msg))
                          (succ (3rd msg))
                          (fail (4th msg)))
                      (lookup key table (lambda (pr) (succ (cdr pr))) fail)))
          ((update!) (for-effect
                       (let ((key (2nd msg))
                             (updater (3rd msg))
                             (initializer (4th msg)))
                         (lookup key 
                                 table 
                                 (lambda (pr) (set-cdr! pr (updater (cdr pr))))
                                 (lambda ()
                                   (let ((pr (cons key (initializer key))))
                                     (set! table (cons pr table))))))))
          ((remove!) (for-effect
                       (let ((key (2nd msg)))
                         (lookup key 
                                 table 
                                 (lambda (pr) (set-car! pr '()))
                                 (lambda () #f)))))
          (else (delegate base-object msg)))))))

(define memoize-bucket
  (lambda (proc)
    (let ((bucket (bucket-maker)))
      (lambda (arg)
        (send bucket 'update!-lookup arg (lambda (val) val) proc)))))

(define word-frequency
  (lambda (string-list)
    (let ((b (bucket-maker)))
      (for-each 
        (lambda (s) (send b 'update! s add1 (lambda (s) 1))) string-list)
      b)))

(define string-list (list "there" "is" "an" "outrageous" "attack" "the"))
(define wf-bucket (word-frequency string-list))

(define wf-bucket-result (map (lambda (s)
                         (cons s (send wf-bucket 'lookup s (lambda (v) v)
                                       (lambda () 0)))) '("a" "an" "the")))
(define b (bucket-maker))

(define hash-table-maker
  (lambda (size hash-fn)
    (let ((v ((vector-generator (lambda (i) (bucket-maker))) size)))
      (lambda msg
        (case (1st msg)
          ((type) "hash-table")
          (else (delegate (vector-ref v (hash-fn (2nd msg))) msg)))))))

(define large-prime (- (expt 2 31) 1))

(define memoize-bucket-table
  (let ((hashf (lambda (x) (remainder x 1000))))
    (let ((h (hash-table-maker 1000 hashf)))
      (lambda (proc)
        (lambda (arg)
          (send bucket 'update!-lookup arg (lambda (val) val) proc))))))

(define word-frequency-hash-table
  (let ((hf (lambda (s) (remainder (char->integer (string-ref s 0)) 26))))
    (let ((h (hash-table-maker 26 hf)))
      (lambda (string-list)
        (for-each (lambda (s) (send h 'update! s add1 (lambda (s) 1))) 
                  string-list)
        h))))
(define wf-hash (word-frequency-hash-table string-list))

(define wf-hash-result (map (lambda (s)
                         (cons s (send wf-hash 'lookup s (lambda (v) v)
                                       (lambda () 0)))) '("a" "an" "the")))

(define combine
  (lambda (f g)
    (lambda msg
      (let ((f-try (delegate f msg)))
        (if (eq? invalid-method f-try)
          (delegate g msg)
          f-try)))))

(define main
  (lambda ()
    (display wf-hash-result)(newline)
    (display wf-bucket-result)(newline)
    (newline)
    (display (send b 'lookup 'a (lambda (x) x) (lambda () 'no)))(newline)
    (display (send b 'update! 'a (lambda (x) (add1 x)) (lambda (x) 0)))(newline)
    (display (send b 'update! 'a (lambda (x) (add1 x)) (lambda (x) 0)))(newline)
    (display (send b 'lookup 'a (lambda (x) x) (lambda () 'no)))(newline)
    (display (begin 
               (send b 'remove! 'a)
               (send b 'lookup 'a (lambda (v) #t) (lambda () #f))))(newline)

    (newline)
    ;(send clist 'insert! 'c)
    (send clist 'insert! 'b)
    (send clist 'insert! 'a)
    (send clist 'print)
    (send clist 'reverse!)
    (send clist 'print)
    (send q 'enqueue-many! 1 2 3)
    (send q 'enqueue-list! '(4 5 6))
    (display (queue-list q))(newline)
    (send q 'print)
    (display (paren-check? 
               (list 13 + 5 * "(" "[" 14 - 3 * "(" 12 - 7 ")" "]" - 15 "}")))
    (newline)
    (test-acc clock (list '(0 10) '(0 20) '(1 0) '(0 40)) )(newline)
    (test-acc hour '(3 7 2 4 10 1 5))(newline)
    (display (send acc 'show))(newline)
    (send acc 'update! 10)
    (display (send acc 'show))(newline)
    (display (send count 'show))(newline)
    (send count 'update!)
    (display (send count 'show))(newline)
    (send box-a 'update! 4)
    (display (send box-a 'show))(newline)
    (display (color-code 'red))(newline)
    ))
