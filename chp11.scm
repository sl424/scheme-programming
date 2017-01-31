#!/usr/bin/gsi-script

(load "chp10")

(define lookup
  (lambda (obj table success-proc failure-proc)
    (letrec ((lookup (lambda (table)
                       (if (null? table)
                         (failure-proc)
                         (let ((pr (car table)))
                           (if (equal? (car pr) obj)
                             (success-proc pr)
                             (lookup (cdr table))))))))
      (lookup table))))

(define assoc
  (lambda (obj table)
    (lookup obj table (lambda (pr) pr) (lambda () #f))))

(define fib
  (lambda (n)
    (display n) (display " ")
    (if ( < n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2))))))

(define memoize
  (lambda (proc)
    (let ((table '()))
      (lambda (arg)
        (lookup arg table 
                (lambda (pr) (cdr pr))
                (lambda ()
                  (let ((val (proc arg)))
                    (set! table (cons (cons arg val) table))
                    val)))))))

(define fib-m (memoize fib))

(define memo-fib
  (memoize (lambda (n)
             (display n) (display " ")
             (if (< n 2)
               n
               (+ (memo-fib (- n 1))
                  (memo-fib (- n 2)))))))

(define vector-memoize
  (lambda (max-arg)
    (lambda (proc)
      (let ((table (make-vector (add1 max-arg) '())))
        (lambda (arg)
          (if (> arg max-arg)
            (proc arg)
            (let ((item-stored (vector-ref table arg)))
              (if (pair? item-stored)
                (car item-stored)
                (let ((val (proc arg)))
                  (vector-set! table arg (list val))
                  val)))))))))

(define matrix-memoize
  (lambda (rows cols)
    (lambda (proc)
      (let ((table (make-zero-matrix rows cols)))
        ((matrix-set! table) 0 0 1)
        (lambda (r c)
          (if (or (< c 0) 
                  (>= c (num-cols table))
                  (< r 0))
            0
            (let ((item-stored ((matrix-ref table) r c)))
              (if (not (= item-stored 0))
                item-stored
                (let ((val (proc r c)))
                 ; (writeln "ref is: " r " and " c " value: " val)
                  ((matrix-set! table) r c val)
                  val)))))))))

(define memo-pascal-triangle
  (lambda (r c)
    (letrec ((helper
              ((matrix-memoize (add1 r) (add1 r))
               (lambda (n k)
                    (+ (helper (- n 1) (- k 1))
                       (helper (- n 1) k))))))
      (helper r c))))

(define memo-fib-vec
  ((vector-memoize 100)
   (lambda (n)
     (display n) (display " ")
     (if (< n 2)
       n
       (+ (memo-fib-vec (- n 1)) (memo-fib-vec (- n 2)))))))

(define imp-member?
  (lambda (item ls)
    (let ((goto (lambda (label) (label))))
      (letrec
        ((start (lambda ()
                  (cond
                    ((null? ls) #f)
                    ((equal? (car ls) item) #t)
                    (else (goto reduce)))))
         (reduce (lambda ()
                   (set! ls (cdr ls))
                   (goto start))))
        (goto start)))))

(define pascal-triangle
  (lambda (n k)
    (cond
      ((and (= n 0) (= k 0)) 1)
      ((or (< k 0) (> k n)) 0)
      (else 
        (+ (pascal-triangle (- n 1) (- k 1))
           (pascal-triangle (- n 1) k))))))

(define last-pair
  (lambda (x)
    (if (pair? (cdr x))
      (last-pair (cdr x))
      x)))

(define append!
  (lambda (ls1 ls2)
    (if (pair? ls1)
      (begin
        (set-cdr! (last-pair ls1) ls2)
        ls1)
      ls2)))

(define mystery
  (lambda (x)
    (let ((box (last-pair x)))
      (set-cdr! box x)
      x)))
(define ans (mystery (list 'a 'b 'c 'd)))

(define cycle?
  (lambda (x)
    (letrec
      ((helper
        (lambda (ref current)
      ;(display "a")
          (if (eq? ref current)
            #f
            (if (eq? ref (cdr current))
              #t
              (helper (cdr ref) current))))))
      (letrec
        ((loop
          (lambda (ls)
         ;(writeln "searching " (car ls))(newline)
            (if (null? ls)
              #f
              (if (helper x ls)
                #t
                (loop (cdr ls)))))))
        (loop x)))))

(define loop
  (let ((x (list 'a 'b 'c 'd 'e)))
    (set-cdr! (last-pair x) (cdr (cdr x)))
    x))

(define writeln2
  (lambda args
    (for-each (lambda (arg)
                  (letrec 
                    ((continue (lambda (n ls)
                             (if (= n 9)
                               '(...)
                               (cons (car ls) (continue (add1 n) (cdr ls)))))))
                    (if (cycle? arg)
                      (display (continue 1 arg))
                      (display arg)))) args)))

(define reconfigure
  (lambda (tape char dir)
    (if (eq? dir 'left)
      (left (overwrite char tape))
      (right (overwrite char tape)))))

(define 3rd
  (lambda (tape)
    (caddr tape)))
(define 4th
  (lambda (tape)
    (cadddr tape)))
(define 2nd
  (lambda (tape)
    (cadr tape)))
(define 1st 
  (lambda (tape)
    (car tape)))

(define at
  (lambda (tape)
    (let ((right-part (2nd tape)))
      (car right-part))))

(define overwrite
  (lambda (char tape)
    (let ((right-part (2nd tape)))
        (set-car! right-part char)
        (if (null? (cdr right-part))
          (append! right-part (list 0)))
        tape)))

(define right
  (lambda (tape)
    (let ((left-part (1st tape)) (right-part (2nd tape)))
      (let ((new-left (cons (car right-part) left-part))
            (new-right (cdr right-part)))
        (list new-left (check-null new-right))))))

(define left
  (lambda (tape)
    (let ((left-part (1st tape)) (right-part (2nd tape)))
      (let ((new-left (cdr left-part))
            (new-right (cons (car left-part) right-part)))
        (list (check-null new-left) new-right )))))

(define check-null
  (lambda (part)
    (if (null? part)
      (list 0)
      part)))

(define test-reconfigure
  (lambda ()
    (let ((tape1 (list (list 'a 'b 'c 0) (list 'x 'y 0))))
      (let ((tape2 (reconfigure tape1 'u 'right))
            (tape3 (reconfigure tape1 'd 'left)))
        (let ((tape4 (reconfigure tape2 'v 'right))
              (tape5 (reconfigure tape3 'e 'left)))
          (let ((tape6 (reconfigure tape4 'w 'right))
                (tape7 (reconfigure tape5 'f 'left)))
            (let ((tape8 (reconfigure tape6 'x 'right))
                  (tape9 (reconfigure tape7 'g 'left)))
              (list tape8 tape9))))))))

(define tape->list
  (lambda (tape)
    (let ((left-part (1st tape)) (right-part (2nd tape)))
      (let ((reverse-left (cdr (reverse left-part)))
            (reverse-right (reverse (cdr (reverse right-part)))))
        ;(set-cdr! (last-pair reverse-left) reverse-right)
        (append! reverse-left reverse-right)
        reverse-left))))

(define list->tape
  (lambda (ls)
    (list (list 0) (reverse (cons 0 (reverse ls))))))

(define busy-beaver
  (letrec
    ((loopright (lambda (tape)
                  (let ((c (at tape)))
                    (cond 
                      ((equal? c 'a)
                       (loopright (reconfigure tape 'a 'right)))
                      (else (maybe-done (reconfigure tape 'a 'right)))))))
     (maybe-done (lambda (tape)
                   (let ((c (at tape)))
                     (cond
                       ((equal? c 'a) (reconfigure tape 'a 'right))
                       (else (continue (reconfigure tape 'a 'left)))))))
     (continue (lambda (tape)
                 (let ((c (at tape)))
                   (cond
                     ((equal? c 'a)
                      (maybe-done (reconfigure tape 'a 'left)))
                     (else (loopright (reconfigure tape 'a 'right))))))))
    loopright))

(define run-lines
  (lambda (lines tape)
    (letrec
      ((driver
         (lambda (state tape)
           (if (eq? state 'halt)
             tape
             (let ((matching-line
                     (find-line state (at tape) lines)))
               (driver (next-state matching-line)
                       (reconfigure 
                         tape
                         (next-char matching-line)
                         (next-direction matching-line))))))))
      (driver (current-state (car lines)) tape))))

(define current-state car)
(define next-char caddr)
(define next-direction cadddr)
(define next-state (lambda (ls) (car (last-pair ls))))

(define find-line 
  (lambda (state char lines)
    ;(writeln "state: " state " char: " char )
    (letrec
      ((helper 
         (lambda (line)
           (let ((current (car line)))
             (if (and (equal? state (car current))
                      (equal? char (cadr current)))
               current
               (helper (cdr line)))))))
      (helper lines))))

(define busy-beaver-lines
  '((loopright a a right loopright)
    (loopright 0 a right maybe-done)
    (maybe-done a a right halt)
    (maybe-done 0 a left continue)
    (continue a a left maybe-done)
    (continue 0 a right loopright)))

(define main
  (lambda ()
    (display (run-lines busy-beaver-lines (list->tape '())))(newline)
    (display (busy-beaver (list->tape '())))(newline)
    (display (tape->list (busy-beaver (list->tape '()))))(newline)
    (display (list->tape '(c b a x y)))(newline)
    (display (tape->list (list (list 'a 'b 'c 0) (list 'x 'y 0))))(newline)
    (display (test-reconfigure))(newline)
    (writeln2 loop)(newline)
    (writeln2 ans)(newline)
    (writeln '(1 2 3 4 5))
    (display (cycle? loop))(newline)
    (display (cycle? ans))(newline)
    (display (memo-pascal-triangle 5 1))(newline)
    (display (pascal-triangle 5 1))(newline)
    (display (imp-member? 'a '(1 2 3 a b)))(newline)
    (display (assoc 4 '((1) (2 1) (3 1 5) (4 3) (6 8)))) (newline)
    (timer fib 6)
    (timer fib-m 6)
    (timer memo-fib 6)
    (timer memo-fib-vec 6)
    ))
