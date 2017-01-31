#!/usr/bin/gsi-script

(load "chp6-interactive")

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define compose-many
  (lambda args ; unrestrained lambda
    (lambda (x)
      (if (null? args)
        x
        ((car args) ((apply compose-many (cdr args)) x))
        ))))

(define map-two
  (lambda (proc ls)
    (if (= (length ls) 1)
      '()
      (cons (proc (car ls) (cadr ls)) (map-two proc (cdr ls))) )))

(define reduce
  (lambda (proc ls)
    (if (= (length ls) 2)
      (proc (car ls) (cadr ls))
      (reduce proc (cons (proc (car ls) (cadr ls)) (cddr ls)))
      )))

(define map
  (lambda (proc ls)
    (if (null? ls)
      '()
      (cons (proc (car ls)) (map proc (cdr ls))))))

(define map2
  (lambda (proc lsa lsb)
    (if (null? lsa)
      '()
      (cons (proc (car lsa) (car lsb)) (map2 proc (cdr lsa) (cdr lsb))))))
      

(define round-n-places
  (lambda (n)
    (let ((scale-factor (expt 10 n)))
      (letrec ((helper 
                 (lambda (dec-num)
                      (/ (round (* dec-num scale-factor)) scale-factor))))
      helper))))

(define round-5-places (round-n-places 5))

(define flat-recur
  (lambda (seed list-proc)
    (letrec
      ((helper
         (lambda (ls)
           (if (null? ls)
             seed
             (list-proc (car ls) (helper (cdr ls)))))))
      helper)))

(define insert-left-m
  (lambda (item key)
    (flat-recur 
      '(); seed
      (lambda (x y) ;list-proc
        (if (equal? key x)
          (cons item (cons x y))
          (cons x y))))))

(define insert-left
  (lambda (item key ls)
    ((insert-left-m item key) ls)))

(define partial
  (lambda (seed pred)
    (lambda (item-proc a b)
      (let ((ls 
              (letrec ((makelist
                         (lambda (a b)
                           (if (> a b)
                             '()
                             (cons a (makelist (add1 a) b))))))
                       (makelist a b))))
      ((flat-recur
        seed
        (lambda (x y)
          (pred (item-proc x) y))) ls)
        ))))


(define partial-sum (partial 0 +))

(define deep-recur
  (lambda (seed item-proc list-proc)
    (letrec
      ((helper
         (lambda (ls)
           (if (null? ls)
             seed
             (let ((a (car ls)))
               (if (or (pair? a) (null? a))
                 (list-proc (helper a) (helper (cdr ls)))
                 (item-proc a (helper (cdr ls)))))))))
      helper)))

(define subst-all-m
  (lambda (new old)
    (deep-recur
      ;seed
      '()
      ;item proc
      (lambda (x y)
        (if (= old x)
          (cons new y)
          (cons x y)))
      ;list proc
      cons)))

(define main
  (lambda ()
    (display ((subst-all-m 1 0) '(0 1 2 ((0 1 2)))))(newline)
    (display (partial-sum (lambda (m) (* m m)) 3 7)) (newline)
    (display (insert-left 'z 'a '(a b a c a)))(newline)
    (display (round-5-places (sqrt 2))) (newline)
    (display (map2 + '(1 2 3 4) '(5 7 9 11)))(newline)
    (display (reduce + '(1 3 5 7 9)))(newline)
    (display (map-two + '(2 3 4 5 7)))(newline)
    (display ((compose-many add1 add1 add1 add1) 3)) (newline)
    (display ((compose-many sqrt abs sub1 (lambda (n) (* n n))) 0.6)) (newline)
    ))
