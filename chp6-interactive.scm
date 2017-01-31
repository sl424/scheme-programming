#!/usr/bin/gsi-script

(load "chp5-local_proc")

(define substring?
  (lambda (sstr strng)
    (let ((nsub (string-length sstr)) 
          (nstr (string-length strng)))
      (cond
        ((string=? sstr strng) #t)
        ((< nstr nsub) #f)
        ;((zero? nstr) #f)
        (else 
            (or (substring? sstr (substring strng 1 nstr))
                (substring? sstr (substring strng 0 (sub1 nstr)))
                ))))))

(define substring-ref
  (lambda (strng n)
    (substring strng n (add1 n))))

(define string-reverse
  (letrec ((helper
             (lambda (strng n ret)
                 (cond
                   ((< n 0) ret)
                   (else (helper 
                           strng 
                           (sub1 n) 
                           (string-append ret (substring-ref strng n))))))))
  (lambda (strng) 
    (let ((n (string-length strng)))
      (helper strng (sub1 n) "")))))

(define palindrome?
  (lambda (strng)
    (let ((n (string-length strng)))
    (let (;(n (string-length strng))
          (mid (quotient n 2))
          (re (modulo n 2)))
      (cond
        ((string=? (substring strng 0 mid) 
                   (string-reverse (substring strng (+ mid re) n))) #t)
        (else #f
          ;(display (substring strng 0 (quotient n 2)))(newline) 
          ;(display (substring strng (quotient n 2) n ))
              ))))))



(define int2string
  (lambda (n ls)
    (if (= n 0)
      (car ls)
      (int2string (sub1 n) (cdr ls)))))

(define convert-date
  (lambda (m d year)
    (let ((c (floor (/ year 100))))
      (let ((y (- year (* c 100))))
        (writeln "c: " c " y: " y " m: " m " d: " d)
        (let ((a (floor (/ (- (* 13 m) 1) 5)))
              (b (floor (/ y 4)))
              (ls (list 'Sun 'Mon 'Tue 'Wed 'Thu 'Fri 'Sat))
              (e (floor (/ c 4))))
          (int2string
          (modulo 
               (+ a (+ b (+ e (+ d (- y (* 2 c))))))
               7)
          ls))))))

(define fwday
  (lambda ()
    (writeln "Enter a date to find the weekday or 'done' to quit:")
    (let ((m (read)))
      (if (eq? m 'done)
        (writeln "goodbye")
        (begin
          (let (
          (d (read))
          (y (read)))
            (cond 
              ((= m 1)
                (writeln "The weekday of " m d y " is " (convert-date 11 d (sub1 y))) (newline)
                (fwday))
              ((= m 2)
                (writeln "The weekday of " m d y " is " (convert-date 12 d (sub1 y))) (newline)
                (fwday))
              (else
                (writeln "The weekday of " m d y " is " (convert-date (- m 2) d y)) (newline)
                (fwday))
              )))))))
      
(define main
  (lambda ()
    (display (palindrome? "able was I ere I saw elba"))(newline)
    (display (palindrome? "mom n dad"))(newline)
    (display (substring? "s a s" "This is a string.")) (newline)
    (display (substring? "ringer" "This is a string.")) (newline)
    (display (string-reverse "Jack and Jill"))(newline)
    (fwday)(newline)
    ))
