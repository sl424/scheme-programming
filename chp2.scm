#!/usr/bin/gsi-script

(define singleton-list?
  (lambda (ls)
    (and (pair? ls) (null? (cdr ls)))))

(define lists
	(lambda (item)
		(cons item '())))

(define second
	(lambda (ls)
		(cadr ls)))

(define juggle
	(lambda (ls)
		(cons (caddr ls) (cons (car ls) (cons (cadr ls) '())))
		))

(define switch
	(lambda (ls)
		(cons (caddr ls) (cons (cadr ls) (cons (car ls) '())))
		))

(define asym
	(lambda (ls)
		(if (or (symbol? (car ls))
		  			(symbol? (cadr ls)))
			(quote what)
			(quote now))))

(define mystery
	(lambda (ls)
		(if (null? (cddr ls))
			(cons (car ls) '())
			(cons (car ls) (mystery (cdr ls))))))

(define remove-1st 
	(lambda (item ls)
		(if (null? ls) 
			'()
			(if (equal? (car ls) item ) 
				(cdr ls)
				(cons (car ls) (remove-1st item (cdr ls)))))))

(define member?
	(lambda (item ls)
		(if (null? ls)
			#f
			(or ( equal? (car ls) item )
					 ( member? item (cdr ls))))))

(define insleft
	(lambda (item key ls)
		(cond
			( (null? ls) '())
			( (equal? (car ls) key) (cons item ls))
			(else (cons (car ls) (insleft item key (cdr ls)))))))

(define lofi
	(lambda (ls)
		(cond 
			((null? ls) '())
			(else (cons (car (car ls)) (lofi(cdr ls)))))))

(define remove2nd
	(lambda (item ls)
		(if (null? ls)
			'()
			(if (equal? (car ls) item)
				(cons (car ls) (remove-1st item (cdr ls)))
				(cons (car ls) (remove2nd item (cdr ls)))))))

(define allsame
	(lambda (ls)
		(if (null? ls)
			#t
			(if (equal? (cdr ls) '())
				#t
				(eqv? (equal? (car ls) (cadr ls)) (allsame (cdr ls)))))))


(define remove-1st-trace
	(lambda (item ls)
    (cond
      ((entering (null? ls) ls 1)
       (leaving '() 1))
      ((entering (equal? (car ls) item) ls 2)
       (leaving (cdr ls) 2))
      ((entering 'else ls 3)
       (leaving (cons (car ls) (remove-1st-trace item (cdr ls))) 3)))))

(define entering
  (lambda (test input number)
    (begin
      (if test (writeln " Entering cond-clause-" 
                      number " with ls = " input))
      test)))

(define leaving
  (lambda (result number)
    (begin
      (writeln "Leaving cond-clause-"
               number " with result = " result)
      result)))

(define tracing
  (lambda (result message)
    (begin
      (writeln message result)
      result)))

(define test-tracing
  (lambda (test input message)
    (begin
      (if test (tracing message input))
        test)))

(define writeln
  (lambda args
    (for-each display args)
    (newline)))

(define swapper
  (lambda (x y ls)
    (cond
      ;((entering (null? ls) ls 1) (leaving '() 1))
      ((test-tracing (null? ls) " Entering 1 " ls)
       (tracing '() "Leaving 1 "))
      ;((entering 'else ls 2)
       ;(leaving (cons (swap x y (car ls)) (swapper x y (cdr ls))) 2 )))))
      ((test-tracing 'else " Entering 2 " ls)
       (tracing (cons (swap x y (car ls)) (swapper x y (cdr ls))) "Leaving 2 " )))))

(define swap
  (lambda (x y key)
    (cond
      ;((equal? key x) y)
      ;((entering (equal? key x) key '2a) (leaving y '2a))
      ((test-tracing (equal? key x) " entering 2a " key )
       (tracing y " leaving 2a "))
      ;((entering (equal? key y) key '2b) (leaving x '2b))
      ((test-tracing (equal? key y) " entering 2b " key)
       (tracing x " leaving 2b "))
      ;((equal? key y) x)
      ;(else key))))
      ;((entering 'else key '2c) (leaving key '2c)))))
      ((test-tracing 'else " entering 2c " key)
       (tracing key " leaving 2c ")))))

(define describe
  (lambda (s)
    (cond
      ((null? s) (quote '()))
      ((number? s) s)
      ((symbol? s) (list 'quote s))
      ((pair? s) (list 'cons (describe (car s)) (describe (cdr s))))
      (else s))))

(define s 'string)

(define (main arg)
	(display (lists arg)) (newline)
	(display (second '(1 2 3))) (newline)
	(display (juggle '(jump quick spot))) (newline)
	(display (juggle '(dog bites man))) (newline)
	(display (switch '(dog bites man))) (newline)
	(display (asym '(a 1))) (newline)
	(display (asym '(2 1))) (newline)
	(display (mystery '(1 2 3 4 5))) (newline)
	(display (remove-1st 'c '(a b c d))) (newline)
	(display (member? 2 '(1 (2 3) 4))) (newline)
	(display (insleft 'hot 'dogs '(I eat dogs))) (newline)
	(display (lofi '((a) (b c d) (e f)))) (newline)
	(display (remove2nd 'cat '())) (newline)
	(display (allsame '())) (newline)
  (remove-1st-trace 'c '(a b c d))
  (remove-1st-trace 'e '(a b c d))
  (display (swapper 'cat 'dog '(my cat eats dog food))) (newline)
  (display (describe s))(newline)
	)
