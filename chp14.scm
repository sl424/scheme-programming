#!/usr/bin/gsi-script

(load "chp13")

(define-macro (unless test . body)
                `(if ,test #f (begin ,@body)))

(define-macro (freeze expr1 expr2 )
              `(lambda () ,expr1 ,expr2 ))

(define thaw (lambda (thunk) (thunk)))

(define th (freeze (display "A random number is: ") (random 10)))

(define make-promise "procedure")
(define =force= "procedure")

(let ((delayed-tag "delay") (value-tag "-->"))
  (set! make-promise (lambda (thunk) (cons delayed-tag thunk)))
  (set! =force= (lambda (arg)
                  (if (and (pair? arg)
                           (eq? (car arg) delayed-tag))
                    (begin
                      (set-car! arg value-tag)
                      (set-cdr! arg (thaw (cdr arg)))))
                  (cdr arg))))

(define-macro (=delay= expr1 expr2)
              `(make-promise (freeze ,expr1 ,expr2)))

(define pr (=delay= (display "a random num is: ") (random 10)))

(define make-lambda
  (lambda (param body-expr)
    (cons 'lambda (cons param body-expr))))

(define make-list-of-params
  (lambda (code) (map 1st code)))
(define make-list-of-operands
  (lambda (code) (map 2nd code)))

(define-macro (=let= code body)
              `(apply (lambda (a b) ,body) 
                (make-list-of-operands ,code)))

(define main
  (lambda ()
    (display (make-list-of-params '((a 2) (b 3))))(newline)
    (display (make-list-of-operands '((a 2) (b 3))))(newline)
    (display (=let= '((a 10) (b 10)) (* a b)))(newline)
    (display (=force= pr))(newline)
    (display (=force= pr))(newline)
    (display (thaw th))(newline)
    (display (thaw th))(newline)
    (display (thaw th))(newline)
    (display (thaw th))(newline)
    (display (let ((test 111)) (unless (= 1 2) (list test test))))(newline)
    ))
