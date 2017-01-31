#!/usr/bin/gsi-script

(load "chp9-vector")

(define insertsort
  (lambda (ls)
    (if (singleton-list? ls)
      ls
      (insert (car ls) (insertsort (cdr ls))))))

(define insert
  (lambda (a ls)
    (cond
      ((null? ls) (cons a '()))
      ((< a (car ls)) (cons a ls))
      (else (cons (car ls) (insert a (cdr ls)))))))

(define numList (list 60 50 40 30 20 10 ))

(define quicksort
  (lambda (rel)
    (letrec
      ((collect
         (lambda (pivot ls lgroup rgroup)
           (if (null? ls)
             (append ((quicksort rel) lgroup) (cons pivot ((quicksort rel) rgroup)))
             (if (rel (car ls) pivot)
               (collect pivot (cdr ls) (cons (car ls) lgroup) rgroup)
               (collect pivot (cdr ls) lgroup (cons (car ls) rgroup)))))))
      (lambda (ls)
        (if (or (null? ls) (null? (cdr ls)))
          ls
          (collect (car ls) (cdr ls) '() '()))))))

(define random-list
  (lambda (n)
    (letrec ((build-list
               (lambda (k)
                 (if (zero? k)
                   '()
                   (cons (random n) (build-list (sub1 k)))))))
      (build-list n))))

(define timer
  (lambda (proc arg)
    (let* ((start (time->seconds (current-time)))
           (val (proc arg))
           (finish (time->seconds (current-time)))
           (elapsed-time (/ (- finish start) 100)))
      (writeln "Time= " elapsed-time ", Answer= " val))))

(define binary-search
  (lambda (rel)
    (lambda (vec target)
      (letrec
        ((search
           (lambda (left right)
             (if (< right left)
               (writeln "Search failed")
               (let* ((middle (floor (/ (+ left right) 2)))
                      (mid-val (vector-ref vec middle)))
                 (cond
                   ((rel target mid-val)
                    (search left (sub1 middle)))
                   ((rel mid-val target)
                    (search (add1 middle) right))
                   (else middle)))))))
        (search 0 (sub1 (vector-length vec)))))))

(define table10-17
  '(("Smith, Harold W" 2324 43 1974 "Fox, Charles Q." 49325)
    ("Jones, Mary Ann" 1888 54 1965 "none" 65230)
    ("Whites, Thomas P" 3403 34 1982 "Smith, Harold W." 27300)))

(define vec-table (list->vector table10-17))

(define find-name-rel
  (lambda (ls-1 ls-2)
    (cond
      ((and (pair? ls-1)
               (pair? ls-2))
        (string<? (car ls-1) (car ls-2)))
      ((and (pair? ls-1)
               (not (pair? ls-2)))
        (string<? (car ls-1) ls-2))
      ((and (not (pair? ls-1))
               (pair? ls-2))
        (string<? ls-1 (car ls-2))))))

(define unlist
  (lambda (proc)
    (lambda (ls)
      (apply proc ls))))

(define age-test?
  (unlist 
    (lambda (name id age yr-emp super salary)
      (> age 25))))

(define set10-17
  (list->set table10-17))

(define over-45?
  (unlist
    (lambda (name id age yr-emp super salary)
      (> age 45))))

(define over-45-set
  ((set-builder over-45? the-empty-set) set10-17))

(define main
  (lambda ()
    (display (set->list
               (set-map
                 (unlist
                   (lambda (name id age yr super salary)
                     (list id age yr salary)))
                 over-45-set)))(newline)
    (display ((for-all age-test?) (list->set table10-17))) (newline)
    (display ((quicksort find-name-rel) table10-17))(newline)
    (display (let ((sorted-list ((quicksort find-name-rel) table10-17)))
               ((binary-search find-name-rel) 
                (list->vector sorted-list) "Jones, Mary Ann")))
               (newline)
    ;(display vec-table)(newline)
    (display (let ((name (vector "Ann s" "Ben J" "Ed A" "Guy S" "Kay W")))
               ((binary-search string<?) name "Ed B")))(newline)
    (display (random-list 10))(newline)
    (display ((quicksort >) (random-list 10)))(newline)
    (display (insertsort numList))(newline)
    ))
