#!/usr/bin/gsi-script

(load "chp8-set_relations")

(define view
  (lambda (vec)
    (lambda args
    (let ((maxidx (sub1 (vector-length vec))))
      (letrec ((loop (lambda (i)
                       (display (vector-ref vec i))
                       (if (< i maxidx)
                         (begin
                           (display " ")
                           (loop (add1 i)))))))
        (if (not (null? args))
        (display (car args)))
        (loop 0)
        (if (not (null? args))
        (display (cadr args))))))))

(define vector-view
  (lambda (vec)
    ((view vec) "<" ">")))

(define vector-generator
  (lambda (gen-proc)
    (lambda (size)
      (let ((vec (make-vector size)))
        (letrec
          ((loop (lambda (i)
                   (if (< i size)
                     (begin
                       (vector-set! vec i (gen-proc i))
                       (loop (add1 i)))))))
          (loop 0))
        vec))))

(define vector-map
  (lambda (proc vec)
    ((vector-generator (lambda (i) (proc (vector-ref vec i))))
       (vector-length vec))))

(define multiply-by-scalar
  (lambda (c vec)
    (vector-map (lambda (elem) (* c elem)) vec)))

(define vector-to-both
  (lambda (proc)
    (lambda (vec1 vec2)
      (let ((gen-proc
              (lambda (i)
                (proc (vector-ref vec1 i) (vector-ref vec2 i)))))
        ((vector-generator gen-proc) (vector-length vec1))))))

(define vec+ (vector-to-both +))
(define vec* (vector-to-both *))

(define vector-update
  (lambda (vec k val)
    (let ((gen-proc (lambda (i)
                      (if (= j k)
                        val
                        (vector-ref vec i)))))
      ((vector-generator gen-proc) (vector-length vec)))))

(define vector-update!
  (lambda (vec i c)
    (vector-set! vec i c)
    vec))

(define =list->vector=
  (lambda (ls)
    (let ((vec (make-vector (length ls))))
      (letrec
        ((convert
           (lambda (ls i)
             (if (not (null? ls))
               (begin
                 (vector-set! vec i (car ls))
                 (convert (cdr ls) (add1 i)))))))
        (convert ls 0))
      vec)))

(define vector-accumulate
  (lambda (proc seed)
    (lambda (vec)
      (let ((size (vector-length vec)))
        (letrec
          ((helper
             (lambda (i)
               (if (= i size)
                 seed
                 (proc (vector-ref vec i) (helper (add1 i)))))))
          (helper 0))))))

(define vector-sum (vector-accumulate + 0))
(define vector-product (vector-accumulate * 1))
;(define dot-product (compose vector-sum vec*))

(define successive-powers
  (lambda (base)
    (vector-generator 
      (lambda (i) (expt base i)))))

(define vector-linear-search
  (lambda (vec key)
    (letrec ((helper
               (lambda (i)
                 (if (>= i (vector-length vec)) 
                   (error "cannot be found")
                   (if (equal? key (vector-ref vec i))
                     i
                     (helper (add1 i)))))))
      (helper 0))))

(define num-cols
  (lambda (mat)
    (let ((size (sub1 (vector-length mat))))
      (vector-ref mat size))))

(define num-rows
  (lambda (mat)
    (let ((size (sub1 (vector-length mat))))
      (/ size (vector-ref mat size)))))

(define matrix-ref
  (lambda (mat)
    (let ((ncols (num-cols mat))
          (nrows (num-rows mat)))
      (lambda (i j)
        (if (and (< i nrows)
                 (< j ncols))
          (vector-ref mat (+ (* i ncols) j))
          (error "out of range"))))))

(define matrix-generator
  (lambda (gen-proc)
    (lambda (nrows ncols)
      (let ((size (* nrows ncols)))
        (let ((vec-gen-proc
                (lambda (k)
                  (if (< k size)
                    (gen-proc (quotient k ncols)
                              (remainder k ncols))
                    ncols))))
          ((vector-generator vec-gen-proc) (add1 size)))))))

(define make-zero-matrix (matrix-generator (lambda (i j) 0)))
(define identity-matrix (matrix-generator (lambda (i j) (if (= i j) 1 0))))

(define row-of
  (lambda (mat)
    (let ((mat-ref (matrix-ref mat))
          (number-of-columns (num-cols mat)))
      (lambda (i)
        (let ((gen-proc (lambda (j) (mat-ref i j))))
          ((vector-generator gen-proc) number-of-columns))))))


(define column-of
  (lambda (mat)
    (let ((mat-ref (matrix-ref mat))
          (number-of-rows (num-rows mat)))
      (lambda (j)
        (let ((gen-proc (lambda (i) (mat-ref i j))))
          ((vector-generator gen-proc) number-of-rows))))))

(define matrix
  (lambda (rows cols)
    (lambda args
      (let ((vec (list->vector args)))
        ((matrix-generator (lambda (i j)
                            (let ((p (+ (* i cols) j)))
                              (vector-ref vec p)))) rows cols)))))

(define matrix-view
  (lambda (mat)
    (let ((nrow (num-rows mat)))
      (letrec 
        ((helper
           (lambda (row)
             (if (< row nrow)
               (begin
                 ((view ((row-of mat) row)))(newline)
                 (helper (add1 row)))))))
        (helper 0)))))

(define mat+
  (lambda (mat-a mat-b)
    (let ((ncol (num-cols mat-a))
          (nrows (num-rows mat-a)))
      (let ((gen-proc
              (lambda (i j) (+ ((matrix-ref mat-a) i j) 
                               ((matrix-ref mat-b) i j)))))
        ((matrix-generator gen-proc) nrows ncol)))))

(define matrix-set!
  (lambda (mat)
    (let ((ncols (num-cols mat)))
      (lambda (i j obj)
        (vector-set! mat (+ (* i ncols) j) obj)))))

(define main
  (lambda ()
    (matrix-view
    (mat+ ((matrix 2 2) 1 1
                        1 1)
          ((matrix 2 2) 2 2 
                        2 2)))
    (matrix-view ((matrix 3 4) 5 2 3 7 
                               1 4 0 5 
                               8 3 1 2))(newline)
    (display ((row-of (identity-matrix 4 4))2))(newline)
    (display (make-zero-matrix 5 5))(newline)
    (display ((matrix-ref '#(5 2 3 7 1 4 0 5 8 3 1 2 4)) 2 3 ))(newline)
    (display (vector-sum (vector 1 3 5 7 9)))(newline)
    (vector-view (vec+ (vector 1 3 5 7 9) (vector 9 7 5 3 1)))(newline)
    (vector-view (vector-map add1 (vector 10 11 12 13)))(newline)
    (vector-view ((vector-generator add1) 6))(newline)
    (display (vector-linear-search '#(a b c d e) 'f))(newline)
    (display (vector 1 2 3 4))(newline)
    (display ((successive-powers 2) 8))(newline)
    ))
