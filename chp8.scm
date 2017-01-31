#!/usr/bin/gsi-script

(load "chp7")

(define random-maker
  (lambda (m a seed)
    (lambda (n)
      (let ((u (/ seed m)))
        (set! seed (modulo (* a seed) m))
        (floor (* n u))))))
(define random-time
  (lambda () 1000))

(define random
  (random-maker (- (expt 2 31) 1) (expt 7 5) (random-time)))

(define both
  (lambda (pred)
    (lambda (arg1 arg2)
      (and (pred arg1) (pred arg2)))))
(define neither
  (lambda (pred)
    (lambda (arg1 arg2)
      (not (or (pred arg1) (pred arg2))))))

(define at-least-one
  (lambda (pred)
    (lambda (arg1 arg2)
      (or (pred arg1) (pred arg2)))))

(define set-tag "set")

(define the-empty-set
  (cons set-tag '()))

(define empty-set?
  (lambda (s)
    (eq? s the-empty-set)))

(define set?
  (lambda (arg)
    (and (pair? arg) (eq? (car arg) set-tag))))

(define pick
  (lambda (s)
    (let ((ls (cdr s)))
      (if (null? ls)
        (error "pick: the set is empty.")
        (list-ref ls (random (length ls)))))))

(define residue
  (lambda (elem)
    (lambda (s)
      (let ((ls (remove elem (cdr s))))
        (cond
          ((null? ls) the-empty-set)
          (else (cons set-tag ls)))))))

(define adjoin
  (lambda (elem s)
    (cons set-tag (cons elem (cdr s)))))

(define make-set
  (lambda args
    (letrec
      ((list-make-set
         (lambda (args-list)
           (if (null? args-list)
             the-empty-set
             (adjoin
               (car args-list)
               (list-make-set (cdr args-list)))))))
      (list-make-set args))))

(define none
  (lambda (pred)
    (letrec
      ((test
         (lambda (s)
           (or (empty-set? s)
               (let ((elem (pick s)))
                 (and (not (pred elem))
                      (test ((residue elem) s))))))))
      test)))

(define there-exists
  (lambda (pred)
    (compose not (none pred))))

(define for-all
  (lambda (pred)
    (none (compose not pred))))

(define set-equal
  (lambda (obj1)
    (lambda (obj2)
      (or (and ((neither set?) obj1 obj2)
               (equal? obj1 obj2))
          (and ((both set?) obj1 obj2)
               ((subset obj1) obj2)
               ((subset obj2) obj1))))))

(define element
  (compose there-exists set-equal))

(define contains
  (lambda (set)
    (lambda (obj)
      ((element obj) set))))

(define superset
  (lambda (s1)
    (lambda (s2)
      ((for-all (contains s1)) s2))))

(define subset
  (lambda (s1)
    (lambda (s2)
      ((superset s2) s1))))

(define cardinal
  (lambda (s)
    (if (empty-set? s)
      0
      (let ((elem (pick s)))
        (add1 (cardinal ((residue elem) s)))))))

(define set-builder
  (lambda (pred base-set)
    (letrec
      ((helper
         (lambda (s)
           (if (empty-set? s)
             base-set
             (let ((elem (pick s)))
               (if (pred elem)
                 (adjoin elem (helper ((residue elem) s)))
                 (helper ((residue elem) s))))))))
      helper)))

(define unrestrict-set
  (letrec ((helper
             (lambda (proc base ls)
               (if (null? ls)
                 base
                 (proc (car ls) (helper proc base (cdr ls)))))))
    (lambda (proc base args)
      (helper proc base args))))
                 
(define intersection
  (lambda (s1 s2)
    ((set-builder (contains s2) the-empty-set) s1)))

(define union
  (lambda (s1 s2)
    ((set-builder (compose not (contains s2)) s2) s1)))

(define union2
  (lambda args
    (unrestrict-set union the-empty-set args)))

(define difference
  (lambda (s1 s2)
    ((set-builder (compose not (contains s2)) the-empty-set) s1)))

(define list->set
  (lambda (ls)
    (apply make-set ls)))

(define set->list
  (lambda (s)
    (if (empty-set? s)
      '()
      (let ((elem (pick s)))
        (cons elem (set->list ((residue elem) s)))))))

(define select-by-cardinal
  (lambda (int)
    (lambda (s)
      (letrec ((helper
              (lambda (set)
                (if (empty-set? set)
                  the-empty-set
                  (let ((a-set (pick set)))
                    (if (= (cardinal a-set) 2)
                      (adjoin a-set (helper ((residue a-set) set)))
                      (helper ((residue a-set) set))))))))
        (helper s)))))

(define set-map
  (lambda (proc s)
    (if (empty-set? s)
      the-empty-set
      (let ((elem (pick s)))
        (adjoin (proc elem)
                (set-map proc ((residue elem) s)))))))

(define make-op
  (lambda (x y)
    (cons x y)))
(define op? pair?)
(define op-1st car)
(define op-2nd cdr)

(define cartesian-product
  (lambda (s1 s2)
    (if (empty-set? s1)
      the-empty-set
      (let ((elem (pick s1)))
        (union (set-map (lambda (x) (make-op elem x)) s2)
               (cartesian-product ((residue elem) s1) s2))))))

(define domain
  (lambda (rel)
    (set-map op-1st rel)))
(define range
  (lambda (rel)
    (set-map op-2nd rel)))

(define old-rel
  (make-set (make-op 'tom 'bob)
            (make-op 'tom 'jim)
            (make-op 'bob 'jim)))

(define subrelation
  (lambda (rel)
    (lambda (arg)
      ((set-builder
         (lambda (x) ((set-equal (op-1st x)) arg))
         the-empty-set)
       rel))))

(define subrelation2
  (lambda (rel)
    (lambda (arg)
      ((set-builder
         (lambda (x) ((set-equal (op-2nd x)) arg))
         the-empty-set)
       rel))))

(define function?
  (lambda (rel)
    (or (empty-set? rel)
        (let ((subrel ((subrelation rel) (op-1st (pick rel)))))
          (and (= (cardinal subrel) 1)
               (function? (difference rel subrel)))))))

(define inverse-relation
  (lambda (rel)
    (if (empty-set? rel)
      the-empty-set
      (let ((subrel (pick rel)))
        (adjoin (make-op (op-2nd subrel) (op-1st subrel))
                (inverse-relation ((residue subrel) rel)))))))


(define one-to-one?
  (lambda (rel)
    (let ((reverse-rel (inverse-relation rel)))
      (function? reverse-rel))))

(define make-relation
  (lambda args
    (letrec ((helper
               (lambda (ls)
                 (if (null? ls) 
                   the-empty-set
                   (adjoin (make-op (car (car ls)) (cadr (car ls)))
                             (helper (cdr ls)))))))
      (helper args))))

(define relation-compose
  (lambda (Q R)
    (let ((z (intersection (domain Q) (range R))))
      ;(display z)(newline)
      (letrec ((helper
                 (lambda (zset)
                   (if (null? zset) 
                     the-empty-set
                     (union
                       (cartesian-product
                         (domain ((subrelation2 Q) (car zset)))
                         (range ((subrelation R) (car zset))))
                       (helper (cdr zset)))))))
        (helper z)))))

(define transitive?
  (lambda (s)
    (display (relation-compose s s))(newline)
    ((subset (relation-compose s s)) s)))

(define main
  (lambda ()
    (display (transitive? 
               (make-relation '(1 1) '(1 2) '(3 2) '(2 1)))) (newline)
    (display (union (make-set 1 2 3) (make-set 'a 'b 'c)))(newline)
    (display (transitive? 
               (make-relation '(1 2) '(1 3) '(1 4) '(2 3) '(2 4) '(3 4))))(newline)
    (display (make-relation '(1 2) '(1 3) '(2 3)))(newline)
    (display old-rel)(newline)
    (display (inverse-relation old-rel))(newline)
    (display (one-to-one? old-rel))(newline)

    (display (domain old-rel))(newline)
    (display (range old-rel))(newline)
    (display ((subrelation old-rel) 'tom))(newline)
    (display ((set-equal (list->set '(a b a b)))
             (list->set '(b a a))))(newline)
    (display (difference (make-set 1 1 2 3 4)
                    (make-set 3 4 4 5 6 6))) (newline)
    (display (union (make-set 1 2 3 4)
                    (make-set 1 3 4 5)))(newline)
    (display (union2 (make-set 1 2 3 4)
                    (make-set 1 3 4 5)
                    (make-set 2 6)))(newline)
    (display ((select-by-cardinal 2)
              (make-set (make-set 'a) (make-set 'a 'b) (make-set 'a 'b 'c)
                        (make-set 'b 'c) (make-set 'b))))(newline)
    ))
