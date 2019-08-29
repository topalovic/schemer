;; (8. lambda the ultimate)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) ())
       ((test? (car l) a) (cdr l))
       (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(let ((l '(lemonade (pop corn) and (cake)))
      (a '(pop corn))
      (res '(lemonade and (cake))))
  (assert
   (equal? ((rember-f equal?) a l) res)
   "removes elements from l that satisfy ((rember-f predicate) element l)"))

(let ((l '(equal? eq? eqan? eqlist? eqpair?))
      (a 'eq?)
      (res '(equal? eqan? eqlist? eqpair?)))
  (assert
   (equal? ((rember-f eq?) a l) res)))

(define insert-g
  (lambda (f)
    (lambda (new old l)
      (cond
       ((null? l) ())
       ((eq? (car l) old) (f new old (cdr l)))
       (else (cons (car l) ((insert-g f) new old (cdr l))))))))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

(assert
 (equal? (insertR 'd 'a '(a b a c)) '(a d b a c)))

(define subst
  (insert-g
   (lambda (new old l)
     (cons new l))))

(assert
 (equal? (subst 'd 'a '(a b a c)) '(d b a c)))

(define rember
  (lambda (a l)                                  ; adapter, reduces 3-arg fn to 2-arg fn
    ((insert-g (lambda (new old l) l)) #f a l))) ; #f is a dummy, can be anything

(assert
 (equal? (rember 'a '(c b a)) '(c b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the ninth commandment
;;
;; abstract common patterns with a new function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; partitions lat into seen (collection of a's) and newlat (the rest),
;; then applies (col newlat seen). better name would be partition&apply.

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col () ())) ; triggers unpacking of nested lambdas backwards
     ((eq? (car lat) a)
      (multirember&co a (cdr lat) (lambda (newlat seen) ; closure that passes both lists to the next level
                                    (col newlat (cons (car lat) seen)))))
     (else
      (multirember&co a (cdr lat) (lambda (newlat seen)
                                    (col (cons (car lat) newlat) seen)))))))

(let ((lat '(a b a c a))
      (newlat '((b c) (a a a))))
  (assert
   (equal? (multirember&co 'a lat list) newlat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the tenth commandment
;;
;; build functions to collect more than one value at a time.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; inserts new to the left of oldL and to the right of oldR in lat,
;; then applies (col newlat L R), where L and R are the numbers of
;; left and right insertions.

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col () 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons new (cons oldL newlat))
                               (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons oldR (cons new newlat))
                               L (add1 R)))))
     (else
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons (car lat) newlat)
                               L R)))))))

(let ((lat    '(<-- _ --> _ -->))
      (newlat '(x <-- _ --> x _ --> x))
      (yield  (lambda (lat L R) lat))
      (lcnt   (lambda (lat L R) L))
      (rcnt   (lambda (lat L R) R)))

  (assert
   (equal? (multiinsertLR&co 'x '<-- '--> lat yield) newlat)
   "x marks the spot")

  (assert
   (equal? (multiinsertLR&co 'x '<-- '--> lat lcnt) 1)
   "counts left insertions")

  (assert
   (equal? (multiinsertLR&co 'x '<-- '--> lat rcnt) 2)
   "counts right insertions"))

;; removes odd numbers from l.

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) ())
     ((atom? (car l))
      (cond
       ((even? (car l))
        (cons (car l)
              (evens-only* (cdr l))))
       (else
        (evens-only* (cdr l)))))
     (else
      (cons (evens-only* (car l))
            (evens-only* (cdr l)))))))

(let ((l '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
      (evens '((2 8) 10 (() 6) 2)))
  (assert
   (equal? (evens-only* l) evens)))

;; ditto, with a collector that multiplies evens, and sums odds.

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l)
      (col () 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col (cons (car l) newl) (* (car l) p) s))))
       (else
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col newl p (+ (car l) s)))))))
     (else
      (evens-only*&co (car l)
                      (lambda (al ap as)
                        (evens-only*&co (cdr l)
                                        (lambda (dl dp ds)
                                          (col (cons al dl)
                                               (* ap dp)
                                               (+ as ds))))))))))

(let ((l      '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
      (newl   '((2 8) 10 (() 6) 2))
      (yield   (lambda (e p s) e))
      (product (lambda (e p s) p))
      (sum     (lambda (e p s) s)))

  (assert
   (equal? (evens-only*&co l yield) newl)
   "yields evens")

  (assert
   (equal? (evens-only*&co l product) 1920)
   "multiplies evens")

  (assert
   (equal? (evens-only*&co l sum) 38)
   "sums odds"))
