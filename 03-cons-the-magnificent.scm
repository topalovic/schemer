;; (3. cons the magnificent)

;; takes an atom and a list of atoms; makes a new lat with the first
;; occurrence of the atom in the old lat removed.

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) ())
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat) (rember a (cdr lat)))))))

(let ((a 'mint)
      (lat '(lamb chops and mint jelly)))
  (assert
   (equal? (rember a lat) '(lamb chops and jelly))))

(let ((a 'mint)
      (lat '(lamb chops and mint flavored mint jelly)))
  (assert
   (equal? (rember a lat) '(lamb chops and flavored mint jelly))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the second commandment
;;
;; use cons to build lists.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a list which is either empty or contains only non-empty
;; lists; builds another list composed of the first s-expressions of
;; each internal list.

(define firsts
  (lambda (l)
    (cond
     ((null? l) ())
     (else (cons (car (car l)) (firsts (cdr l)))))))

(let ((l ()))
  (assert
   (equal? (firsts l) ())))

(let ((l '((a b) (c d) (e f))))
  (assert
   (equal? (firsts l) '(a c e))))

(let ((l '(((five plums) four) (eleven green oranges) ((no) more))))
  (assert
   (equal? (firsts l) '((five plums) eleven (no)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the third commandment
;;
;; when building a list, describe the first typical element, and then
;; cons it onto the natural recursion.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) ())
     ((eq? (car lat) old) (cons old (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(assert
 (equal? (insertR 'quux 'foo '(foo bar foo baz)) '(foo quux bar foo baz))
 "builds a lat with new inserted to the right of the first occurrence of old in lat")

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) ())
     ((eq? (car lat) old) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(assert
 (equal? (insertL 'quux 'foo '(foo bar foo baz)) '(quux foo bar foo baz))
 "builds a lat with new inserted to the left of the first occurrence of old in lat")

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) ())
     ((eq? (car lat) old) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(assert
 (equal? (subst 'quux 'foo '(foo bar foo baz)) '(quux bar foo baz))
 "replaces the first occurrence of old in lat by new")

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) ())
     ((or (eq? (car lat) o1) (eq? (car lat) o2))
      (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(assert
 (equal? (subst2 'quux 'bar 'baz '(foo bar foo baz)) '(foo quux foo baz))
 "replaces either the first occurrence of o1 or o2 in lat by new")

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) ())
     ((eq? (car lat) a) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(assert
 (equal? (multirember 'foo '(foo bar foo baz)) '(bar baz))
 "removes all occurrences of a in lat")

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) ())
     ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(assert
 (equal? (multiinsertR 'quux 'foo '(foo bar foo baz)) '(foo quux bar foo quux baz))
 "builds a lat with new inserted to the right of all occurrences of old in lat")

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) ())
     ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(assert
 (equal? (multiinsertL 'quux 'foo '(foo bar foo baz)) '(quux foo bar quux foo baz))
 "builds a lat with new inserted to the left of all occurrences of old in lat")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the fourth commandment (preliminary)
;;
;; always change at least one argument while recurring. it must be
;; changed to be closer to termination. the changing argument must be
;; tested in the termination condition: when using cdr, test
;; termination with null?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) ())
     ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))

(assert
 (equal? (multisubst 'quux 'foo '(foo bar foo baz)) '(quux bar quux baz))
 "replaces all occurrences of old in lat by new")
