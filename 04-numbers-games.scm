;; (4. numbers games)

(assert
 (atom? 14)
 "all numbers are atoms")

(define add1
  (lambda (n)
    (+ n 1)))

(assert
 (equal? (add1 67) 68)
 "adds one to its argument")

(define sub1
  (lambda (n)
    (- n 1)))

(assert
 (equal? (sub1 5) 4)
 "subtracts one from its argument")

(assert
 (zero? 0))

(assert
 (not (zero? 1492)))

(define add
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (add n (sub1 m)))))))

(assert
 (equal? (add 46 12) 58)
 "adds its two numeric arguments")

(define sub
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (sub n (sub1 m)))))))

(assert
 (equal? (sub 14 3) 11)
 "subtracts its two numeric arguments")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the first commandment (first revision)
;;
;; when recurring on a list of atoms, lat, ask two questions about it:
;; (null? lat) and else.
;;
;; when recurring on a number, n, ask two questions about it:
;; (zero? n) and else.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (add (car tup) (addtup (cdr tup)))))))

(assert
 (equal? (addtup '(3 5 2 8)) 18)
 "adds all numbers of a given tuple")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the fourth commandment (first revision)
;;
;; always change at least one argument while recurring. it must be
;; changed to be closer to termination. the changing argument must be
;; tested in the termination condition:
;;
;; when using cdr, test termination with null?
;; when using sub1, test termination with zero?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mul
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (add n (mul n (sub1 m)))))))

(assert
 (equal? (mul 13 4) 52)
 "multiplies its two numeric arguments")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the fifth commandment
;;
;; when building a value with add, always use 0 for the value of the
;; terminating line, for adding 0 does not change the value of an
;; addition.
;;
;; when building a value with mul, always use 1 for the value of the
;; terminating line, for multiplying by 1 does not change the value of
;; a multiplication.
;;
;; when building a value with cons, always consider () for the value
;; of the terminating line.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (add (car tup1) (car tup2))
            (tup+ (cdr tup1) (cdr tup2)))))))

(let ((tup1 '(3 7))
      (tup2 '(4 6 8 1)))
  (assert
   (equal? (tup+ tup1 tup2) '(7 13 8 1))
   "adds individual elements of two given tuples"))

(define gt
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (gt (sub1 n) (sub1 m))))))

(assert
 (gt 6 4)
 "returns #t if first arg is greater than the second arg")

(define lt
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (lt (sub1 n) (sub1 m))))))

(assert
 (lt 4 6)
 "returns #t if first arg is less than the second arg")

(define ==
  (lambda (n m)
    (cond
     ((gt n m) #f)
     ((lt n m) #f)
     (else #t))))

(assert
 (== 4 4)
 "returns #t if two arguments are equal")

(assert
 (not (== 4 6))
 "returns #f if two arguments are not equal")

(define pow
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (mul n (pow n (sub1 m)))))))

(assert
 (== (pow 5 0) 1)
 "raises n to the power of m")

(assert
 (== (pow 5 3) 125)
 "raises n to the power of m")

(define div
  (lambda (n m)
    (cond
     ((lt n m) 0)
     (else (add1 (div (sub n m) m))))))

(assert
 (== (div 15 4) 3)
 "divides n by m")

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(let ((lat '(ham and cheese on rye)))
  (assert
   (== (length lat) 5)
   "returns length of given lat"))

(define pick
  (lambda (n lat)
    (cond
     ((== n 1) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(let ((lat '(ham and cheese on rye)))
  (assert
   (equal? (pick 3 lat) 'cheese)
   "picks nth element of given lat"))

(define rempick
  (lambda (n lat)
    (cond
     ((== n 1) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(let ((lat '(hotdogs with hot mustard)))
  (assert
   (equal? (rempick 3 lat) '(hotdogs with mustard))
   "removes nth element of given lat"))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) ())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat) (no-nums (cdr lat)))))))

(let ((lat '(5 pears 6 prunes 9 dates)))
  (assert
   (equal? (no-nums lat) '(pears prunes dates))
   "removes numeric elements from given lat"))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) ())
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(let ((lat '(5 pears 6 prunes 9 dates)))
  (assert
   (equal? (all-nums lat) '(5 6 9))
   "extracts numeric elements from given lat"))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))

(assert
 (eqan? 5 5)
 "compares two atoms")

(assert
 (eqan? 'a 'a)
 "compares two atoms")

(assert
 (not (eqan? 'a 5))
 "compares two atoms")

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? (car lat) a) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(let ((lat '(foo bar foo baz foo)))
  (assert
   (== (occur 'foo lat) 3)
   "counts occurrences of specific atom in a given lat"))
