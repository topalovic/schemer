;; (9. ... and again, and again, and again, ...)

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sym-or-num lat)
    (cond
     ((number? sym-or-num)
      (keep-looking a (pick sym-or-num lat) lat))
     (else (eq? sym-or-num a)))))

(let ((lat '(6 2 4 caviar 5 7 3)))
  (assert
   (looking 'caviar lat)
   "indirect addressing finds caviar eventually"))

(let ((lat '(6 2 grits caviar 5 7 3)))
  (assert
   (not (looking 'caviar lat))
   "indirect addressing fails to find caviar"))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(let ((x '((a b) (c d)))
      (shifted '(a (b (c d)))))
  (assert
   (equal? (shift x) shifted)
   "shifts the 2nd part of the 1st component into the 2nd component"))

;; https://en.wikipedia.org/wiki/Collatz_conjecture

(define C
  (lambda (n)
    (cond
     ((equal? n 1) 1)
     ((even? n) (C (/ n 2)))
     (else (C (add1 (* 3 n)))))))

(assert
 (equal? (C 2) 1)
 "don't try with zero!")

;; https://en.wikipedia.org/wiki/Ackermann_function
;;
;; example of a total computable function that is not primitive recursive.

(define A
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else (A (sub1 n) (A n (sub1 m)))))))

(assert
 (equal? (A 2 2) 7)
 "don't try with anything beyond 2!")

;; partial function from A to B is a total function A' → B for some A' ⊆ A.
;; i.e. it may not cover the entire domain.
;;
;; total function is a partial function with A' = A.
;; i.e. it covers the entire domain.
;;
;; partial functions are often used when the exact domain is not known; e.g.
;; in computability theory, general recursive functions are partial functions
;; from integers to integers, and there cannot be any algorithm for deciding
;; whether such a function is total.

(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eternity x))))

;; or simplified:

(define f
  (lambda ()
    (and (halts? f)
         (loop))))

;; halts? cannot exist, proof by contradiction:
;;
;; assuming (halts? f) is #f:
;; "and" short circuits making f return #f, implying that (halts? f) is #t.
;;
;; assuming (halts? f) is #t:
;; "and" checks the 2nd condition making f fall into a loop, implying that (halts? f) is #f.
;;
;; see: https://en.wikipedia.org/wiki/Halting_problem#Proof_concept

;; and now for the main dish:
;; https://en.wikipedia.org/wiki/Fixed-point_combinator#Y_combinator

(define Y
  (lambda (f)
    ((lambda (g) (g g))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))

;; expanded symmetric form:

(define YY
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))

;; you supply f with its own termination condition and recur call:

(assert
 (equal?
  ((Y
    (lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l))))))))
   '(a b c d e)) 5)
 "does your hat still fit?")

;; a nice concise tutorial on deriving Y: https://ebzzry.com/en/y/
