;; (1. toys)

(assert
 (atom? 'atom)
 "atom is a string of characters beginning with the letter a")

(assert
 (atom? 'turkey)
 "turkey is a string of characters beginning with the letter a")

(assert
 (atom? 1492)
 "1492 is a string of digits")

(assert
 (atom? 'u)
 "u is a string of one character which is a letter")

(assert
 (atom? '*abc$)
 "*abc$ is a string characters beginning with a letter or special character other than a paren")

(assert
 (list? '(atom))
 "(atom) is an atom enclosed by parentheses")

(assert
 (list? '(atom turkey or))
 "(atom turkey or) is a collection of atoms enclosed by parentheses")

(assert
 (list? '((atom turkey) or))
 "two s-expressions enclosed by parentheses")

(assert
 (s-exp? 'xyz)
 "all atoms are s-expressions")

(assert
 (s-exp? '(x y z))
 "all lists are s-expressions")

(assert
 (s-exp? '((x y) z))
 "all lists are s-expressions")

(assert
 (list? '(how are you doing so far))
 "list is a collection of s-expressions enclosed by parentheses")

;; todo: count s-exps? p. 4

(assert
 (list? ())
 "empty list is a list because it contains zero s-expressions enclosed by parentheses")

(assert
 (not (atom? ()))
 "empty list is not an atom")

(assert
 (list? '(() () () ()))
 "list of empty lists is a list")

(let ((l '(a b c)))
  (assert
   (equal? (car l) 'a)
   "a is the first atom of the list"))

(let ((l '((a b c) x y z)))
  (assert
   (equal? (car l) '(a b c))
   "(a b c) is the first s-expressions of a non-empty list"))

;; todo: assert-error? (car 'hotdog) i (car ()) p. 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the law of car: the primitive car is defined only for non-empty lists
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((l '(((hotdogs)) (and) (pickle) relish)))
  (assert
   (equal? (car l) '((hotdogs)))
   "((hotdogs)) is the first s-expressions of l"))

(let ((l '(((hotdogs)) (and))))
  (assert
   (equal? (car (car l)) '(hotdogs))
   "(hotdogs) is the first s-expressions of l's first s-expression"))

(let ((l '(a b c)))
  (assert
   (equal? (cdr l) '(b c))
   "(b c) is the list l without (car l)"))

(let ((l '((a b c) x y z)))
  (assert
   (equal? (cdr l) '(x y z))))

(let ((l '(hamburger)))
  (assert
   (equal? (cdr l) ())))

;; todo: assert-error? (cdr 'hotdog) i (cdr ()) p. 6-7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the law of cdr: the primitive cdr is defined only for non-empty lists.
;; the cdr of any non-empty list is always another list.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((l '((b) (x y) ((c)))))
  (assert
   (equal? (car (cdr l)) '(x y))
   "remember, car returns the leading s-exp"))

(let ((l '((b) (x y) ((c)))))
  (assert
   (equal? (cdr (cdr l)) '(((c))))
   "remember, cdr always returns a list"))

(let ((a 'peanut)
      (l '(butter and jelly)))
  (assert
   (equal? (cons a l) '(peanut butter and jelly))
   "cons adds an atom to the front of a list"))

(let ((s '(banana and))
      (l '(peanut butter and jelly)))
  (assert
   (equal? (cons s l) '((banana and) peanut butter and jelly))
   "cons adds any s-expression to the front of a list"))

(let ((s '((help) this))
      (l '(is very ((hard) to learn))))
  (assert
   (equal? (cons s l) '(((help) this) is very ((hard) to learn)))))

(let ((s '(a b (c)))
      (l '()))
  (assert
   (equal? (cons s l) '((a b (c))))))

(let ((s 'a)
      (l '()))
  (assert
   (equal? (cons s l) '(a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the law of cons: the primitive cons takes two arguments. the second
;; argument to cons must be a list. the result is a list.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((s 'a)
      (l '((b) c d)))
  (assert
   (equal? (cons s (car l)) '(a b)))
  (assert
   (equal? (cons s (cdr l)) '(a c d))))

(assert
 (null? (quote ()))
 "(quote ()) is a notation for the null list")

(assert
 (null? '())
 "'() is a notation for the null list")

(assert
 (null? ())
 "() is also a notation for the null list")

(let ((l '(a b c)))
  (assert
   (not (null? l))
   "l is a non-empty list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the law of null?: the primitive null is defined only for lists.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((s 'Harry))
  (assert
   (atom? s)
   "s is an atom"))

(let ((l '(Harry had a heap of apples)))
  (assert
   (not (atom? l))
   "list is not an atom")

  (assert
   (atom? (car l)))

  (assert
   (not (atom? (cdr l)))))

(let ((l '(Harry)))
  (assert
   (not (atom? (cdr l)))
   "empty list is not an atom"))

(let ((l '(swing low sweet cherry oat)))
  (assert
   (atom? (car (cdr l)))
   "low is an atom"))

(let ((l '(swing (low sweet) cherry oat)))
  (assert
   (not (atom? (car (cdr l))))
   "(low sweet) is not an atom"))

(let ((a1 'Harry)
      (a2 'Harry))
  (assert
   (eq? a1 a2)
   "a1 and a2 are the same non-numeric atom"))

(let ((a1 'margarine)
      (a2 'butter))
  (assert
   (not (eq? a1 a2))
   "a1 and a2 are different atoms"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the law of eq?: the primitive eq takes two arguments.
;; each must be a non-numeric atom.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((a 'Mary)
      (l '(Mary had a little lamb chop)))
  (assert
   (eq? a (car l))))

(let ((l '(beans beans we need jelly beans)))
  (assert
   (eq? (car l) (car (cdr l)))))
