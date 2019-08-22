;; (2. do it, do it again, and again, and again...)

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(let ((l '(Jack Sprat could eat no chicken fat)))
  (assert
   (lat? l)
   "l is a list of atoms"))

(let ((l '((Jack) Sprat could eat no chicken fat)))
  (assert
   (not (lat? l))
   "l is not a lat, since (car l) is a list"))

(let ((l '(Jack (Sprat could) eat no chicken fat)))
  (assert
   (not (lat? l))
   "l is not a lat, since one of the s-expressions in l is a list"))

(let ((l ()))
  (assert
   (lat? l)
   "l is a lat, since l does not contain a list"))

(let ((l1 '())
      (l2 '(d e f g)))
  (assert
   (or (null? l1) (atom? l2))))

(let ((l1 '(a b c))
      (l2 '()))
  (assert
   (or (null? l1) (null? l2))))

(let ((l1 '(a b c))
      (l2 '(atom)))
  (assert
   (not (or (null? l1) (null? l2)))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(let ((a 'poached)
      (lat '(fried eggs and scrambled eggs)))
  (assert
   (not (member? a lat))))

(let ((a 'meat)
      (lat '(mashed potatoes and meat gravy)))
  (assert
   (member? a lat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the first commandment (preliminary)
;;
;; always ask null? as the first question in expressing any function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
