;; (5. *oh my gawd*: it's full of stars)

(define rember*
  (lambda (a l)
    (cond
     ((null? l) ())
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
     ;; else: (car l) is s-exp
     (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(let ((a 'cup)
      (l '((coffee) cup ((tea) cup) (and (hick)) cup)))
  (assert
   (equal? (rember* a l) '((coffee) ((tea)) (and (hick))))
   "removes all occurrences of atom a from l"))

(let ((a 'sauce)
      (l '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))))
  (assert
   (equal? (rember* a l) '(((tomato)) ((bean)) (and ((flying)))))
   "removes all occurrences of atom a from l"))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) ())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
     (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(let ((new 'roast)
      (old 'chuck)
      (l '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood)))
  (assert
   (equal? (insertR* new old l) '((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood))
   "inserts atom new to the right of all occurrences of old in l"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the first commandment (final revision)
;;
;; when recurring on a list of atoms, lat, ask two questions about it:
;; (null? lat) and else.
;;
;; when recurring on a number, n, ask two questions about it:
;; (zero? n) and else.
;;
;; when recurring on a list of s-expressions, l, ask three questions
;; about it: (null? l), (atom? (car l)), and else.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the fourth commandment (final version)
;;
;; always change at least one argument while recurring.
;;
;; when recurring on a list of atoms, lat, use (cdr lat).
;;
;; when recurring on a number, n, use (sub1 n).
;;
;; when recurring on a list of s-expressions, l, use (car l) and
;; (cdr l) if neither (null? l) nor (atom? (car l)) are true.
;;
;; when using cdr, test termination with null?
;; when using sub1, test termination with zero?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else (add (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) ())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(let ((a 'banana)
      (old 'banana)
      (new 'orange)
      (l '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy))))

  (assert
   (equal? (occur* a l) 5)
   "counts all occurrences of atom a in l")

  (assert
   (equal? (subst* new old l) '((orange) (split ((((orange ice))) (cream (orange)) sherbet)) (orange) (bread) (orange brandy)))
   "replaces all occurrences of atom old with new in l"))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) ())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
     (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(let ((new 'pecker)
      (old 'chuck)
      (l '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood)))
  (assert
   (equal? (insertL* new old l) '((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood))
   "inserts atom new to the left of all occurrences of old in l"))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (eq? (car l) a) (member* a (cdr l))))
     (else (or (member* a (car l)) (member* a (cdr l)))))))

(let ((a 'chips)
      (l '((potato) (chips ((with) fish) (chips)))))
  (assert
   (member* a l)
   "tests for presence of atom a in l"))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(let ((l '(((hot) (tuna (and))) cheese)))
  (assert
   (equal? (leftmost l) 'hot)
   "finds the leftmost atom in a non-empty l"))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2)))
      (eqlist? (cdr l1) (cdr l2))))))

(let ((l1 '(beef ((sausage)) (and (soda))))
      (l2 '(beef ((sausage)) (and (soda)))))
  (assert
   (eqlist? l1 l2)
   "determines if two lists are equal"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the sixth commandment
;;
;; simplify only after the function is correct.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
