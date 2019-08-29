;; (7. friends and relations)

(define set?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((member? (car l) (cdr l)) #f)
     (else (set? (cdr l))))))

(let ((l ()))
  (assert
   (set? l)
   "empty list is a set"))

(let ((l '(apples peaches pears plums)))
  (assert
   (set? l)
   "list without duplicates is a set"))

(let ((l '(apple 3 pear 4 9 apple 3 4)))
  (assert
   (not (set? l))
   "list with duplicates is not a set"))

(define makeset
  (lambda (l)
    (cond
     ((null? l) l)
     (else
      (cons (car l) (makeset (multirember (car l) (cdr l))))))))

(let ((l '(apple peach pear peach plum apple lemon peach))
      (s '(apple peach pear plum lemon)))
  (assert
   (equal? (makeset l) s)
   "removes duplicates from a list"))

(let ((l '(apple 3 pear 4 9 apple 3 4))
      (s '(apple 3 pear 4 9)))
  (assert
   (equal? (makeset l) s)
   "removes duplicates from a list"))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

(let ((set1 '(5 chicken wings))
      (set2 '(5 hamburgers 2 pieces fried chicken and light duckling wings)))
  (assert
   (subset? set1 set2)))

(let ((set1 '(4 pounds of horseradish))
      (set2 '(four pounds chicken and 5 ounces horseradish)))
  (assert
   (not (subset? set1 set2))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(let ((set1 '(6 large chickens with wings))
      (set2 '(6 chickens with large wings)))
  (assert
   (eqset? set1 set2)
   "sets are equal if they're subsets of each other"))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(let ((set1 '(stewed tomatoes and macaroni))
      (set2 '(macaroni and cheese)))
  (assert
   (intersect? set1 set2)
   "two sets have elements in common"))

(let ((set1 '(stewed tomatoes and macaroni))
      (set2 '(cheese)))
  (assert
   (not (intersect? set1 set2))
   "two sets have no elements in common"))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) set1)
     ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(let ((set1 '(stewed tomatoes and macaroni))
      (set2 '(macaroni and cheese))
      (res  '(and macaroni)))
  (assert
   (equal? (intersect set1 set2) res)))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (union (cdr set1) set2))
     (else (cons (car set1) (union (cdr set1) set2))))))

(let ((set1 '(stewed tomatoes and macaroni casserole))
      (set2 '(macaroni and cheese))
      (res  '(stewed tomatoes casserole macaroni and cheese)))
  (assert
   (equal? (union set1 set2) res)))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(let ((l-set '((6 pears and)
               (3 peaches and 6 peppers)
               (8 pears and 6 plums)
               (and 6 prunes with some apples))))
  (assert
   (equal? (intersectall l-set) '(6 and))))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(assert
 (a-pair? '((2) (pair))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

;; a finite function (fun) is a list of pairs in which no first
;; element of any pair is repeated. think discrete (co)domain.

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) rel)
     (else (cons (revpair (car rel))
                 (revrel  (cdr rel)))))))

(let ((rel '((a 8) (pie pumpkin) (sick got)))
      (rev '((8 a) (pumpkin pie) (got sick))))
  (assert
   (equal? (revrel rel) rev)
   "inverts relation"))

;; injective or 1-to-1 function preserves distinctness: it never maps
;; distinct elements of its domain to the same element of its codomain.

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(define injective? one-to-one?)

(let ((fun '((grape raisin)
             (plum prune)
             (stewed prune))))
  (assert
   (not (injective? fun))))

(let ((fun '((grape raisin)
             (plum prune)
             (stewed grape))))
  (assert
   (injective? fun)))
