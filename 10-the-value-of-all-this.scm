;; (10. what is the value of all of this?)

;; an entry is a pair of lists whose first list is a set.
;; the two lists must be of equal length. i.e. a map/dict.
;; represents evaluation scope.

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name) (car values))
     (else
      (lookup-in-entry-help name
                            (cdr names)
                            (cdr values)
                            entry-f)))))

(let ((entry '((appetizer entree beverage) (food tastes good))))
  (assert
   (equal? (lookup-in-entry 'entree entry list) 'tastes)
   "looks up value by name in entry")
  (assert
   (equal? (lookup-in-entry 'dessert entry list) '(dessert))
   "falls back to supplied entry-f"))

;; a table (or environment) is a list of entries.
;; represents nested scopes.

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else
      (lookup-in-entry name
                       (car table)
                       (lambda (name)
                         (lookup-in-table name
                                          (cdr table)
                                          table-f)))))))

(let ((table
       '(((entree dessert)
          (spaghetti spumoni))
         ((appetizer entree beverage)
          (food tastes good)))))

  (assert
   (equal? (lookup-in-table 'entree table list) 'spaghetti)
   "looks up value starting from innermost entry (scope)"))

;; produces the correct action (or function) for each possible s-exp:

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

;; produces the correct action for each possible atom:

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e)      *const)
     ((eq? e #t)       *const)
     ((eq? e #f)       *const)
     ((eq? e 'cons)    *const)
     ((eq? e 'car)     *const)
     ((eq? e 'cdr)     *const)
     ((eq? e 'null?)   *const)
     ((eq? e 'eq?)     *const)
     ((eq? e 'atom?)   *const)
     ((eq? e 'zero?)   *const)
     ((eq? e 'add1)    *const)
     ((eq? e 'sub1)    *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

;; produces the correct action for a given list:

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application))))

;; home-brewed eval:
;; ("table" stands for evaluation context or environment)

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;; specific actions:

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(let ((e '(quote (car (quote (a b c)))))
      (v '(car (quote (a b c)))))
  (assert
   (equal? (value e) v)
   "sanity check: value"))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car ())))

;; time to lambda

(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

(let ((e '(lambda (x) (cons x y)))
      (table '(((y z ((8) 9)))))
      (v '(non-primitive ((((y z ((8) 9)))) (x) (cons x y)))))
  (assert
   (equal? (meaning e table) v)
   "*lambda works"))

(define table-of first)
(define formals-of second)
(define body-of third)

;; our take on cond:

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x 'else))
     (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

;; taking it for a spin:

(let ((e '(cond (coffee klatsch) (else party)))
      (table '(((coffee) (#t)) ((klatsch party) (5 6)))))
  (assert
   (equal? (*cond e table) 5)
   "*cond works"))

;; list evaluator:

(define evlis
  (lambda (args table)
    (cond
     ((null? args) ())
     (else (cons (meaning (car args) table)
                 (evlis (cdr args) table))))))

;; application, i.e. function call:

(define *application
  (lambda (e table)
    (apply*
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

;; two representations of functions:
;;   primitive:     (primitive name)
;;   non-primitive: (non-primitive table formals body)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply*
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive (second fun) vals))
     ((non-primitive? fun)
      (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name 'cons) (cons (first vals) (second vals)))
     ((eq? name 'car) (car (first vals)))
     ((eq? name 'cdr) (cdr (first vals)))
     ((eq? name 'null?) (null? (first vals)))
     ((eq? name 'eq?) (eq? (first vals) (second vals)))
     ((eq? name 'atom?) (:atom? (first vals)))
     ((eq? name 'zero?) (zero? (first vals)))
     ((eq? name 'add1) (add1 (first vals)))
     ((eq? name 'sub1) (sub1 (first vals)))
     ((eq? name 'number?) (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) 'primitive) #t)
     ((eq? (car x) 'non-primitive) #t)
     (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning
     (body-of closure)
     (extend-table (build (formals-of closure) vals)
                   (table-of closure)))))

(let ((closure '((((x y) (5 (2))))   ; outer scope, x=1, y='(2)
                 (x)                 ; formals
                 (cons x y)))        ; body
      (vals '(1)))                   ; args, x=1 overrides outer x=5

  (assert
   (equal?
    (apply-closure closure vals)
    '(1 2))
   "apply-closure works"))

(let ((closure '((((u v w) (1 2 3))  ; inner scope
                  ((x y z) (4 5 6))) ; outer scope
                 (x y)               ; formals
                 (cons z x)))        ; body
      (vals '((a b c) (d e f))))     ; actual args

  (assert
   (equal?
    (apply-closure closure vals)
    '(6 a b c))
   "example from the book"))

;; now all together!

(assert
 (equal?
  (value
   '((lambda (x y)
       (cond
        (x (car y))
        (else z)))
     #t (quote (4 5 6))))
  4)
 "evaluates a lambda call")

(assert
 (equal?
  (meaning
   '((lambda (x y)
       (cond
        (x (car y))
        (else z)))
     #f (quote (4 5 6)))
   '(((z) (0))))
  0)
 "now with an outer scope")

;; intermission
