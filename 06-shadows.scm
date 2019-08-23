;; (6. shadows)

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))                  ; both 1st...
           (numbered? (car (cdr (cdr aexp))))))))) ; and 3rd element are a-exps

(assert
 (numbered? 3)
 "number is numbered")

(assert
 (numbered? '(3 + (4 ^ 5)))
 "arithmetic expression is numbered")

(assert
 (not (numbered? '(2 * sausage)))
 "sausage is not a number")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the seventh commandment
;;
;; recur on the subparts that are of the same nature:
;;  - on the sublists of a list
;;  - on the subexpressions of an arithmetic expression
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; defined for infix notation
;; (1st op 2nd), e.g. (4 * 5)

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define value
  (lambda (nexp) ; numbered arithmetic expression
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) '+) (add (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) '*) (mul (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     (else (pow (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))))) ; assuming pow for all other operators

(assert
 (equal? (value 3) 3))

(assert
 (equal? (value '(4 * 5)) 20))

(assert
 (equal? (value '(3 + (4 ^ 5))) 1027))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the eighth commandment
;;
;; use help functions to abstract from representations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
