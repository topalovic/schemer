;; (preface)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define s-exp?
  (lambda (x)
    (or (atom? x) (list? x))))

(assert
 (not (atom? (quote())))
 "compliance check: empty list is not an atom")
