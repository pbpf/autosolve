#lang racket/base
(require  (for-syntax "compiler.rkt")
          (for-syntax racket/base)
         )

(define-syntax (expr: syntax-object)
  (syntax-case syntax-object ()
    ((_ expr-string)
     (with-syntax([x (datum->syntax syntax-object (syntax->datum(expr-expander(format "~a" (syntax->datum #'expr-string)))))])
       #'x))))

(provide expr:)
(module+ test
(expr: "(x->1,y->2)=>x+y")
(expr: "(x->3,y->2){z:=x*y;}=>x+y-z")
(define(f x)
    (expr: "sin(x)+1" ))
(expr: "{g(x):={&z:=8;&z=$z+1;t:=$z*7;}=>t*x;}=>g(6)")
(f 6)
(define err 1e-100)
(define(sf y)
  (let loop([sum y][n 1][u y][d 1])
    (if(< u 1e-100)
       sum
       (expr: "(d1-> 1/(1-y*d/(n*n+2*n))){u1:=u*(d1-1);sum1:=sum+u1;}=>loop(sum1,n+2,u1,d1)")
       )))
(define(sfa a x)
  (/(sf (* a ((lambda(x)(* x x)) (* 0.5 x))))
    (* a 0.5 x)))

(define(solve-sfd)
  (let loop ([a 1.442143234][b 1.45])
   
       
    (define r (/ (+ a b) 2))
     (if(< (- a b) err)
        r
        (if(= (sf r) 0)
           (loop a r)
           (loop r b)))))

(define(fn n)
  (if (odd? n)
      (expr: "(n+2)*(n+5)/((2*n+1)*(2*n+3))")
      (expr: "n*(n-3)/((2*n+1)*(2*n+3))")))
  )

;(require plot)

;(plot (function sf 2.467401 2.467402));2.46740110027-2.46740110028