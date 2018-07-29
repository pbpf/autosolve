#lang racket/base
(require "grammar/ast.rkt"
         "ast.rkt"
         "name.rkt"
         racket/match
         racket/string
         racket/list
         )

(provide compile-statements)
(define tb (box (hash)))
(define cb (box (hash)))
(define compile-name
  (match-lambda
    [(exprforname expr)
     (define-values(a b c)(pass-exprforname expr (unbox tb) (unbox cb)))
     (set-box! tb b)
     (set-box! cb c)
     a]
    [(varsub x y)
     (format "~a~a" (compile-name x) y)]
    [(vecsub x y)
     (format "~a_~a" (compile-name x) y)]
    [t (if(symbol? t) (format "~a" t) t)]))
(define optb (hash 'expt '^))
(define compile-value
  (match-lambda
    [(operation op lst)
     (if(> (length lst) 1)
        (format "(~a)" (string-join (map compile-value lst) (format "~a" (hash-ref optb op (lambda() op)))))
        (format "(~a~a)" op (compile-value (car lst))))]
    [(add x y)
     (format "(~a+~a)" (compile-value x)(compile-value y))]
    [(varsub x y)(compile-name (varsub x y))]
    [(exprforname a)(compile-name (exprforname a))]
    [(call id lst)(format "~a(~a)" id (string-join (map compile-value lst) ","))]
    [(constant x)(format "~a" x)]
    [(variable x)(format "~a" x)]
    [(times x y)(format "(~a*~a)" (compile-value x)(compile-value y))]
    [(addmany lst)(format "(~a)" (string-join (map compile-value lst)"+"))]
    [(divide x y)(format "(~a/~a)"(compile-value x)(compile-value y))]
    [else (format "~a" else)]))
(define compile-statement 
  (match-lambda
    [(assignment name value)
     (format "~a=~a;" (compile-name name) (compile-value value) )]
     
    ;optcall do not support
    
   ; [(func-definition name formlist value)
   ;  #`(define (#,name #,@formlist ) #,(compile-statement value))]
  ;  [(pair-expr car cdr)
  ;   #`(#,(compile-statement cdr) #,(compile-statement car))]
  ;  [(pair-exprlist lst else1)
   ;  #`(cond 
   ;      #,@(map compile-statement lst)
   ;      [else #,(compile-statement else1)])]
    
    [else (write else)#f];do not soport
   ))

(define(compile-statements lst)
  (string-join (remove-duplicates(map compile-statement lst))"\n"))
