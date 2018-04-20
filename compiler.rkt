#lang racket/base
(require "grammar/ast.rkt"
         "grammar/yacc.rkt"
         math/array
         racket/match
         )

(define compile-statement 
  (match-lambda
    [(definition name value)
     #`(define #,name #, (compile-expr value))]
    [(func-definition name lst value)
     #`(define(#,name #,@lst)#,@(compile-expr-for-define-func value))]
    [(box-definition name value)
     #`(define #,name (box #,(compile-expr value)))]
    [(assign id value)
     #`(set! #,id #,(compile-expr value))]
    [(box-assign id value)
     #`(set-box! #,id #,(compile-expr value))]
    [(when-statement test block)
     #`(when #,(compile-expr test)
         #,@(map compile-statement block))]
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

(define (compile-expr-for-define-func x)
  (match x
    [(block-expr block value)
     #`(#,@(map compile-statement block) #,(compile-expr value))]
    [else (compile-expr x)]))
(define compile-expr
   (match-lambda
         [(constant value)
             value]
         [(variable sym)
     (define srcstx (datum->syntax #f 'this-scope))
     (datum->syntax #f sym srcstx)]
    [(operation sym lst)
     #`(#,sym #,@(map compile-expr lst))]
    [(call expr lst)
     #`(#,(compile-expr expr) #,@(map compile-expr lst))]
    [(sub-extract expr lst)
     #`(list-ref #,expr #,(car lst))]
    [(seq f s e)
     #`(let([fr #,(compile-expr f)])
        (:: fr #,(compile-expr e) (- #,(compile-expr s) fr)))]
    [(array-expr lst)
     #`(list->array  #,(map compile-expr lst))]
    [(unbox-expr value)
     #`(unbox #,(compile-expr value))]
    [(block-expr block value)
     #`(begin #,@(map compile-statement block) #,(compile-expr value))]
    [(binding-block-expr bindings block value)
     ;(displayln bindings)
     (define(compiler-binding binding)
      ; (displayln binding)
       #`(#,(car binding)#,(compile-expr (cdr binding))))
     #`(let #,(map compiler-binding bindings)
         #,@(map compile-statement block)
         #,(compile-expr value))]
    [(if-expr test avalue bvalue)
     #`(if #,(compile-expr test) #,(compile-expr avalue) #,(compile-expr bvalue))]
    [(hash-expr name index else)
     #`(hash-ref #,(compile-expr name) #,(compile-expr index) (lambda()#,(compile-expr else)))]
    [else #f]))

(define(compile-program lst)
  (map compile-statement lst))
(define(expr-expander x)
  (compile-expr(parse-expr (open-input-string x))))

(provide expr-expander
         compile-expr
         compile-program
         compile-statement)
(module+ test
  (syntax->datum(expr-expander "(d1-> 1/(1-y*d/(n*n+n))){u1:=u*(d1-1);sum1:=sum+u1;}=>loop(sum1,n+1,u1,d1)"))
  (expr-expander "{g(x):={&z:=8;&z=$z+1;t:=$z;}=>t*x;}=>g(6)"))
;(syntax-debug-info #'expr-expander)
;(compile-expr(parse-expr (open-input-string "{a=a+1;cond x>0:{&z=2;} x>1: {z=2;} else {z=1;}}=>x+a+z")))