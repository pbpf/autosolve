#lang racket
(require "grammar/ast.rkt"
         "grammar/yacc.rkt"
         "ast.rkt"
         "name.rkt"
         math/array
         racket/match
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
(module+ test
(displayln(map compile-statement '(#s(assignment
     #s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
     #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
  #s(assignment #s(varsub #s(vecsub k 1) 1) #s(varsub vy 0))
  #s(assignment #s(varsub #s(vecsub k 2) 1) #s(varsub vx 0))
  #s(assignment
     #s(exprforname #s(virtualcall v (#s(varsub t 0) #s(varsub y 0) #s(varsub x 0) #s(varsub vx 0) #s(varsub vy 0))))
     #s(call
        sqrt
        (#s(operation
            +
            (#s(operation expt (#s(varsub vx 0) #s(constant 2))) #s(operation expt (#s(varsub vy 0) #s(constant 2))))))))
  #s(assignment
     #s(exprforname #s(virtualcall r (#s(varsub t 0) #s(varsub y 0) #s(varsub x 0) #s(varsub vx 0) #s(varsub vy 0))))
     #s(operation
        +
        (#s(call
            sqrt
            (#s(operation
                +
                (#s(operation expt (#s(varsub y 0) #s(constant 2))) #s(operation expt (#s(varsub x 0) #s(constant 2)))))))
         #s(exprforname #s(virtualcall v (#s(varsub t 0) #s(varsub y 0) #s(varsub x 0) #s(varsub vx 0) #s(varsub vy 0)))))))
  #s(assignment
     #s(varsub #s(vecsub k 3) 1)
     #s(operation
        *
        (#s(operation
            /
            (#s(operation - (#s(variable mu)))
             #s(operation
                expt
                (#s(exprforname
                    #s(virtualcall r (#s(varsub t 0) #s(varsub y 0) #s(varsub x 0) #s(varsub vx 0) #s(varsub vy 0))))
                 #s(constant 3)))))
         #s(varsub x 0))))
  #s(assignment
     #s(varsub #s(vecsub k 4) 1)
     #s(operation
        *
        (#s(operation
            /
            (#s(operation - (#s(variable mu)))
             #s(operation
                expt
                (#s(exprforname
                    #s(virtualcall r (#s(varsub t 0) #s(varsub y 0) #s(varsub x 0) #s(varsub vx 0) #s(varsub vy 0))))
                 #s(constant 3)))))
         #s(varsub y 0))))
  #s(assignment
     #s(varsub #s(vecsub k 1) 2)
     #s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 1))))
  #s(assignment
     #s(varsub #s(vecsub k 2) 2)
     #s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 1))))
  #s(assignment
     #s(exprforname
        #s(virtualcall
           v
           (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
            #s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 1)))
            #s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 1)))
            #s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 1)))
            #s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 1))))))
     #s(call
        sqrt
        (#s(operation
            +
            (#s(operation
                expt
                (#s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 1))) #s(constant 2)))
             #s(operation
                expt
                (#s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 1)))
                 #s(constant 2))))))))
  #s(assignment
     #s(exprforname
        #s(virtualcall
           r
           (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
            #s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 1)))
            #s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 1)))
            #s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 1)))
            #s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 1))))))
     #s(operation
        +
        (#s(call
            sqrt
            (#s(operation
                +
                (#s(operation
                    expt
                    (#s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 1)))
                     #s(constant 2)))
                 #s(operation
                    expt
                    (#s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 1)))
                     #s(constant 2)))))))
         #s(exprforname
            #s(virtualcall
               v
               (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
                #s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 1)))
                #s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 1)))
                #s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 1)))
                #s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 1)))))))))
  #s(assignment
     #s(varsub #s(vecsub k 3) 2)
     #s(operation
        *
        (#s(operation
            /
            (#s(operation - (#s(variable mu)))
             #s(operation
                expt
                (#s(exprforname
                    #s(virtualcall
                       r
                       (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
                        #s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 1)))
                        #s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 1)))
                        #s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 1)))
                        #s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 1))))))
                 #s(constant 3)))))
         #s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 1))))))
  #s(assignment
     #s(varsub #s(vecsub k 4) 2)
     #s(operation
        *
        (#s(operation
            /
            (#s(operation - (#s(variable mu)))
             #s(operation
                expt
                (#s(exprforname
                    #s(virtualcall
                       r
                       (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
                        #s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 1)))
                        #s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 1)))
                        #s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 1)))
                        #s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 1))))))
                 #s(constant 3)))))
         #s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 1))))))
  #s(assignment
     #s(varsub #s(vecsub k 1) 3)
     #s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 2))))
  #s(assignment
     #s(varsub #s(vecsub k 2) 3)
     #s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 2))))
  #s(assignment
     #s(exprforname
        #s(virtualcall
           v
           (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
            #s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 2)))
            #s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 2)))
            #s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 2)))
            #s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 2))))))
     #s(call
        sqrt
        (#s(operation
            +
            (#s(operation
                expt
                (#s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 2))) #s(constant 2)))
             #s(operation
                expt
                (#s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 2)))
                 #s(constant 2))))))))
  #s(assignment
     #s(exprforname
        #s(virtualcall
           r
           (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
            #s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 2)))
            #s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 2)))
            #s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 2)))
            #s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 2))))))
     #s(operation
        +
        (#s(call
            sqrt
            (#s(operation
                +
                (#s(operation
                    expt
                    (#s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 2)))
                     #s(constant 2)))
                 #s(operation
                    expt
                    (#s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 2)))
                     #s(constant 2)))))))
         #s(exprforname
            #s(virtualcall
               v
               (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
                #s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 2)))
                #s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 2)))
                #s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 2)))
                #s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 2)))))))))
  #s(assignment
     #s(varsub #s(vecsub k 3) 3)
     #s(operation
        *
        (#s(operation
            /
            (#s(operation - (#s(variable mu)))
             #s(operation
                expt
                (#s(exprforname
                    #s(virtualcall
                       r
                       (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
                        #s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 2)))
                        #s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 2)))
                        #s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 2)))
                        #s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 2))))))
                 #s(constant 3)))))
         #s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 2))))))
  #s(assignment
     #s(varsub #s(vecsub k 4) 3)
     #s(operation
        *
        (#s(operation
            /
            (#s(operation - (#s(variable mu)))
             #s(operation
                expt
                (#s(exprforname
                    #s(virtualcall
                       r
                       (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
                        #s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 2)))
                        #s(add #s(varsub x 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 2)))
                        #s(add #s(varsub vx 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 3) 2)))
                        #s(add #s(varsub vy 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 4) 2))))))
                 #s(constant 3)))))
         #s(add #s(varsub y 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 1) 2))))))
  #s(assignment #s(varsub #s(vecsub k 1) 4) #s(add #s(varsub vy 0) h))
  #s(assignment #s(varsub #s(vecsub k 2) 4) #s(add #s(varsub vx 0) h))
  #s(assignment
     #s(exprforname
        #s(virtualcall
           v
           (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
            #s(add #s(varsub y 0) h)
            #s(add #s(varsub x 0) h)
            #s(add #s(varsub vx 0) h)
            #s(add #s(varsub vy 0) h))))
     #s(call
        sqrt
        (#s(operation
            +
            (#s(operation expt (#s(add #s(varsub vx 0) h) #s(constant 2)))
             #s(operation expt (#s(add #s(varsub vy 0) h) #s(constant 2))))))))
  #s(assignment
     #s(exprforname
        #s(virtualcall
           r
           (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
            #s(add #s(varsub y 0) h)
            #s(add #s(varsub x 0) h)
            #s(add #s(varsub vx 0) h)
            #s(add #s(varsub vy 0) h))))
     #s(operation
        +
        (#s(call
            sqrt
            (#s(operation
                +
                (#s(operation expt (#s(add #s(varsub y 0) h) #s(constant 2)))
                 #s(operation expt (#s(add #s(varsub x 0) h) #s(constant 2)))))))
         #s(exprforname
            #s(virtualcall
               v
               (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
                #s(add #s(varsub y 0) h)
                #s(add #s(varsub x 0) h)
                #s(add #s(varsub vx 0) h)
                #s(add #s(varsub vy 0) h)))))))
  #s(assignment
     #s(varsub #s(vecsub k 3) 4)
     #s(operation
        *
        (#s(operation
            /
            (#s(operation - (#s(variable mu)))
             #s(operation
                expt
                (#s(exprforname
                    #s(virtualcall
                       r
                       (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
                        #s(add #s(varsub y 0) h)
                        #s(add #s(varsub x 0) h)
                        #s(add #s(varsub vx 0) h)
                        #s(add #s(varsub vy 0) h))))
                 #s(constant 3)))))
         #s(add #s(varsub x 0) h))))
  #s(assignment
     #s(varsub #s(vecsub k 4) 4)
     #s(operation
        *
        (#s(operation
            /
            (#s(operation - (#s(variable mu)))
             #s(operation
                expt
                (#s(exprforname
                    #s(virtualcall
                       r
                       (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
                        #s(add #s(varsub y 0) h)
                        #s(add #s(varsub x 0) h)
                        #s(add #s(varsub vx 0) h)
                        #s(add #s(varsub vy 0) h))))
                 #s(constant 3)))))
         #s(add #s(varsub y 0) h))))
  #s(assignment
     #s(varsub y 1)
     #s(add
        #s(varsub y 0)
        #s(times
           #s(exprforname #s(divide h 6))
           #s(addmany
              (#s(varsub #s(vecsub k 1) 1)
               #s(times 2 #s(add #s(varsub #s(vecsub k 1) 2) #s(varsub #s(vecsub k 1) 3)))
               #s(varsub #s(vecsub k 1) 4))))))
  #s(assignment
     #s(varsub x 1)
     #s(add
        #s(varsub x 0)
        #s(times
           #s(exprforname #s(divide h 6))
           #s(addmany
              (#s(varsub #s(vecsub k 2) 1)
               #s(times 2 #s(add #s(varsub #s(vecsub k 2) 2) #s(varsub #s(vecsub k 2) 3)))
               #s(varsub #s(vecsub k 2) 4))))))
  #s(assignment
     #s(varsub vx 1)
     #s(add
        #s(varsub vx 0)
        #s(times
           #s(exprforname #s(divide h 6))
           #s(addmany
              (#s(varsub #s(vecsub k 3) 1)
               #s(times 2 #s(add #s(varsub #s(vecsub k 3) 2) #s(varsub #s(vecsub k 3) 3)))
               #s(varsub #s(vecsub k 3) 4))))))
  #s(assignment
     #s(varsub vy 1)
     #s(add
        #s(varsub vy 0)
        #s(times
           #s(exprforname #s(divide h 6))
           #s(addmany
              (#s(varsub #s(vecsub k 4) 1)
               #s(times 2 #s(add #s(varsub #s(vecsub k 4) 2) #s(varsub #s(vecsub k 4) 3)))
               #s(varsub #s(vecsub k 4) 4)))))))
       )))
;(syntax-debug-info #'expr-expander)
;(compile-expr(parse-expr (open-input-string "{a=a+1;cond x>0:{&z=2;} x>1: {z=2;} else {z=1;}}=>x+a+z")))