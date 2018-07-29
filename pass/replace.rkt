#lang racket/base
 (require "../grammar/ast.rkt"
          "../ast.rkt"
          racket/match
          racket/list)
(provide pass)
;match and replace
;匹配的对象主要时状态变量，方程

;----------------第一步---------------------
;将模板中的变量换成微分方程变量 已完成

;----------------第二步---------------------
;将模板中的函数调用替换成具体的形式
;(replace 微分方程right形式 table(a->a0,....))
(define(make-replace-table a b)
  (for/hash ([i (in-list a)]
             [j (in-list b)])
    (values i j)))
(define(replace-callitem replace-table eq)
  ;(displayln replace-table)
 ; (displayln eq)
  ((varreplacerules2 replace-table) eq))
;(replace-callitem #hash((vy . #s(varsub vy 0)) (x . #s(varsub x 0)) (t . #s(varsub t 0)) (y . #s(varsub y 0)) (vx . #s(varsub vx 0)))
;#s(operation * (#s(operation / (#s(operation - (#s(variable mu))) #s(operation expt (#s(virtualcall r (t y x vx vy)) #s(constant 3))))) #s(variable x))))
(define(replace-call vars normalizationeq-sorted)
  (match-lambda
     [(assignment name value)
      (if(and (funcall? value)(difffuncsub? (funcall-f value)))
         (assignment name (replace-callitem (make-replace-table vars
                                                               (funcall-explist value))
                                            (diffequation-value (list-ref normalizationeq-sorted (-(difffuncsub-step (funcall-f value))1)))))
         (assignment name (list value '())))]
     [else else]))

(define(pass1 vcallexpr vcalldef-table)
  (define v1 (hash-ref vcalldef-table (virtualcall-id vcallexpr)))
  (vfundefreplace (virtualcall-sublist vcallexpr) v1))
(define(replace-vcall vcalldef-table)
  (match-lambda
     [(assignment name value)
      (if(null?(cadr value))
         (list (assignment name (car value)))
         `(,@(map (lambda(x)(pass1 x vcalldef-table))
               (cadr value))
               ,(assignment name (car value))))]
     [else (list else)]))

(define(replace-vcall2 vcalldef-table)
  (match-lambda
     [(vcallassignment name value)
      (if(null?(cadr value))
         (list (assignment name (car value)))
         `(,@(map (lambda(x)(pass1 x vcalldef-table))
               (cadr value))
               ,(assignment name (car value))))]
     [else (list else)]))
  
;(define(replace-vcall
  
(define(replace1 modole vars normalizationeq-sorted)
  (map (replace-call vars normalizationeq-sorted) modole))
(define(replace2 modole1  vcalldef-table)
  (remove-duplicates (apply append (map (replace-vcall vcalldef-table) modole1))))
(define(replace3 modole2 vcalldef-table)
  (if(empty? (filter vcallassignment? modole2))
     modole2
     (replace3 (apply append(map (replace-vcall2 vcalldef-table) modole2)) vcalldef-table)))

(define(pass modole vars  normalizationeq-sorted vcalldef-table)
  (replace3(replace2(replace1 modole vars normalizationeq-sorted)vcalldef-table)vcalldef-table))
  


;---------------------------------
;codegen
;pass exprforname

(module+ test
;--------------------------------
(require "../methods/autork4.rkt"
         )
(replace3
(replace2
(replace1
 (gen-normal* '(y x vx vy) 't 'h 'k)
 '(t y x vx vy)
 '(#s(diffequation y t #s(variable vy))
  #s(diffequation x t #s(variable vx))
  #s(diffequation
     vx
     t
     #s(operation
        *
        (#s(operation
            /
            (#s(operation - (#s(variable mu))) #s(operation expt (#s(virtualcall r (t y x vx vy)) #s(constant 3)))))
         #s(variable x))))
  #s(diffequation
     vy
     t
     #s(operation
        *
        (#s(operation
            /
            (#s(operation - (#s(variable mu))) #s(operation expt (#s(virtualcall r (t y x vx vy)) #s(constant 3)))))
         #s(variable y))))))
'#hash((v
        .
        #s(virtual-func-definition
           v
           (t x y vx vy)
           #s(call
              sqrt
              (#s(operation
                  +
                  (#s(operation expt (#s(variable vx) #s(constant 2)))
                   #s(operation expt (#s(variable vy) #s(constant 2)))))))))
       (r
        .
        #s(virtual-func-definition
           r
           (t x y vx vy)
           #s(operation
              +
              (#s(call
                  sqrt
                  (#s(operation
                      +
                      (#s(operation expt (#s(variable x) #s(constant 2)))
                       #s(operation expt (#s(variable y) #s(constant 2)))))))
               #s(virtualcall v (t x y vx vy))))))))
'#hash((v
        .
        #s(virtual-func-definition
           v
           (t x y vx vy)
           #s(call
              sqrt
              (#s(operation
                  +
                  (#s(operation expt (#s(variable vx) #s(constant 2)))
                   #s(operation expt (#s(variable vy) #s(constant 2)))))))))
       (r
        .
        #s(virtual-func-definition
           r
           (t x y vx vy)
           #s(operation
              +
              (#s(call
                  sqrt
                  (#s(operation
                      +
                      (#s(operation expt (#s(variable x) #s(constant 2)))
                       #s(operation expt (#s(variable y) #s(constant 2)))))))
               #s(virtualcall v (t x y vx vy)))))))))
#|

(define f (replace-call '(#s(diffequation y t #s(variable vy))
  #s(diffequation x t #s(variable vx))
  #s(diffequation
     vx
     t
     #s(operation
        *
        (#s(operation
            /
            (#s(operation - (#s(variable mu))) #s(operation expt (#s(virtualcall r (t y x vx vy)) #s(constant 3)))))
         #s(variable x))))
  #s(diffequation
     vy
     t
     #s(operation
        *
        (#s(operation
            /
            (#s(operation - (#s(variable mu))) #s(operation expt (#s(virtualcall r (t y x vx vy)) #s(constant 3)))))
         #s(variable y)))))))
(f   #s(assignment
     #s(varsub #s(vecsub k 2) 2)
     #s(funcall
        #s(difffuncsub 2)
        (#s(exprforname #s(add #s(varsub t 0) #s(exprforname #s(divide h 2))))
         #s(add #s(varsub b 0) #s(times #s(exprforname #s(divide h 2)) #s(varsub #s(vecsub k 2) 1)))))))
|#