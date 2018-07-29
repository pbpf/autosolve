#lang racket/base
(require "../ast.rkt"
         racket/match
         racket/list)
(provide (all-defined-out)
         (struct-out diffequation))

;parser
;(
;(define-struct exprbase (variable-list)#:prefab)
(define-struct constant  (value)#:prefab);
(define-struct definition (id value)#:prefab);也视作函数定义 变量隐含在 value 中
;(define-struct box-definition (id value)#:prefab);
;(define-struct box-assign (id value)#:prefab);
;(define-struct unbox-expr(value)#:prefab)
;(define-struct assign (id value)#:prefab);
(define-struct equation (left right)#:prefab);代数 方程
(define-struct diffequation (y x value)#:prefab);常微分方程
(define-struct pdiffequation (y x value)#:prefab);偏微分方程
(define-struct diffequationomit (y value)#:prefab); 自变量隐含常微分方程 where x using default
(define-struct diffequation-high (y xlst value)#:prefab);高阶常微分方程
(define-struct pdiffequation-high (y xlst value)#:prefab);高阶偏微分方程
(define-struct diffequationomit-high (y times value)#:prefab);高阶自变量常微分方程 where x using default
(define-struct omitarea (omit lst)#:prefab);隐含变量
;(define-struct sub-assign(expr sublist value)#:prefab);
(define-struct func-definition(id formlist value)#:prefab)
(define-struct virtual-func-definition(id formlist value)#:prefab)
;(define-struct block-expr (block value)#:prefab)
;(define-struct binding-block-expr (bindings block value)#:prefab)
;(define-struct binding (id value)#:prefab)
;(define-struct pair-expr (car cdr)#:prefab)
;(define-struct pair-exprlist (lst else)#:prefab)
(define-struct variable (sym)#:prefab);
;(define-struct seq (first second end)#:prefab)
;(define-struct array-expr(lst)#:prefab)
;(define-struct hash-expr (name index nfd)#:prefab)
;(define-struct seq-inf (first second)#:prefab)
;(define-struct assignment (expr value)#:prefab);var ass
;(define-struct free-assignment(expr value)#:prefab);var
(define-struct operation (sym parameters)#:prefab);
(define-struct call  (expr sublist )#:prefab);
(define-struct virtualcall  (id sublist)#:prefab);由变量变换而来
(define-struct opt-call (id formlist )#:prefab)
;(define-struct when-statement (test block)#:prefab)
;(define-struct while-statement (test block)#:prefab)
;(define-struct do-statement (test block)#:prefab)
;(define-struct if-statement (test block else-block)#:prefab)
;(define-struct cond-statement (test-blocks else-block)#:prefab)
;(define-struct if-expr (test block else-block)#:prefab)
;(define-struct for-statement(bindings test update block)#:prefab)
;(define-struct sub-extract (expr sublist)#:prefab);

;----------------------------------------------------
;(define-struct if-statement (ifcond body)#:prefab)
;(define-struct ifelse-statement (ifcond body elsebody)#:prefab)

(define reslove-variable-list-iner
  (match-lambda
    ;[(constant a) '()]
    [(variable a)(list a)]
    [(operation sym plst) (apply append (map reslove-variable-list-iner plst))]
    [(call sym plst)(apply append (map reslove-variable-list-iner plst))]
    [else '()]))
(define(reslove-variable-list x)
  (remove-duplicates	  (reslove-variable-list-iner x)
                          eq?))

(define(varreplacerule org new)
  (match-lambda
    [(variable a)(if(eq? a org) new (make-variable a))]
    [(operation sym plst) (make-operation  sym (map (varreplacerule org new) plst))]
    [(call sym plst)(make-call sym  (map (varreplacerule org new) plst))]
    [(virtualcall sym plst)(make-virtualcall sym  (map (varreplacerule org new) plst))]
    [t t]))

(define(varreplacerules table)
   (match-lambda
    [(variable a)(hash-ref table a (lambda()(make-variable a)))]
    [(operation sym plst) (make-operation  sym (map (varreplacerules  table) plst))]
    [(call sym plst)(make-call sym  (map (varreplacerules  table) plst))]
    [(virtualcall sym plst)(make-virtualcall sym  (map (varreplacerules  table) plst))]
    [t (if(symbol? t)(hash-ref table t (lambda() t)) t)]))
;;-------------核心函数
(define((diffvarreplacerules table)df)
  (match-define(diffequation y x expr) df)
  (make-diffequation y x ((varreplacerules table) expr)))
;-------------------------
(define(make-table1 a b)
  (for/hash ([i (in-list a)]
             [j (in-list b)])
    (values i j)))

(define(vfundefreplace sublst vfundefexpr)
  (vcallassignment (exprforname (make-virtualcall (virtual-func-definition-id vfundefexpr) sublst))
                  ((varreplacerules2 (make-table1 (virtual-func-definition-formlist vfundefexpr) sublst))
                  (virtual-func-definition-value vfundefexpr))))
(define(varreplacerules2 table)
   (match-lambda
    [(variable a)(list (hash-ref table a (lambda()(make-variable a))) '())]
    [(operation sym plst)(define data (map (varreplacerules2  table) plst))
                         (list (make-operation  sym (map car data))
                               (apply append (map cadr data)))]
    [(call sym plst)(define data (map (varreplacerules2  table) plst))
                    (list (make-call sym  (map car data))
                          (apply append( map cadr data)))]
    [(virtualcall sym plst)(define rule (map (varreplacerules  table) plst))
                           (list (exprforname(make-virtualcall sym  rule))
                                 (list (make-virtualcall sym  rule)))]
    [t (list (if(symbol? t)(hash-ref table t (lambda() t)) t)
             '())]))

(module+ test

(reslove-variable-list  #s(operation
        +
        (#s(operation
            *
            (#s(operation / (#s(operation - (#s(variable mu))) #s(operation expt (#s(variable r) #s(constant 3)))))
             #s(variable x)))
         #s(operation
            /
            (#s(operation
                *
                (#s(operation / (#s(operation * (#s(variable drt1) #s(variable Fm))) #s(variable m))) #s(variable vx)))
             #s(variable v))))))
  
  ((varreplacerules '#hash((v . #s(virtualcall v (x t y vx vy m k u))) (r . #s(virtualcall r (x t y vx vy m k u)))))
#s(operation
        +
        (#s(operation
            *
            (#s(operation / (#s(operation - (#s(variable mu))) #s(operation expt (#s(variable r) #s(constant 3)))))
             #s(variable x)))
         #s(operation
            /
            (#s(operation
                *
                (#s(operation / (#s(operation * (#s(variable drt1) #s(variable Fm))) #s(variable m))) #s(variable vx)))
             #s(variable v))))))
  (define(replace-callitem replace-table eq)
 ; (displayln replace-table)
 ; (displayln eq)
  ((varreplacerules2 replace-table) eq))
(replace-callitem #hash((vy . #s(varsub vy 0)) (x . #s(varsub x 0)) (t . #s(varsub t 0)) (y . #s(varsub y 0)) (vx . #s(varsub vx 0)))
#s(operation * (#s(operation / (#s(operation - (#s(variable mu))) #s(operation expt (#s(virtualcall r (t y x vx vy)) #s(constant 3))))) #s(variable x))))

  
  )

