#lang racket
(require "divide.rkt"
          "../grammar/ast.rkt")

(provide pass)
;加入微分方程隐含的变量
;this is for definevar
;rule-------------
;1. suppose we know what is a ode variable and what is not,then there is a list, say v1,v2,v3...vn or t (base var)
;2. suppose r=expr ; if expr contain at least a ode variable,then we replace r with r(t,v1,v2,v3...vn);
;2. create a replacer,that is once we call r(t1,x1,y1,....) , we replace expr with rule {t->t1,v1->x1,v2->y1,...}

(define(converr sym expr odevars)
  (define myvars (reslove-variable-list expr))
  (if(set-empty? (set-intersect myvars odevars))
     (make-definition sym expr)
     (make-func-definition sym odevars expr)))


(define(solve-def rulestable defvars odevars  defid defexpr)
  (define containvar (reslove-variable-list defexpr))
  (define mixvar (set-intersect containvar defvars))
  (if(not(set-empty? mixvar))
     (error ':= "using variable ~a before it's definition,auto solve algebra equation not support yet"
            (string-join (set->list mixvar) ","))
     (if(set-empty? (set-intersect containvar (set-union (hash-keys rulestable) odevars)))
        (values (make-definition defid defexpr)
                rulestable);如果不显含ode var,也不含有之前定义的变量 则保持不变否则变为 虚拟函数
        (values(make-virtual-func-definition defid odevars ((varreplacerules rulestable)defexpr))
               (hash-set rulestable defid  (make-virtualcall defid odevars))))));可以不考虑 defexpr 中的二级变量
;--------------------------------------------------------
(define(pass-item non-difflst nodiffvars odevars)
  (let loop([nodiffvarst nodiffvars][rest non-difflst][rt '()][rtb (hash)])
    (if(null? rest)
       (reverse rt)
       (let-values([(a tb)(solve-def rtb nodiffvarst odevars (definition-id (car rest))(definition-value (car rest)))])
          (loop (cdr nodiffvarst) (cdr rest) (cons a rt) tb)))))

;;;统计虚变量
(define(resolve-virtualvars lst)
  (for/list([i (in-list lst)]
            #:when (virtual-func-definition? i))
    (virtual-func-definition-id i)))

(define(pass lst)
  (define-values(defs diffeqs)(divide lst))
  (define ode?(ode-check? diffeqs))
  (when (not ode?)
    (error 'not-support "this is not a ode"))
  (define selfvar (ode-selfvar diffeqs))
  ;
  (define odevars (cons selfvar (get-diffvars diffeqs)))
  (define defvars (get-nodiffvars defs))
  (define passed1 (pass-item defs defvars  odevars))
  (define virtualvars (resolve-virtualvars passed1))
  (define rule-table (for/hash ([key (in-list virtualvars)])
                       (values key (make-virtualcall key odevars))))
  (values rule-table odevars virtualvars passed1 (map (diffvarreplacerules rule-table) diffeqs))
  )
  ;
  ;midvar to 
    
  ;all define
  ;(match
  