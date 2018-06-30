#lang racket
(require "ast.rkt")

(provide pass-exprforname)
;
(define(get-core-id efn)
  'variable)
(define(pass-exprforname2  efn table countb)
  ;命名算法
  (define c-id (get-core-id efn))
  (define name (hash-ref table efn (lambda()(shortname c-id (+ (hash-ref countb c-id (lambda()0)) 1)))))
  
  (values name
          (hash-set table efn name)
          (hash-update countb add1 (lambda(obj)0))))

(define(pass-exprforname  efn table countb)
  ;命名算法
  (define c-id (get-core-id efn))
  (define name (hash-ref table efn (lambda()(format "~a_~a" c-id (+ (hash-ref countb c-id (lambda()0)) 1)))))
  
  (values name
          (hash-set table efn name)
          (hash-update countb c-id add1 (lambda()0))))