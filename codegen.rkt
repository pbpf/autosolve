#lang racket/base
(require "ast.rkt"
         "grammar/yacc.rkt"
         "compiler.rkt"
         "methods/main.rkt"
         "pass/main.rkt"
        )

(provide codegenrk4)

(define((codegen  gen-model) in)
  (define-values(tb vars eqs)(dediffvar:pass(deomit:pass (parse-expr  in))))
  (define normaleqs (gen-model (cdr vars )(car vars) 'h 'k));;;h? k? bug?
  ;(replace:pass normaleqs vars eqs tb)
  (compile-statements(replace:pass normaleqs vars eqs tb))
  )
(define codegenrk4 (codegen rk4:gen-normal*))
