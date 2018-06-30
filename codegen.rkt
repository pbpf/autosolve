#lang racket
(require "ast.rkt"
         "grammar/yacc.rkt"
         (prefix-in replace: "replace/base.rkt")
         "autork4.rkt"
         "compiler.rkt"
        (prefix-in deomit: "pass/deomit.rkt")
        (prefix-in dediffvar: "pass/deomitdiffvar.rkt"))

(provide codegen)

(define(codegen x)
  (define-values(tb vars eqs)(dediffvar:pass(deomit:pass (parse-expr  (open-input-string x)))))
  (define normaleqs (gen-normal* (cdr vars )(car vars) 'h 'k));;;h? k? bug?
  ;(replace:pass normaleqs vars eqs tb)
  (compile-statements(replace:pass normaleqs vars eqs tb))
  )
