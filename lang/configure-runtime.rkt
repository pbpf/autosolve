#lang racket/base

(define (configure data)
  (current-read-interaction the-read))

(provide configure)

(require "../grammar/yacc.rkt"
         "../compiler.rkt"
         "../srcloc.rkt")

(define (the-read src ip)
  (cond
    [(or (not (char-ready? ip))
         (eof-object? (peek-char ip)))
     eof]
    [else 
  (compile-expr
   (parameterize ([current-source-name src])
        (parse-expr ip)))]))