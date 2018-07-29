(module reader syntax/module-reader
  #:language 'autosolve
  #:read (lambda ([in (current-input-port)]) (this-read-syntax #f in))
  #:read-syntax this-read-syntax
  #:whole-body-readers? #t  
  #:language-info
  '#(autosolve/lang/lang-info get-info #f)
  #:info (lambda (key defval default)
           ; XXX Should have different comment character key
           (case key
             [(drracket:default-filters) '(["autosolve Sources" "*.ode"])]
             [(drracket:default-extension) "ode"]
             ;[(drracket:opt-out-toolbar-buttons)'(File Edit file edit)]
             [(color-lexer)
             ; (displayln 'pass)
              (dynamic-require 'autosolve/grammar/lex 'colorizer-lexer)]
             [( interaction-color-lexer)
              (dynamic-require 'autosolve/grammar/lex 'colorizer-lexer)]
             [else (default key defval)]))
  

  (require  "../grammar/yacc.rkt"
            "../compiler.rkt"
           "../srcloc.rkt"
           "../codegen.rkt"
           )
  (define (this-read-syntax [src #f] [in (current-input-port)])
    (parameterize ([current-source-name src])
       (begin0 '()
              (display(codegenrk4 in)))))
  )