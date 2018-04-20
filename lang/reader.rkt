(module reader syntax/module-reader
  #:language 'expr
  #:read (lambda ([in (current-input-port)]) (this-read-syntax #f in))
  #:read-syntax this-read-syntax
  #:whole-body-readers? #t  
  #:language-info
  '#(Mathr/lang/lang-info get-info #f)
  #:info (lambda (key defval default)
           ; XXX Should have different comment character key
           (case key
             [(drracket:default-filters) '(["Mathematica Sources" "*.math"])]
             [(drracket:default-extension) "math"]
             ;[(drracket:opt-out-toolbar-buttons)'(File Edit file edit)]
             [(color-lexer interaction-color-lexer)
              (dynamic-require 'expr/grammar/lex 'colorizer-lexer)]
             [else (default key defval)]))
  

  (require  "../grammar/yacc.rkt"
            "../compiler.rkt"
           "../srcloc.rkt"
           )
  (define (this-read-syntax [src #f] [in (current-input-port)])
    (compile-expr
     (parameterize ([current-source-name src])
       (parse-expr in)))))