#lang racket/base
(require  parser-tools/lex
          (for-syntax racket/base)
         (prefix-in : parser-tools/lex-sre))

(provide (all-defined-out))
;
(define-tokens value-tokens (NUM_CONST STR_CONST  SYMBOL BOOLEAN));number variable and function
(define-empty-tokens op-tokens (   ?
                                   := 
                                   OP CP MOP MCP BOP BCP
                                   + - * / ^ |'| |#d| |#p|
                                   > < >= <= = != => ->
                                   @ : COMMA //  $ & with-omit extern
                                   or and xor ! ~
                                   NEWLINE ...
                                   EOF
                                   |;| 
                                   SEMIC
                                   TILDE UMINUS UPLUS
                                   in is mod
                                   option WHEN IF WHILE DO FOR COND ELSE
                                   ))

(define-lex-abbrevs
  (nu-line (:or (:: #\newline #\newline) #\newline))
   (py-blank (:or #\space #\tab #\page))
   (comment (:: #\% #\* (:* (:~ #\return #\newline))))
   (letter (:or (:/ #\A #\Z) (:/ #\a #\z)))
   (digit (:/ #\0 #\9))
   (decimal-integer (:+ digit))
   (decimal-integer-L (:: decimal-integer))
   (bin-integer (:: "0"(:or #\b #\B) (:+ (:/ "0" "1"))))
   (oct-integer (:: "0"(:or #\o #\O) (:+ (:/ "0" "7"))))
   (hex-integer (:: "0" (:or #\x "X") (:+ (:or digit (:/ "A" "F") (:/ "a" "f")))))
   (float (:or point-float exponent-float))
   (point-float (:or (:: (:?  int-part) fraction)
                   (:: int-part ".")))
   (exponent-float (:: (:or int-part point-float) exponent))
   (int-part (:+ digit))
   (fraction (:: "." (:+ digit)))
   (exponent (:: (:or #\e "E") (:?  (:or #\+ #\-)) (:+ digit)))
   (double (:or decimal-integer  float ))
   ;(sdouble(:: (:or #\- #\+)double))
   (pure-complex (:: (:or decimal-integer  float)(:or #\i #\I)))
  )

(define-lex-abbrev STRING  (:or ;(:: #\' STRING1 #\')
                                (:: #\" STRING2 #\")
                                ))
;(define-lex-abbrev STRING1  (:*(:or (:~ #\' #\\)(:: #\\ any-char ))))
 (define-lex-abbrev STRING2  (:*(:or (:~ #\" #\\)(:: #\\ any-char ))))

;(define-lex-abbrev Rchar2 (:& any-char(complement (:or #\"))))

;(define-lex-abbrev R-comment (:: #\# (:* R-common-char
(define-lex-abbrevs
  (var-begin  letter)
  (var-next (:or  letter digit #\_ #\?)))

(define-lex-abbrev reserved-word (:or  "..." "and" "or" "xor" "mod"))

(define-lex-abbrevs 
  (symbol (:& (:: var-begin (:* var-next))
        (complement reserved-word))))
 
;shift reduce-able?
;(define current-reduce-able? (make-parameter #f))
;词法
(define rlexer
   (lexer-src-pos
   [(:or #\tab #\space #\page #\newline)  (return-without-pos(rlexer input-port))]
   [comment (return-without-pos(rlexer input-port))]
   ["when"(token-WHEN)]
   ["while"(token-WHILE)]
   ["do"(token-DO)]
   ["if"(token-IF)]
   ["for" (token-FOR)]
   ["cond" (token-COND)]
   ["else" (token-ELSE)]
   ["|"(token-option)]
   ["+" (token-+)]
   ["->" (token-->)]
   ["-" (token--)]
   ["*" (token-*)]
   ["//" (token-//)]
   ["/" (token-/)]
   ["^" (token-^)]
   ["?" (token-?)]
   ["or" (token-or)]
   ["xor" (token-xor)]
   ["and"(token-and)]
   ["mod"(token-mod)]
   ["in"(token-in)]
   ["is"(token-is)]
   [":=" (token-:=)]
   ["!=" (token-!=)]
   ["!" (token-!)]
   ["'" (token-|'|)];;;' diff
   ["#with"(token-with-omit)];
   ["#extern"(token-extern)]
   ["#d"(token-|#d|)];;常微分
   ["#p"(token-|#p|)];;偏微分
   ["~" (token-~)]
   [":" (token-:)]
   [";" (token-|;|)]
   ["%" (token-mod)]
   [">" (token->)]
   [">="(token->=)]
   ["<" (token-<)]
   ["<="(token-<=)]
   ["=>" (token-=>)]
   ["=" (token-=)]
   ["$" (token-$)]
   ["&" (token-&)]
   ["@" (token-@)]
   ["(" (token-OP)]
   [")" (token-CP)]
   ["{" (token-BOP)]
   ["}" (token-BCP)]
   ["[" (token-MOP)]
   ["]" (token-MCP)]
   ["," (token-COMMA)]
   ["..." (token-...)]
   ["#t" (token-BOOLEAN #t)]
   ["#f" (token-BOOLEAN #f)]
   [pure-complex(token-NUM_CONST (string->number (string-append "+" lexeme)))]
   [double(let(( mayint(string->number(get-integer-str 0 lexeme))))
           (if(integer? mayint)(token-NUM_CONST (inexact->exact mayint))
              (token-NUM_CONST  mayint)))]
   [bin-integer (token-NUM_CONST (string->number (get-integer-str 2 lexeme) 2))]
   [oct-integer (token-NUM_CONST (string->number (get-integer-str 2 lexeme) 8))]
   [hex-integer (token-NUM_CONST (string->number (get-integer-str 2 lexeme) 16))]
   [symbol(token-SYMBOL  (string->symbol  lexeme))] ;varable
   [STRING (token-STR_CONST (get-str lexeme))]
   ;[(:+ (:& any-char (complement whitespace )))(error "unbound indefier" lexeme)]
   [(eof) (token-EOF)]
   ))
(define(get-str s)
  (substring s 1 (-(string-length s)1)))
(define(get-integer-str skip x)
  (define len (string-length  x))
  (if(char=? #\L (string-ref x (- len 1)))
     (substring x skip  (- len 1))
     (substring x skip  )))

;
(define-lex-abbrevs
    (binary-operator (:or #\+   #\-   #\*  #\/   "//"   #\%  #\<     #\>    "<="     ">="      "="      "!="  "|" "^"
                         "and" "or" "xor" "mod"))
   (misc-operator (:or #\, #\; #\. #\`
                       #\: )))

(define colorizer-lexer
    (lexer
     ;; whitespace
     [(:+ (:or nu-line py-blank)) (color-token 'white-space)]
     ;; comments
     [comment (color-token 'comment)]
     ;; backslash
     [#\\ (color-token 'other)]
     ;; keywords (def, return, if, etc.)
     [reserved-word (color-token 'hash-colon-keyword)]
     [(:or binary-operator misc-operator) (color-token 'other)]
     ;; parenthesis
     [(:or "(" ")" "[" "]" "{" "}") (color-token 'parenthesis (string->symbol lexeme))]
     ;; identifiers
     [(:: (:or letter "_") (:* (:or letter digit "_"))) (color-token 'symbol)]
     ;; string literals
     [STRING (color-token 'string)]
     ;; number literals
     [(:or decimal-integer oct-integer hex-integer)
      (color-token 'constant)]
     [float (color-token 'constant)]
     [(:: (:or float int-part) (:or #\i "I")) (color-token 'constant)]
     [(:or "#t" "#f")(color-token 'constant)]
     [(:or "#with" "#extern")(color-token 'hash-colon-keyword)]
     [#\' (color-token 'hash-colon-keyword)]
     ;; incomplete string literals
     ;; misc
     [(eof) (values lexeme 'eof #f #f #f)]
     [(special) (color-token 'error)]
     [(special-comment) (color-token 'error)]
     [any-char (color-token 'error)]))


  (define-syntax (color-token stx)
    (syntax-case stx ()
      [(_ category)
       #'(color-token category #f)]
      [(_ category paren)
       (with-syntax ([lexeme    (datum->syntax stx 'lexeme)]
                     [start-pos (datum->syntax stx 'start-pos)]
                     [end-pos   (datum->syntax stx 'end-pos)])
         #'(values lexeme category paren (position-offset start-pos) (position-offset end-pos)))]))
;%6