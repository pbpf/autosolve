#lang racket
(require "../grammar/ast.rkt")
(provide divide
         get-diffvars
         get-nodiffvars
         )
;将微分和非微分分开
;after  omitarea solved
(define(divide lst)
  (for/fold([diff '()]
            [notdiff '()]
            #:result (values (reverse notdiff)
                             (reverse diff))
            )
           ([i (in-list lst)])
    (if(diffequation? i)
       (values (cons i diff)
               notdiff)
       (values diff
               (cons i notdiff)))))

(define(get-diffvar df)
  (list (diffequation-y df)
        (diffequation-x df)))
(define(get-defcar df)
  (definition-id df))
; lst is all diff
;odevars
(define(get-diffvars lst)
  (remove-duplicates(apply append
         (map get-diffvar lst))
                    eq?))

(define(get-nodiffvars lst)
  (remove-duplicates(map get-defcar lst)
                    eq?))
  