#lang racket
(require "../grammar/ast.rkt"
         racket/match)
(provide pass)
;this is compiler pass for get rid of omitarea

(define(omitarea-de omit lst)
     (apply append
            (for/list ([i (in-list lst)])
           (match i
            [`#s(diffequationomit ,y ,value)(list (make-diffequation y omit value))]
            [`#s(omitarea ,omit1 ,lst1) (omitarea-de omit1 lst1)]
            [t (list t)]))))

(define(pass-item a)
  (match a
    [`#s(omitarea ,omit1 ,lst1) (omitarea-de omit1 lst1)]
    [t (list t)]))

(define(pass a)
  (apply append (map  pass-item a)))