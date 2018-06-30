#lang racket
;
(require (rename-in "deomit.rkt" [pass pass1])
         (rename-in "deomitdiffvar.rkt"[pass pass2])
         )

(module+ test
  (require "../grammar/yacc.rkt")


  (define x1 (pass1(parse-expr  (open-input-string "r:=sqrt(x^2+y^2);v:=sqrt(vx^2+vy^2+r);z:=v+1;#with(t){#d(x,t)=vx;y'=vy;vx'=-mu/r^3*x+drt1*Fm/m*vx/v;vy'=-mu/r^3*y+drt1*Fm/m*vy/v;m'=m1*drt2;}"))
     ))
(pass2 x1)
  )