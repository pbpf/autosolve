#lang racket
(require "ast.rkt")
;自动生成4阶龙格库塔算法 for matlab

;input form1
;vector form
;

;expr
;equation
;assignment
(define(gen-item y x f h k)
  (define xn (varsub x 0))
  (define yn (varsub y 0))
  (define yn+1 (varsub y 1))
  (define k1 (varsub k 1))
  (define k2 (varsub k 2))
  (define k3 (varsub k 3))
  (define k4 (varsub k 4))
  (define halfh (exprforname (divide h 2)))
  (define h/6 (exprforname (divide h 6)))
  (define xf (exprforname (add xn halfh)))
  
  (list
   ;(assignment halfh (divide h 2))
   ;(assignment h/6 (divide h 6))
   (assignment xf (add xn halfh))
   (assignment k1 (funcall f (list xn yn)))
   (assignment k2 (funcall f (list xf (add yn (times halfh k1)))))
   (assignment k3 (funcall f (list xf (add yn (times halfh k2)))))
   (assignment k4 (funcall f (list xf (add yn h))))
   (assignment yn+1 (add yn (times h/6 (addmany (list k1 (times 2 (add k2 k3)) k4)))))))

(define(gen-vecform y x f h k)
  (define halfh (exprforname (divide h 2)))
  (define h/6 (exprforname (divide h 6)))
  (list*  (assignment halfh (divide h 2))
   (assignment h/6 (divide h 6))
   (gen-item y x f h k)))
  
(define(gen-baseform n y x f h k)
  (define halfh (exprforname (divide h 2)))
  (define h/6 (exprforname (divide h 6)))
 (append* (list(assignment halfh (divide h 2))
        (assignment h/6 (divide h 6)))
   (apply map list
         (for/list ([i (in-range 0 n)])
          (gen-item (vecsub 'y i) (vecsub 'x i) (vecsub 'f i) h (vecsub 'k i))))))

;test
; (gen-baseform 10 'y 'x 'f 'h 'k)



  
   