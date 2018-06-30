#lang racket
(require "ast.rkt")
(provide gen-normal*
         )
;自动生成4阶龙格库塔算法 for matlab

;input form1
;vector form
;
(define(add* y z)
  (map (lambda(yi zi)(add yi zi)) y z))
(define(add2* y z)
  (map (lambda(yi)(add yi z)) y))
(define(assignment* as bs)
  (map assignment as bs))
;expr
;equation
;assignment
(define(gen-item y x  h k)
  (define xn (varsub x 0))
  (define yn (map (lambda(yi)(varsub yi 0)) y))
  (define yn+1 (map (lambda(yi)(varsub yi 1)) y))
;  (define k1 (varsub k 1))
;  (define k2 (varsub k 2))
 ; (define k3 (varsub k 3))
 ; (define k4 (varsub k 4))
  (define halfh (exprforname (divide h 2)))
  (define h/6 (exprforname (divide h 6)))
  (define xf (exprforname (add xn halfh)))
  (define len (length y))
  (define index (range 1 (+ len 1)))
  (list*
   (assignment halfh (divide h 2))
   (assignment h/6 (divide h 6))
   (assignment xf (add xn halfh))
   (append
   (for/list ([i (in-range 1 (+ len 1))])
    (assignment (varsub(vecsub k i)1) (funcall (difffuncsub i) (list* xn yn))))
   (for/list ([i (in-range 1 (+ len 1))])
      (assignment (varsub(vecsub k i)2) (funcall (difffuncsub i) (list* xf (add* yn (map (lambda(z)(times halfh (varsub(vecsub k z)1))) index)))))
     )
   (for/list ([i (in-range 1 (+ len 1))])
      (assignment (varsub(vecsub k i)3) (funcall (difffuncsub i) (list* xf (add* yn (map (lambda(z)(times halfh (varsub(vecsub k z)2))) index))))))
   (for/list ([i (in-range 1 (+ len 1))])
      (assignment (varsub(vecsub k i)4) (funcall (difffuncsub i) (list* xf (add* yn (map (lambda(z)(times h (varsub(vecsub k z)3))) index))))))
   (assignment* yn+1 (add* yn (map (lambda(z)
                                     (times h/6 (addmany (list (varsub(vecsub k z)1)
                                                               (times 2 (add (varsub(vecsub k z)2)
                                                                             (varsub(vecsub k z)3)))
                                                               (varsub(vecsub k z)4)))))
                                     index)))
   ))
  )

  

(define(gen-vecform y x f h k)
  (define halfh (exprforname (divide h 2)))
  (define h/6 (exprforname (divide h 6)))
  (list*  (assignment halfh (divide h 2))
   (assignment h/6 (divide h 6))
   (gen-item y x f h k)))
  
(define(gen-baseform2 n y x f h k)
  (define halfh (exprforname (divide h 2)))
  (define h/6 (exprforname (divide h 6)))
 (append* (list(assignment halfh (divide h 2))
        (assignment h/6 (divide h 6)))
   (apply map list
         (for/list ([i (in-range 0 n)])
          (gen-item (vecsub y i) (vecsub x i) (vecsub f i) h (vecsub k i))))))

(define(gen-baseform n y x f h k)
  (define halfh (exprforname (divide h 2)))
  (define h/6 (exprforname (divide h 6)))
 (append* (list(assignment halfh (divide h 2))
        (assignment h/6 (divide h 6)))
   (apply map list
         (for/list ([i (in-range 1 (add1 n))])
          (gen-item (vecsub y i) x (vecsub f i) h (vecsub k i))))))

(define(gen-baseform* ys x h k)
  (define n (length ys))
  (define halfh (exprforname (divide h 2)))
  (define h/6 (exprforname (divide h 6)))
  (remove-duplicates
 
 (append* (list(assignment halfh (divide h 2))
        (assignment h/6 (divide h 6)))
   (apply map list
         (for/list ([i (in-range 1 (add1 n))])
          (gen-item ys x (difffuncsub i) h (vecsub k i)))))))

;(define(replace-var models vars

;test
; (gen-baseform 10 'y 'x 'f 'h 'k)


;(gen-item '(y x vx vy) 't 'h 'k)

(define gen-normal* gen-item)
  
   