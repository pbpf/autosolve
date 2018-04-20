#lang racket/base
(require math/array)
(define (in-slice x  start end step)
  (call-with-values (lambda()
                      (quotient/remainder (- x start)
                                          step))
                    (lambda(q r)(and (zero? r)(>= q 0)(if end
                                                          (if(> step 0)(< x end)(> x end))
                                                          #t)))))
(define(in x s)
  (in-slice x (slice-start s)(slice-end s)(slice-step s)))
;(define mod quotient)
(define(is x y?)
  (y? x))

(define(for opt tran filter b s)
  (for/fold([r b])
           ([i s]
            #:when (filter i))
    (values (opt  (tran i)r))))

(define(first q? seq)
  (for/first([i seq]
             #:when (q? i))
    i))
(define inrange in-range)
(define add +)
(define empty '())
(provide in is for first  empty
         (rename-out [+ add]
                      [- sub]
                      ;[quotient mod]
                      [* times]
                      [/ div]
                      [in-range inrange]
                      [< LT]
                      [> GT]
                      [>= GE]
                      [<= LE]))