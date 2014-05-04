#lang racket

(provide (all-defined-out))

(require racket/flonum)

(define (stereographic-projection v)
  (let ([x (flvector-ref v 0)]
        [y (flvector-ref v 1)]
        [z (flvector-ref v 2)])
    (flvector (fl/ x (fl- 1.0 z))
              (fl/ y (fl- 1.0 z))
              0.0)))

(define (inverse-stereographic-projection v)
  (let* ([x (flvector-ref v 0)]
         [y (flvector-ref v 1)]
         [squares (fl+ (flexpt x 2.0) (flexpt y 2.0))]
         [z (fl/ (+ squares -1.0)
                 (+ squares 1.0))])
    (flvector (fl* x (fl- 1.0 z))
              (fl* y (fl- 1.0 z))
              z)))
