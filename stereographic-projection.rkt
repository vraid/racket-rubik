#lang typed/racket

(require racket/flonum)

(provide (all-defined-out))

(: stereographic-projection (-> FlVector FlVector))
(define (stereographic-projection v)
  (let ([x (flvector-ref v 0)]
        [y (flvector-ref v 1)]
        [scale (fl/ 1.0 (fl- 1.0 (flvector-ref v 2)))])
    (flvector (fl* scale x)
              (fl* scale y)
              0.0)))

(: inverse-stereographic-projection (-> FlVector FlVector))
(define (inverse-stereographic-projection v)
  (let* ([x (flvector-ref v 0)]
         [y (flvector-ref v 1)]
         [squares (fl+ (flexpt x 2.0) (flexpt y 2.0))]
         [z (fl/ (+ squares -1.0)
                 (+ squares 1.0))])
    (flvector (fl* x (fl- 1.0 z))
              (fl* y (fl- 1.0 z))
              z)))
