#lang typed/racket

(require math/flonum)

(provide (all-defined-out))

(: col (FlVector (Vectorof Integer) FlVector (Vectorof Integer) -> FlVector))
(define (col v m u n)
  (flvector-map *
                (remap v m)
                (remap u n)))

(: remap (FlVector (Vectorof Integer) -> FlVector))
(define (remap v m)
  (let ([elm (λ ([v : FlVector]
                 [m : (Vectorof Integer)]
                 [i : Integer])
               (flvector-ref v (vector-ref m i)))])
    (flvector (elm v m 0)
              (elm v m 1) 
              (elm v m 2))))
