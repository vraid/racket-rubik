#lang typed/racket

(provide post-spin-tile
         post-spin-permutation)

(define post-spin-tile
  (λ ([rotate : (-> (Vectorof Integer) (Vectorof Integer))]
      [normal : (-> Integer (Vectorof Integer))]
      [position : (-> Integer (Vectorof Integer))])
    (λ ([k : Integer])
      (let* ([rotated-normal (rotate (normal k))]
             [rotated-position (rotate (position k))])
        (λ ([n : Integer])
          (and (equal? rotated-normal (normal n))
               (equal? rotated-position (position n))))))))

(: find/default (All (A) (-> A (-> A Boolean) (Listof A) A)))
(define (find/default default f ls)
  (let ([res (findf f ls)])
    (if res res default)))

(define post-spin-permutation
  (λ ([tile-range : (Listof Integer)]
      [spinning? : (-> Integer Boolean)]
      [matching : (-> Integer (-> Integer Boolean))])
    (map (λ ([n : Integer])
           (if (not (spinning? n)) n (find/default -1 (matching n) tile-range)))
         tile-range)))
