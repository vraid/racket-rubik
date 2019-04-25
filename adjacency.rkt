#lang typed/racket

(require "flvector3.rkt"
         "quaternion.rkt"
         "discrete-algebra.rkt")

(provide closest-tile
         adjacent-tiles)

(define closest-tile
  (λ ([tile-center : (-> Integer FlVector)]
      [tile-rotation : (-> Integer quaternion)])
    (λ ([tile-range : (Listof Integer)]
        [v : FlVector])
      (argmin (λ ([n : Integer])
                (flvector3-distance-squared
                 (quaternion-vector-product (tile-rotation n) v)
                 (tile-center n)))
              tile-range))))

(define adjacent-tiles
  (λ ([tile-range : (Listof Integer)]
      [tile-normal : (-> Integer (Vectorof Integer))]
      [tile-position : (-> Integer (Vectorof Integer))])
    (λ ([n : Integer])
      (let ([neighbours?
             (λ ([k : Integer])
               (>= (if (equal? (tile-normal k)
                               (tile-normal n))
                       1
                       0)
                   (discrete-vector-distance
                    (tile-position k)
                    (tile-position n))))])
        (filter neighbours? tile-range)))))
