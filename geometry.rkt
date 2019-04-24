#lang typed/racket

(require math/flonum
         "tile.rkt"
         "vertices.rkt"
         "quaternion.rkt"
         "discrete-algebra.rkt")

(provide make-tiles)

(define face-orientations
  (list (quaternion-identity)
        (axis-angle->quaternion (flvector 0.0 1.0 0.0) (/ pi 2))
        (axis-angle->quaternion (flvector 1.0 0.0 0.0) (- (/ pi 2)))
        (axis-angle->quaternion (flvector 0.0 1.0 0.0) (- (/ pi 2)))
        (axis-angle->quaternion (flvector 1.0 0.0 0.0) (/ pi 2))
        (axis-angle->quaternion (flvector 1.0 0.0 0.0) pi)))

(: to-int-vector (-> FlVector (Vectorof Integer)))
(define (to-int-vector v)
  (vector-map exact-round (flvector->vector v)))

(define tiles
  (λ ([orientations : (Listof quaternion)])
    (λ ([vertex-count : Integer])
      (apply vector-append
             (map (λ ([orientation : quaternion])
                    (let* ([orientation-matrix (to-int-vector (quaternion->matrix3 orientation))])
                      (vector-map (λ ([t : tile])
                                    (struct-copy tile t
                                                 [position (discrete-matrix-vector-product
                                                            orientation-matrix
                                                            (tile-position t))]
                                                 [normal (discrete-matrix-vector-product
                                                          orientation-matrix
                                                          (tile-normal t))]
                                                 [rotation orientation]))
                                  (face-vertices vertex-count))))
                  orientations)))))

(define make-tiles
  (tiles face-orientations))
