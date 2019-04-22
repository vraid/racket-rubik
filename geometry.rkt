#lang typed/racket

(require math/flonum
         "tile.rkt"
         "vertices.rkt"
         "matrix3.rkt"
         "quaternion.rkt")

(provide make-tiles
         vector-sum
         vector-subtract
         vector-product
         vector-cross-product
         matrix-vector-product
         rotation-matrix)

(define ref
  (λ ([v : (Vectorof Integer)])
    (λ ([n : Integer])
      (vector-ref v n))))

(: matrix-row (-> (Vectorof Integer) Integer (Vectorof Integer)))
(define (matrix-row m row)
  (let ([r : Integer (* 3 row)])
    (list->vector
     (map
      (ref m)
      (range r (+ 3 r))))))

(: vector-sum (-> (Vectorof Integer) Integer))
(define (vector-sum v)
  (for/fold ([sum 0])
            ([n (range (vector-length v))])
    (+ sum (vector-ref v n))))

(: matrix-vector-product (-> (Vectorof Integer) (Vectorof Integer) (Vectorof Integer)))
(define (matrix-vector-product m v)
  (let ([m (λ ([row : Integer])
             (vector-sum
              (vector-map *
                          (matrix-row m row)
                          v)))])
    (build-vector 3 m)))

(: vector-product (-> (Vectorof Integer) (Vectorof Integer) (Vectorof Integer)))
(define (vector-product u v)
  (vector-map * u v))

(: vector-cross-product (-> (Vectorof Integer) (Vectorof Integer) (Vectorof Integer)))
(define (vector-cross-product a b)
  (let ([v (ref a)]
        [u (ref b)])
    (vector (- (* (u 1) (v 2))
               (* (u 2) (v 1)))
            (- (* (u 2) (v 0))
               (* (u 0) (v 2)))
            (- (* (u 0) (v 1))
               (* (u 1) (v 0))))))

(: vector-subtract (-> (Vectorof Integer) (Vectorof Integer) (Vectorof Integer)))
(define (vector-subtract u v)
  (vector-map - v u))

(: rotation-matrix (-> (Vectorof Integer) Integer (Vectorof Integer)))
(define (rotation-matrix axis direction)
  (let ([x (vector-ref axis 0)]
        [y (vector-ref axis 1)]
        [z (vector-ref axis 2)]
        [d direction])
    (vector (* x x) (- (* d z)) (* d y)
            (* d z) (* y y) (- (* d x))
            (- (* d y)) (* d x) (* z z))))

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
             (map (λ ([face : Integer]
                      [orientation : quaternion])
                    (let* ([orientation-matrix (to-int-vector (quaternion->matrix3 orientation))])
                      (vector-map (λ ([t : tile])
                                    (tile
                                     face
                                     (matrix-vector-product
                                      orientation-matrix
                                      (tile-position t))
                                     (matrix-vector-product
                                      orientation-matrix
                                      (tile-normal t))
                                     orientation
                                     (tile-center-vertex t)
                                     (tile-edge-vertices t)))
                                  (face-vertices vertex-count))))
                  (range 6)
                  orientations)))))

(define make-tiles
  (tiles face-orientations))
