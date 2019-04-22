#lang typed/racket

(require math/flonum
         "color.rkt"
         "tile.rkt"
         "vertices.rkt")

(provide make-tiles
         vector-sum
         vector-subtract
         vector-product
         vector-cross-product
         matrix-vector-product
         rotation-matrix)

(define rubik-colors
  (list
   (flcolor 1.0 1.0 0.0 1.0)
   (flcolor 1.0 0.0 0.0 1.0)
   (flcolor 0.0 1.0 0.0 1.0)
   (flcolor 1.0 0.0 1.0 1.0)
   (flcolor 0.0 0.0 1.0 1.0)
   (flcolor 1.0 1.0 1.0 1.0)))

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
  (list
   (rotation-matrix (vector 1 1 1) 0)
   (rotation-matrix (vector 0 1 0) 1)
   (rotation-matrix (vector 1 0 0) -1)
   (rotation-matrix (vector 0 1 0) -1)
   (rotation-matrix (vector 1 0 0) 1)
   (vector 1 0 0
           0 -1 0
           0 0 -1)))

(define tiles
  (λ ([colors : (Listof flcolor)]
      [orientations : (Listof (Vectorof Integer))])
    (λ ([vertex-count : Integer])
      (apply vector-append
             (map (λ ([color : flcolor]
                      [orientation : (Vectorof Integer)])
                    (vector-map (λ ([t : tile])
                                  (tile
                                   color
                                   (matrix-vector-product
                                    orientation
                                    (tile-position t))
                                   (matrix-vector-product
                                    orientation
                                    (tile-normal t))
                                   (build-flvector 9 (compose fl (ref orientation)))
                                   (tile-center-vertex t)
                                   (tile-edge-vertices t)))
                                (face vertex-count)))
                  colors
                  orientations)))))

(define make-tiles
  (tiles rubik-colors face-orientations))
