#lang typed/racket

(require math/flonum
         "color.rkt"
         "constants.rkt"
         "flvector3.rkt"
         "matrix3.rkt"
         "quaternion.rkt")

(provide (struct-out tile)
         make-tiles
         edge-vertex-count
         tile-vertex-count
         vector-sum
         vector-subtract
         vector-product
         vector-cross-product
         matrix-vector-product
         rotation-matrix)

(struct tile
  ([color : flcolor]
   [position : (Vectorof Integer)]
   [normal : (Vectorof Integer)]
   [rotation : FlVector]
   [center-vertex : FlVector]
   [edge-vertices : (Vectorof FlVector)]))

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

(define rotation-plane (fl (sin (fl/ tau 24.0))))
(define plane-gap 0.025)
(define inner (- plane-gap rotation-plane))
(define outer (- 0.0 plane-gap rotation-plane))

(: angle-fraction (-> Flonum Integer Integer Flonum))
(define (angle-fraction a n m)
  (fl (* a (/ n m))))

(define diagonal-plane (flvector3-normal (flvector 1.0 0.0 1.0)))
(define diagonal-offset (flvector3-scale-to plane-gap diagonal-plane))

(define edge-vertex-count 16)
(define tile-vertex-count (+ 1 (* 4 edge-vertex-count)))

(define center-tile-vertices
  ((thunk
    (let* ([x inner]
           [corner-vec (let ([z (flsqrt (- 1.0 (* 2.0 x x)))])
                         (flvector x x z))]
           [center-vec (λ ([v : FlVector])
                         (flvector3-sum v (flvector (- x) 0.0 0.0)))]
           [angle (flvector3-angle (center-vec corner-vec)
                                   (center-vec
                                    (flvector* corner-vec
                                               (flvector 1.0 -1.0 1.0))))]
           [first-edge (build-vector edge-vertex-count
                                     (λ ([n : Integer])
                                       (quaternion-vector-product
                                        (axis-angle->quaternion (flvector 1.0 0.0 0.0)
                                                                (angle-fraction angle n edge-vertex-count))
                                        corner-vec)))])
      (apply vector-append
             (map (λ ([a : Integer])
                    (vector-map (curry quaternion-vector-product
                                       (axis-angle->quaternion (flvector 0.0 0.0 1.0)
                                                               (angle-fraction tau a 4)))
                                first-edge))
                  (range 4)))))))

(define flvector-curry
  (λ ([f : (-> FlVector FlVector FlVector)]
      [a : FlVector])
    (λ ([b : FlVector])
      (f a b))))

(define vector-angle
  (λ ([f : (-> FlVector FlVector)]
      [a : FlVector]
      [b : FlVector])
    (flvector3-angle (f a) (f b))))

(define edge-tile-vertices
  ((thunk
    (let* ([x inner]
           [y outer]
           [z (flsqrt (- 1.0 (flexpt x 2.0) (flexpt y 2.0)))]
           [one (let* ([horizontal (flvector 0.0 inner 0.0)]
                       [diagonal diagonal-offset]
                       [rem (flvector3-scale-to (flsqrt (- 1.0
                                                           (flvector3-length-squared horizontal)
                                                           (flvector3-length-squared diagonal)))
                                                (flvector -1.0 0.0 1.0))])
                  (flvector3-sum horizontal diagonal rem))]
           [two (flvector* one (flvector 1.0 -1.0 1.0))]
           [three (flvector y (- x) z)]
           [four (flvector* three (flvector 1.0 -1.0 1.0))]
           [top-angle (vector-angle (flvector-curry flvector3-rejection diagonal-plane)
                                    one two)]
           [side-angle (vector-angle (flvector-curry flvector3-subtract (flvector 0.0 inner 0.0))
                                     one four)]
           [bottom-angle (vector-angle (flvector-curry flvector3-subtract (flvector outer 0.0 0.0))
                                       three four)]
           [build-side
            (λ ([plane : FlVector]
                [angle : Flonum]
                [vector : FlVector])
              (build-vector edge-vertex-count
                            (λ ([n : Integer])
                              (quaternion-vector-product (axis-angle->quaternion
                                                          plane
                                                          (angle-fraction angle n edge-vertex-count))
                                                         vector))))])
      (vector-append
       (build-side diagonal-plane top-angle one)
       (build-side (flvector 0.0 -1.0 0.0) side-angle two)
       (build-side (flvector -1.0 0.0 0.0) bottom-angle three)
       (build-side (flvector 0.0 1.0 0.0) side-angle four))))))

(define corner-tile-vertices
  ((thunk
    (let* ([x outer]
           [z (flsqrt (- 1.0 (* 2.0 x x)))]
           [one (let* ([diagonal (flvector3-sum
                                  (flvector3-scale-to plane-gap (flvector 1.0 0.0 1.0))
                                  (flvector3-scale-to plane-gap (flvector 0.0 1.0 1.0)))]
                       [rem (flvector -1.0 -1.0 1.0)])
                  (flvector3-normal (flvector3-sum diagonal rem)))]
           [two (let* ([horizontal (flvector 0.0 outer 0.0)]
                       [diagonal diagonal-offset]
                       [rem (flvector3-scale-to (flsqrt (- 1.0
                                                           (flvector3-length-squared horizontal)
                                                           (flvector3-length-squared diagonal)))
                                                (flvector -1.0 0.0 1.0))])
                  (flvector3-sum horizontal diagonal rem))]
           [three (flvector x x z)]
           [four (flvector (flvector-ref two 1)
                           (flvector-ref two 0)
                           (flvector-ref two 2))]
           [top-angle (vector-angle (flvector-curry flvector3-rejection diagonal-plane)
                                    one two)]
           [side-angle (vector-angle (flvector-curry flvector3-subtract (flvector 0.0 inner 0.0))
                                     two three)]
           [build-side
            (λ ([plane : FlVector]
                [angle : Flonum]
                [vector : FlVector])
              (build-vector edge-vertex-count
                            (λ ([n : Integer])
                              (quaternion-vector-product (axis-angle->quaternion
                                                          plane
                                                          (angle-fraction angle n edge-vertex-count))
                                                         vector))))])
      (vector-append
       (build-side diagonal-plane top-angle one)
       (build-side (flvector 0.0 -1.0 0.0) side-angle two)
       (build-side (flvector -1.0 0.0 0.0) side-angle three)
       (build-side (flvector3-normal (flvector 0.0 1.0 1.0)) top-angle four))))))

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

(define face-tile-vertices
  (λ ([x : Integer]
      [y : Integer])
    (let* ([rotation (λ ([v : (Vectorof FlVector)])
                       (λ ([a : Flonum])
                         (vector-map (curry quaternion-vector-product
                                            (axis-angle->quaternion (flvector 0.0 0.0 1.0) (* a tau)))
                                     v)))]
           [rotated-edge (rotation edge-tile-vertices)]
           [rotated-corner (rotation corner-tile-vertices)])
      (match (list x y)
        ['(0 0) center-tile-vertices]
        ['(-1 0) edge-tile-vertices]
        ['(0 1) (rotated-edge 0.25)]
        ['(1 0) (rotated-edge 0.5)]
        ['(0 -1) (rotated-edge 0.75)]
        ['(-1 -1) corner-tile-vertices]
        ['(-1 1) (rotated-corner 0.25)]
        ['(1 1) (rotated-corner 0.5)]
        ['(1 -1) (rotated-corner 0.75)]
        [default (begin
                   (error "invalid position")
                   (vector))]))))

(define face-tile
  (let ([no-color (flcolor 0.0 0.0 0.0 0.0)]
        [no-rotation (flvector 0.0 0.0 0.0)])
    (λ ([n : Integer])
      (let* ([x (- (modulo n 3) 1)]
             [y (- (floor (/ n 3)) 1)]
             [center (flvector (fl x) (fl y) 1.5)])
        (tile
         no-color
         (vector x y 1)
         (vector 0 0 1)
         no-rotation
         (flvector3-normal center)
         (face-tile-vertices x y))))))

(define face
  (build-vector 9 face-tile))

(define tiles
  (λ ([colors : (Listof flcolor)]
      [orientations : (Listof (Vectorof Integer))])
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
                              face))
                colors
                orientations))))

(define (make-tiles)
  (tiles rubik-colors face-orientations))
