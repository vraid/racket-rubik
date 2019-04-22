#lang typed/racket

(require math/flonum
         "constants.rkt"
         "flvector3.rkt"
         "quaternion.rkt"
         "color.rkt")

(provide (struct-out tile)
         face)

(struct tile
  ([color : flcolor]
   [position : (Vectorof Integer)]
   [normal : (Vectorof Integer)]
   [rotation : FlVector]
   [center-vertex : FlVector]
   [edge-vertices : (Vectorof FlVector)]))

(define rotation-plane (fl (sin (fl/ tau 24.0))))
(define plane-gap 0.025)
(define inner (- plane-gap rotation-plane))
(define outer (- 0.0 plane-gap rotation-plane))

(: angle-fraction (-> Flonum Integer Integer Flonum))
(define (angle-fraction a n m)
  (fl (* a (/ n m))))

(define diagonal-plane (flvector3-normal (flvector 1.0 0.0 1.0)))
(define diagonal-offset (flvector3-scale-to plane-gap diagonal-plane))

(define center-tile-vertices
  (λ ([vertex-count : Integer])
    (let* ([x inner]
           [corner-vec (let ([z (flsqrt (- 1.0 (* 2.0 x x)))])
                         (flvector x x z))]
           [center-vec (λ ([v : FlVector])
                         (flvector3-sum v (flvector (- x) 0.0 0.0)))]
           [angle (flvector3-angle (center-vec corner-vec)
                                   (center-vec
                                    (flvector* corner-vec
                                               (flvector 1.0 -1.0 1.0))))]
           [first-edge (build-vector vertex-count
                                     (λ ([n : Integer])
                                       (quaternion-vector-product
                                        (axis-angle->quaternion (flvector 1.0 0.0 0.0)
                                                                (angle-fraction angle n vertex-count))
                                        corner-vec)))])
      (apply vector-append
             (map (λ ([a : Integer])
                    (vector-map (curry quaternion-vector-product
                                       (axis-angle->quaternion (flvector 0.0 0.0 1.0)
                                                               (angle-fraction tau a 4)))
                                first-edge))
                  (range 4))))))

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
  (λ ([vertex-count : Integer])
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
              (build-vector vertex-count
                            (λ ([n : Integer])
                              (quaternion-vector-product (axis-angle->quaternion
                                                          plane
                                                          (angle-fraction angle n vertex-count))
                                                         vector))))])
      (vector-append
       (build-side diagonal-plane top-angle one)
       (build-side (flvector 0.0 -1.0 0.0) side-angle two)
       (build-side (flvector -1.0 0.0 0.0) bottom-angle three)
       (build-side (flvector 0.0 1.0 0.0) side-angle four)))))

(define corner-tile-vertices
  (λ ([vertex-count : Integer])
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
              (build-vector vertex-count
                            (λ ([n : Integer])
                              (quaternion-vector-product (axis-angle->quaternion
                                                          plane
                                                          (angle-fraction angle n vertex-count))
                                                         vector))))])
      (vector-append
       (build-side diagonal-plane top-angle one)
       (build-side (flvector 0.0 -1.0 0.0) side-angle two)
       (build-side (flvector -1.0 0.0 0.0) side-angle three)
       (build-side (flvector3-normal (flvector 0.0 1.0 1.0)) top-angle four)))))

(define face-tile-vertices
  (λ ([center-vertices : (Vectorof FlVector)]
      [edge-vertices : (Vectorof FlVector)]
      [corner-vertices : (Vectorof FlVector)])
    (λ ([x : Integer]
        [y : Integer])
      (let* ([rotation (λ ([v : (Vectorof FlVector)])
                         (λ ([a : Flonum])
                           (vector-map (curry quaternion-vector-product
                                              (axis-angle->quaternion (flvector 0.0 0.0 1.0) (* a tau)))
                                       v)))]
             [edge (rotation edge-vertices)]
             [corner (rotation corner-vertices)])
        (match (list x y)
          ['(0 0) center-vertices]
          ['(-1 0) (edge 0.0)]
          ['(0 1) (edge 0.25)]
          ['(1 0) (edge 0.5)]
          ['(0 -1) (edge 0.75)]
          ['(-1 -1) (corner 0.0)]
          ['(-1 1) (corner 0.25)]
          ['(1 1) (corner 0.5)]
          ['(1 -1) (corner 0.75)]
          [default (begin
                     (error "invalid position")
                     (vector))])))))

(define face-tile
  (let ([no-color (flcolor 0.0 0.0 0.0 0.0)]
        [no-rotation (flvector 0.0 0.0 0.0)])
    (λ ([vertex-count : Integer])
      (let* ([face-vertices
              (face-tile-vertices
               (center-tile-vertices vertex-count)
               (edge-tile-vertices vertex-count)
               (corner-tile-vertices vertex-count))])
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
             (face-vertices x y))))))))

(define face
  (λ ([vertex-count : Integer])
    (build-vector 9 (face-tile vertex-count))))
