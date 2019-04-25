#lang typed/racket

(require math/flonum
         "constants.rkt"
         "flvector3.rkt"
         "quaternion.rkt")

(provide center-tile-vertices
         edge-tile-vertices
         corner-tile-vertices)

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
