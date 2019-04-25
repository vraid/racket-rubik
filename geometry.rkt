#lang typed/racket

(require math/flonum
         "constants.rkt"
         "flvector3.rkt"
         "vertices.rkt"
         "quaternion.rkt"
         "discrete-algebra.rkt")

(provide (struct-out cube)
         make-cube)

(struct cube
  ([tile-range : (Listof Integer)]
   [face-normal : (-> Integer (Vectorof Integer))]
   [face-rotation : (-> Integer quaternion)]
   [tile-face : (-> Integer Integer)]
   [tile-position : (-> Integer (Vectorof Integer))]
   [tile-vertices : (-> Integer (Vectorof FlVector))]
   [tile-center : (-> Integer FlVector)]))

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

(struct tile
  ([position : (Vector Integer Integer)]
   [vertices : (Vectorof FlVector)]))

(define tile-definitions
  (λ ([vertex-count : Integer])
    (let* ([quaternion (λ ([a : Flonum])
                         (axis-angle->quaternion (flvector 0.0 0.0 1.0) (* a tau)))]
           [rotation (λ ([v : (Vectorof FlVector)])
                       (λ ([a : Flonum])
                         (vector-map (curry quaternion-vector-product (quaternion a))
                                     v)))]
           [edge (rotation (edge-tile-vertices vertex-count))]
           [corner (rotation (corner-tile-vertices vertex-count))])
      (list
       (tile '#(0 0) (center-tile-vertices vertex-count))
       (tile '#(-1 0) (edge 0.0))
       (tile '#(0 1) (edge 0.25))
       (tile '#(1 0) (edge 0.5))
       (tile '#(0 -1) (edge 0.75))
       (tile '#(-1 -1) (corner 0.0))
       (tile '#(-1 1) (corner 0.25))
       (tile '#(1 1) (corner 0.5))
       (tile '#(1 -1) (corner 0.75))))))

(define 3d-position
  (λ ([pos : (Vector Integer Integer)])
    (vector (vector-ref pos 0)
            (vector-ref pos 1)
            1)))

(define center
  (λ ([pos : (Vector Integer Integer)])
    (let ([x (vector-ref pos 0)]
          [y (vector-ref pos 1)])
      (flvector3-normal (flvector (fl x) (fl y) 1.5)))))

(: ref (All (A) (-> (Listof A) (-> Integer A))))
[define (ref ls)
  (let ([vec (list->vector ls)])
    (λ ([n : Integer])
      (vector-ref vec n)))]

(: to-cube (-> (Listof tile) cube))
(define (to-cube tiles)
  (let* ([rotations face-orientations]
         [rotation-matrices (map (compose to-int-vector quaternion->matrix3)
                                 rotations)]
         [normal (ref (map (λ ([m : (Vectorof Integer)])
                             (discrete-matrix-vector-product m (vector 0 0 1)))
                           rotation-matrices))]
         [repeat (λ ([f : (-> Integer (Listof Integer))])
                   (apply append (map f (range 6))))]
         [face (ref (repeat (λ ([n : Integer])
                              (make-list 9 n))))]
         [corresponding (ref (repeat (λ ([n : Integer])
                                       (range 9))))]
         [2d-positions (map tile-position tiles)]
         [positions (map 3d-position 2d-positions)]
         [vertices (map tile-vertices tiles)]
         [centers (map center 2d-positions)]
         [rotation-matrices (list->vector rotation-matrices)]
         [positions (list->vector positions)])
    (cube
     (range (* 6 9))
     normal
     (ref rotations)
     face
     (λ ([n : Integer])
       (discrete-matrix-vector-product
        (vector-ref rotation-matrices (face n))
        (vector-ref positions (corresponding n))))
     (compose (ref vertices) corresponding)
     (compose (ref centers) corresponding))))

(define make-cube (compose to-cube tile-definitions))
