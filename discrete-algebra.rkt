#lang typed/racket

(provide discrete-vector-subtract
         discrete-vector-product
         discrete-vector-cross-product
         discrete-vector-distance
         discrete-matrix-vector-product
         discrete-rotation-matrix)

(define ref
  (λ ([v : (Vectorof Integer)])
    (λ ([n : Integer])
      (vector-ref v n))))

(: discrete-matrix-row (-> (Vectorof Integer) Integer (Vectorof Integer)))
(define (discrete-matrix-row m row)
  (let ([r : Integer (* 3 row)])
    (list->vector
     (map
      (ref m)
      (range r (+ 3 r))))))

(: discrete-vector-sum (-> (Vectorof Integer) Integer))
(define (discrete-vector-sum v)
  (for/fold ([sum 0])
            ([n (range (vector-length v))])
    (+ sum (vector-ref v n))))

(: discrete-matrix-vector-product (-> (Vectorof Integer) (Vectorof Integer) (Vectorof Integer)))
(define (discrete-matrix-vector-product m v)
  (let ([m (λ ([row : Integer])
             (discrete-vector-sum
              (vector-map *
                          (discrete-matrix-row m row)
                          v)))])
    (build-vector 3 m)))

(: discrete-vector-product (-> (Vectorof Integer) (Vectorof Integer) (Vectorof Integer)))
(define (discrete-vector-product u v)
  (vector-map * u v))

(: discrete-vector-cross-product (-> (Vectorof Integer) (Vectorof Integer) (Vectorof Integer)))
(define (discrete-vector-cross-product a b)
  (let ([v (ref a)]
        [u (ref b)])
    (vector (- (* (u 1) (v 2))
               (* (u 2) (v 1)))
            (- (* (u 2) (v 0))
               (* (u 0) (v 2)))
            (- (* (u 0) (v 1))
               (* (u 1) (v 0))))))

(: discrete-vector-subtract (-> (Vectorof Integer) (Vectorof Integer) (Vectorof Integer)))
(define (discrete-vector-subtract u v)
  (vector-map - v u))

(: discrete-vector-distance (-> (Vectorof Integer) (Vectorof Integer) Integer))
(define (discrete-vector-distance v u)
  (discrete-vector-sum (vector-map abs (discrete-vector-subtract v u))))

(: discrete-rotation-matrix (-> (Vectorof Integer) Integer (Vectorof Integer)))
(define (discrete-rotation-matrix axis direction)
  (let ([x (vector-ref axis 0)]
        [y (vector-ref axis 1)]
        [z (vector-ref axis 2)]
        [d direction])
    (vector (* x x) (- (* d z)) (* d y)
            (* d z) (* y y) (- (* d x))
            (- (* d y)) (* d x) (* z z))))
