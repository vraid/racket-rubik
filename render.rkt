#lang racket

(require ffi/cvector
         ffi/unsafe
         math/flonum
         "matrix3.rkt"
         "quaternion.rkt"
         "opengl.rkt"
         "color.rkt"
         "geometry.rkt")

(provide make-tile-buffers
         make-tile-vertices
         make-tile-indices
         updated-vertices)

(define (->gl-vertex coord color)
  (make-gl-vertex
   (flvector-ref coord 0)
   (flvector-ref coord 1)
   (flvector-ref coord 2)
   (flcolor->byte (flcolor-red color))
   (flcolor->byte (flcolor-green color))
   (flcolor->byte (flcolor-blue color))
   (flcolor->byte (flcolor-alpha color))))

(define origo (flvector 0.0 0.0 0.0))
(define black (flcolor 0.0 0.0 0.0 0.0))

(define zero-vertex
  (->gl-vertex origo black))

(define (make-tile-buffers cube vertex-count)
  (let* ([tile-vertex-count (+ 1 (* 4 vertex-count))]
         [vertices (make-cvector _gl-vertex (* 6 9 tile-vertex-count))]
         [indices (make-cvector _uint (* 6 9 3 4 vertex-count))])
    (for ([n (cube-tile-range cube)])
      (cvector-set! vertices (* n tile-vertex-count) zero-vertex)
      (for ([i (* 4 vertex-count)])
        (cvector-set! vertices (+ 1 i (* n tile-vertex-count)) zero-vertex)
        (let ([k (+ (* i 3) (* n 3 4 vertex-count))])
          (cvector-set! indices k (* n tile-vertex-count))
          (cvector-set! indices (+ 1 k) (+ 1 (modulo i (* 4 vertex-count)) (* n tile-vertex-count)))
          (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) (* 4 vertex-count)) (* n tile-vertex-count)))))
      (set-gl-vertex-buffer! 'tile-vertices vertices)
      (set-gl-index-buffer! 'tile-indices indices))))

(define (make-tile-vertices vertex-count)
  (let* ([tile-vertex-count (+ 1 (* 4 vertex-count))]
         [vectors (make-cvector _gl-vertex tile-vertex-count)])
    (for ([i tile-vertex-count])
      (cvector-set! vectors i zero-vertex))
    vectors))

(define (make-tile-indices vertex-count)
  (let ([indices (make-cvector _uint (* 12 vertex-count))])
    (for ([i (* 4 vertex-count)])
      (let ([k (+ (* i 3))])
        (cvector-set! indices k 0)
        (cvector-set! indices (+ 1 k) (+ 1 (modulo i (* 4 vertex-count))))
        (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) (* 4 vertex-count))))))
    indices))

(define ((updated-vertices cube tile-color vertex-count) top-tile projection)
  (let* ([tile-vertex-count (+ 1 (* 4 vertex-count))]
         [vertices (gl-buffer-data (get-gl-buffer 'tile-vertices))]
         [tile-center (cube-tile-center cube)]
         [tile-vertices (cube-tile-vertices cube)])
    (when top-tile
      (let* ([vertices (gl-buffer-data (get-gl-buffer 'top-tile-vertices))]
             [project (projection top-tile)])
        (for ([i (* 4 vertex-count)])
          (cvector-set! vertices (+ 1 i) (->gl-vertex (let ([v (project (vector-ref (tile-vertices top-tile) i))])
                                                        (if (or (nan? (flvector-ref v 0))
                                                                (nan? (flvector-ref v 1)))
                                                            (project (vector-ref (tile-vertices top-tile) (modulo (+ 1 i) (* 4 vertex-count))))
                                                            v))
                                                      black)))
        (set-gl-vertex-buffer! 'top-tile-vertices vertices)))
    (for ([n (* 6 9)])
      (let* ([color (tile-color n)])
        (if (eq? n top-tile)
            (for ([i tile-vertex-count])
              (cvector-set! vertices (+ i (* n tile-vertex-count)) (->gl-vertex origo
                                                                                color)))
            (let ([project (projection n)])
              (begin
                (cvector-set! vertices (* n tile-vertex-count) (->gl-vertex (project (tile-center n))
                                                                            color))
                (for ([i (* 4 vertex-count)])
                  (cvector-set! vertices (+ (* n tile-vertex-count) 1 i) (->gl-vertex (project (vector-ref (tile-vertices n) i))
                                                                                      color))))))))
    vertices))
