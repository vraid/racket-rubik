#lang racket

(require ffi/cvector
         ffi/unsafe
         math/flonum
         "opengl.rkt"
         "color.rkt"
         "stereographic-projection.rkt"
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

(define (make-tile-buffers tiles vertex-count)
  (let* ([tile-vertex-count (+ 1 (* 4 vertex-count))]
         [vertices (make-cvector _gl-vertex (* 6 9 tile-vertex-count))]
         [indices (make-cvector _uint (* 6 9 3 4 vertex-count))]
         [color (flcolor 0.0 0.0 0.0 0.0)])
    (for ([n (* 6 9)])
      (let ([tile (vector-ref tiles n)])
        (cvector-set! vertices (* n tile-vertex-count) (->gl-vertex (tile-center-vertex tile) color))
        (for ([i (* 4 vertex-count)])
          (cvector-set! vertices (+ 1 i (* n tile-vertex-count)) (->gl-vertex (vector-ref (tile-edge-vertices tile) i)
                                                                              color))
          (let ([k (+ (* i 3) (* n 3 4 vertex-count))])
            (cvector-set! indices k (* n tile-vertex-count))
            (cvector-set! indices (+ 1 k) (+ 1 (modulo i (* 4 vertex-count)) (* n tile-vertex-count)))
            (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) (* 4 vertex-count)) (* n tile-vertex-count))))))
      (set-gl-vertex-buffer! 'tile-vertices vertices)
      (set-gl-index-buffer! 'tile-indices indices))))

(define (make-tile-vertices vertex-count)
  (let* ([tile-vertex-count (+ 1 (* 4 vertex-count))]
         [vectors (make-cvector _gl-vertex tile-vertex-count)]
         [zero-vertex (->gl-vertex (flvector 0.0 0.0 0.0)
                                   (flcolor 0.0 0.0 0.0 0.0))])
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

(define ((updated-vertices tiles vertex-count) top-tile rotate-tile)
  (let* ([tile-vertex-count (+ 1 (* 4 vertex-count))]
         [vertices (gl-buffer-data (get-gl-buffer 'tile-vertices))])
    (when top-tile
      (let* ([color (flcolor 0.0 0.0 0.0 0.0)]
             [vertices (gl-buffer-data (get-gl-buffer 'top-tile-vertices))]
             [rotate (rotate-tile top-tile)])
        (for ([i (* 4 vertex-count)])
          (cvector-set! vertices (+ 1 i) (->gl-vertex (let ([v (stereographic-projection (rotate (vector-ref (tile-edge-vertices top-tile) i)))])
                                                        (if (or (nan? (flvector-ref v 0))
                                                                (nan? (flvector-ref v 1)))
                                                            (stereographic-projection (rotate (vector-ref (tile-edge-vertices top-tile) (modulo (+ 1 i) (* 4 vertex-count)))))
                                                            v))
                                                      color)))
        (set-gl-vertex-buffer! 'top-tile-vertices vertices)))
    (for ([n (* 6 9)])
      (let* ([tile (vector-ref tiles n)]
             [color (tile-color tile)])
        (if (eq? tile top-tile)
            (for ([i tile-vertex-count])
              (cvector-set! vertices (+ i (* n tile-vertex-count)) (->gl-vertex (flvector 0.0 0.0 0.0)
                                                                                color)))
            (let ([rotate (rotate-tile tile)])
              (begin
                (cvector-set! vertices (* n tile-vertex-count) (->gl-vertex (stereographic-projection (rotate (tile-center-vertex tile)))
                                                                            color))
                (for ([i (* 4 vertex-count)])
                  (cvector-set! vertices (+ (* n tile-vertex-count) 1 i) (->gl-vertex (stereographic-projection (rotate (vector-ref (tile-edge-vertices tile) i)))
                                                                                      color))))))))
    vertices))
