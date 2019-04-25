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

(define origin (flvector 0.0 0.0 0.0))
(define black (flcolor 0.0 0.0 0.0 0.0))

(define zero-vertex
  (->gl-vertex origin black))

(define (make-tile-buffers cube vertex-count)
  (let* ([tile-vertex-count (* 4 vertex-count)]
         [total-vertex-count (+ 1 tile-vertex-count)]
         [vertices (make-cvector _gl-vertex (* (length (cube-tile-range cube))
                                               total-vertex-count))]
         [indices (make-cvector _uint (* 3 (length (cube-tile-range cube)) tile-vertex-count))])
    (for ([n (cube-tile-range cube)])
      (cvector-set! vertices (* n total-vertex-count) zero-vertex)
      (for ([i tile-vertex-count])
        (cvector-set! vertices (+ 1 i (* n total-vertex-count)) zero-vertex)
        (let ([k (+ (* i 3) (* n 3 tile-vertex-count))])
          (cvector-set! indices k (* n total-vertex-count))
          (cvector-set! indices (+ 1 k) (+ 1 (modulo i tile-vertex-count) (* n total-vertex-count)))
          (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) tile-vertex-count) (* n total-vertex-count)))))
      (set-gl-vertex-buffer! 'tile-vertices vertices)
      (set-gl-index-buffer! 'tile-indices indices))))

(define (make-tile-vertices vertex-count)
  (let* ([tile-vertex-count (+ 1 (* 4 vertex-count))]
         [vectors (make-cvector _gl-vertex tile-vertex-count)])
    (for ([i tile-vertex-count])
      (cvector-set! vectors i zero-vertex))
    vectors))

(define (make-tile-indices vertex-count)
  (let* ([tile-vertex-count (* 4 vertex-count)]
         [indices (make-cvector _uint (* 3 tile-vertex-count))])
    (for ([i tile-vertex-count])
      (let ([k (+ (* i 3))])
        (cvector-set! indices k 0)
        (cvector-set! indices (+ 1 k) (+ 1 (modulo i tile-vertex-count)))
        (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) tile-vertex-count)))))
    indices))

(define (((set-vertices! vertices) color) index vec)
  (cvector-set! vertices
                index
                (->gl-vertex vec color)))

(define ((updated-vertices cube tile-color vertex-count) top-tile projection)
  (let* ([tile-vertex-count (+ 1 (* 4 vertex-count))]
         [vertices (gl-buffer-data (get-gl-buffer 'tile-vertices))]
         [set-vertex! (set-vertices! vertices)]
         [tile-center (cube-tile-center cube)]
         [tile-vertices (cube-tile-vertices cube)])
    (when top-tile
      (let* ([vertices (gl-buffer-data (get-gl-buffer 'top-tile-vertices))]
             [set-vertex! ((set-vertices! vertices) black)]
             [project (projection top-tile)]
             [tile-vertices (tile-vertices top-tile)])
        (for ([i (* 4 vertex-count)])
          (set-vertex! (+ 1 i)
                       (let ([v (project (vector-ref tile-vertices i))])
                         (if (or (nan? (flvector-ref v 0))
                                 (nan? (flvector-ref v 1)))
                             (project (vector-ref tile-vertices (modulo (+ 1 i) (* 4 vertex-count))))
                             v))))
        (set-gl-vertex-buffer! 'top-tile-vertices vertices)))
    (for ([n (cube-tile-range cube)])
      (let* ([offset (* n tile-vertex-count)]
             [color (tile-color n)]
             [set-vertex! (set-vertex! color)])
        (if (eq? n top-tile)
            (for ([i tile-vertex-count])
              (set-vertex! (+ offset i) origin))
            (let* ([project (projection n)]
                   [tile-vertices (tile-vertices n)])
              (begin
                (set-vertex! offset (project (tile-center n)))
                (for ([i (* 4 vertex-count)])
                  (set-vertex! (+ offset i 1) (project (vector-ref tile-vertices i)))))))))
    vertices))
