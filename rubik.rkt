#lang racket

(require racket/gui/base
         "flvector3.rkt"
         "stereographic-projection.rkt"
         "quaternion.rkt"
         "matrix3.rkt"
         "math.rkt"
         "color.rkt"
         "opengl.rkt"
         math/flonum
         ffi/cvector
         ffi/unsafe)

(require plot
         profile
         profile/render-text)

(define current-rotation (quaternion-identity))
(define (rotation) current-rotation)
(define scale 0.15)
(define scale-max 100.0)
(define scale-min 0.5)

(define mouse-moving? #f)
(define mouse-down? #f)
(define mouse-down-x #f)
(define mouse-down-y #f)
(define mouse-down-vector #f)
(define last-mouse-x #f)
(define last-mouse-y #f)
(define mouse-button #f)
(define milliseconds-between-frames 15.0)
(define last-draw 0.0)

(define-values
  (display-width display-height)
  (get-display-size))

(define-values
  (window-width window-height)
  (get-display-size))

(define rubik-colors
  (list
   (flcolor 1.0 1.0 0.0)
   (flcolor 1.0 0.0 0.0)
   (flcolor 0.0 1.0 0.0)
   (flcolor 1.0 0.0 1.0)
   (flcolor 0.0 0.0 1.0)
   (flcolor 1.0 1.0 1.0)))

(define face-orientations
  (list
   (vector 1 0 0
           0 1 0
           0 0 1)
   (vector 0 0 1
           0 1 0
           1 0 0)
   (vector 1 0 0
           0 0 1
           0 1 0)
   (vector 0 0 -1
           0 1 0
           1 0 0)
   (vector 1 0 0
           0 0 -1
           0 1 0)
   (vector 1 0 0
           0 1 0
           0 0 -1)))

(define edge-vertex-count 16)

(define tile-vertex-count (+ 1 (* 4 edge-vertex-count)))

(define (angle-fraction a n m)
  (fl (* a (/ n m))))

(define rotation-plane (fl (sin (fl/ tau 24.0))))
(define plane-gap 0.025)
(define inner (- plane-gap rotation-plane))
(define outer (- 0.0 plane-gap rotation-plane))

(define center-tile-vertices
  ((thunk
    (define x inner)
    (define corner-vec (let ([z (sqrt (- 1.0 (* 2.0 x x)))])
                         (flvector x x z)))
    (define (center-vec v) (flvector3-sum v (flvector (- x) 0.0 0.0)))
    (define angle (flvector3-angle (center-vec corner-vec)
                                   (center-vec
                                    (flvector* corner-vec
                                               (flvector 1.0 -1.0 1.0)))))
    (define first-edge (build-vector edge-vertex-count
                                     (lambda (n)
                                       (quaternion-vector-product
                                        (axis-angle->quaternion (flvector 1.0 0.0 0.0)
                                                                (angle-fraction angle n edge-vertex-count))
                                        corner-vec))))
    (apply vector-append
           (map (lambda (a)
                  (vector-map (curry quaternion-vector-product
                                     (axis-angle->quaternion (flvector 0.0 0.0 1.0)
                                                             (angle-fraction tau a 4)))
                              first-edge))
                (range 4))))))

(define diagonal-plane (flvector3-normal (flvector 1.0 0.0 1.0)))
(define diagonal-offset (flvector3-scale-to plane-gap diagonal-plane))

(define edge-tile-vertices
  ((thunk
    (define x inner)
    (define y outer)
    (define z (sqrt (- 1.0 (flexpt x 2.0) (flexpt y 2.0))))
    (define one (let* ([horizontal (flvector 0.0 inner 0.0)]
                       [diagonal diagonal-offset]
                       [rem (flvector3-scale-to (sqrt (- 1.0
                                                         (flvector3-length-squared horizontal)
                                                         (flvector3-length-squared diagonal)))
                                                (flvector -1.0 0.0 1.0))])
                  (flvector3-sum horizontal diagonal rem)))
    (define two (flvector* one (flvector 1.0 -1.0 1.0)))
    (define three (flvector y (- x) z))
    (define four (flvector* three (flvector 1.0 -1.0 1.0)))
    (define top-angle (apply flvector3-angle (map (curry flvector3-rejection diagonal-plane)
                                                  (list one two))))
    (define side-angle (apply flvector3-angle (map (curry flvector3-subtract (flvector 0.0 inner 0.0))
                                                   (list one four))))
    (define bottom-angle (apply flvector3-angle (map (curry flvector3-subtract (flvector outer 0.0 0.0))
                                                     (list three four))))
    (define (build-side plane angle vector)
      (build-vector edge-vertex-count
                    (lambda (n)
                      (quaternion-vector-product (axis-angle->quaternion
                                                  plane
                                                  (angle-fraction angle n edge-vertex-count))
                                                 vector))))
    (vector-append
     (build-side diagonal-plane top-angle one)
     (build-side (flvector 0.0 -1.0 0.0) side-angle two)
     (build-side (flvector -1.0 0.0 0.0) bottom-angle three)
     (build-side (flvector 0.0 1.0 0.0) side-angle four)))))

(define corner-tile-vertices
  ((thunk
    (define x outer)
    (define z (sqrt (- 1.0 (* 2.0 x x))))
    (define one (let* ([diagonal (flvector3-sum
                                  (flvector3-scale-to plane-gap (flvector 1.0 0.0 1.0))
                                  (flvector3-scale-to plane-gap (flvector 0.0 1.0 1.0)))]
                       [rem (flvector -1.0 -1.0 1.0)])
                  (flvector3-normal (flvector3-sum diagonal rem))))
    (define two (let* ([horizontal (flvector 0.0 outer 0.0)]
                       [diagonal diagonal-offset]
                       [rem (flvector3-scale-to (sqrt (- 1.0
                                                         (flvector3-length-squared horizontal)
                                                         (flvector3-length-squared diagonal)))
                                                (flvector -1.0 0.0 1.0))])
                  (flvector3-sum horizontal diagonal rem)))
    (define three (flvector x x z))
    (define four (flvector (flvector-ref two 1)
                           (flvector-ref two 0)
                           (flvector-ref two 2)))
    (define top-angle (apply flvector3-angle (map (curry flvector3-rejection diagonal-plane)
                                                  (list one two))))
    (define side-angle (apply flvector3-angle (map (curry flvector3-subtract (flvector 0.0 inner 0.0))
                                                   (list two three))))
    (define (build-side plane angle vector)
      (build-vector edge-vertex-count
                    (lambda (n)
                      (quaternion-vector-product (axis-angle->quaternion
                                                  plane
                                                  (angle-fraction angle n edge-vertex-count))
                                                 vector))))
    (vector-append
     (build-side diagonal-plane top-angle one)
     (build-side (flvector 0.0 -1.0 0.0) side-angle two)
     (build-side (flvector -1.0 0.0 0.0) side-angle three)
     (build-side (flvector3-normal (flvector 0.0 1.0 1.0)) top-angle four)))))

(define edge-vertices
  ((thunk
    (define k (fl* 0.95 0.5))
    (define -k (- k))
    (define (interpolate n)
      (fl* k (fl- (fl (* 2 (/ n edge-vertex-count))) 1.0)))
    (apply vector-append
           (map (curry build-vector edge-vertex-count)
                (list
                 (lambda (n)
                   (flvector (interpolate n) -k 0.0))
                 (lambda (n)
                   (flvector k (interpolate n) 0.0))
                 (lambda (n)
                   (flvector (- (interpolate n)) k 0.0))
                 (lambda (n)
                   (flvector -k (- (interpolate n)) 0.0))))))))

(struct tile
  (color
   position
   normal
   rotation
   center-vertex
   edge-vertices))

(define face
  (build-vector 9 (lambda (n)
                    (let* ([x (- (modulo n 3) 1)]
                           [y (- (floor (/ n 3)) 1)]
                           [center (flvector (fl x) (fl y) 1.5)])
                      (tile
                       #f
                       (vector x y 1)
                       (vector 0 0 1)
                       #f
                       (flvector3-normal center)
                       (cond
                         [(= 0 x y) center-tile-vertices]
                         [(and (= -1 x) (= 0 y)) edge-tile-vertices]
                         [(and (= 0 x) (= 1 y)) (vector-map (curry quaternion-vector-product
                                                                   (axis-angle->quaternion (flvector 0.0 0.0 1.0) (* 0.25 tau)))
                                                            edge-tile-vertices)]
                         [(and (= 1 x) (= 0 y)) (vector-map (curry quaternion-vector-product
                                                                   (axis-angle->quaternion (flvector 0.0 0.0 1.0) (* 0.5 tau)))
                                                            edge-tile-vertices)]
                         [(and (= 0 x) (= -1 y)) (vector-map (curry quaternion-vector-product
                                                                    (axis-angle->quaternion (flvector 0.0 0.0 1.0) (* 0.75 tau)))
                                                             edge-tile-vertices)]
                         [(= -1 x y) corner-tile-vertices]
                         [(and (= -1 x) (= 1 y)) (vector-map (curry quaternion-vector-product
                                                                    (axis-angle->quaternion (flvector 0.0 0.0 1.0) (* 0.25 tau)))
                                                             corner-tile-vertices)]
                         [(and (= 1 x) (= 1 y)) (vector-map (curry quaternion-vector-product
                                                                   (axis-angle->quaternion (flvector 0.0 0.0 1.0) (* 0.5 tau)))
                                                            corner-tile-vertices)]
                         [(and (= 1 x) (= -1 y)) (vector-map (curry quaternion-vector-product
                                                                    (axis-angle->quaternion (flvector 0.0 0.0 1.0) (* 0.75 tau)))
                                                             corner-tile-vertices)]
                         [else (vector-map (lambda (a) (flvector 0.0 0.0 0.0))
                                           (vector-map (compose flvector3-normal (curry flvector3-sum center))
                                                       edge-vertices))]))))))

(define (matrix-row m n)
  (let ([n (* 3 n)])
    (list->vector
     (map
      (curry vector-ref m)
      (range n (+ 3 n))))))

(define (vector-sum v)
  (for/fold ([sum 0])
    ([n (range (vector-length v))])
    (+ sum (vector-ref v n))))

(define (matrix-vector-product m v)
  (let ([m (lambda (r)
             (vector-sum
              (vector-map *
                          (matrix-row m r)
                          v)))])
    (build-vector 3 m)))

(define tiles
  (apply vector-append
         (map (lambda (n color orientation)
                (vector-map (lambda (t)
                              (tile
                               color
                               (matrix-vector-product
                                orientation
                                (tile-position t))
                               (matrix-vector-product
                                orientation
                                (tile-normal t))
                               (build-flvector 9 (compose fl (curry vector-ref orientation)))
                               (tile-center-vertex t)
                               (tile-edge-vertices t)))
                            face))
              (range 6)
              rubik-colors
              face-orientations)))

(define (->gl-vertex coord color)
  (make-gl-vertex
   (flvector-ref coord 0)
   (flvector-ref coord 1)
   (flvector-ref coord 2)
   (flcolor->byte (flcolor-red color))
   (flcolor->byte (flcolor-green color))
   (flcolor->byte (flcolor-blue color))
   0))

(define (make-tile-buffers!)
  (let* ([vertices (make-cvector _gl-vertex (* 6 9 tile-vertex-count))]
         [indices (make-cvector _uint (* 6 9 3 4 edge-vertex-count))]
         [color (flcolor 0.0 0.0 0.0)])
    (for ([n (* 6 9)])
      (let ([tile (vector-ref tiles n)])
        (cvector-set! vertices (* n tile-vertex-count) (->gl-vertex (tile-center-vertex tile) color))
        (for ([i (* 4 edge-vertex-count)])
          (cvector-set! vertices (+ 1 i (* n tile-vertex-count)) (->gl-vertex (vector-ref (tile-edge-vertices tile) i)
                                                                              color))
          (let ([k (+ (* i 3) (* n 3 4 edge-vertex-count))])
            (cvector-set! indices k (* n tile-vertex-count))
            (cvector-set! indices (+ 1 k) (+ 1 (modulo i (* 4 edge-vertex-count)) (* n tile-vertex-count)))
            (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) (* 4 edge-vertex-count)) (* n tile-vertex-count))))))
      (set-gl-vertex-buffer! 'tile-vertices vertices)
      (set-gl-index-buffer! 'tile-indices indices))))

(define (set-gl-vertex-color! p color)
  (set-gl-vertex-red! p (byte-color-red color))
  (set-gl-vertex-green! p (byte-color-green color))
  (set-gl-vertex-blue! p (byte-color-blue color)))

(define (update-vertices!)
  (let ([vertices (gl-buffer-data (get-gl-buffer 'tile-vertices))])
    (for ([n (* 6 9)])
      (let* ([tile (vector-ref tiles n)]
             [color (tile-color tile)]
             [m (matrix3* (quaternion->matrix3 (rotation)) (tile-rotation tile))]
             [rotate (curry matrix3-vector3* m)])
        (if (fl< 0.91 (flvector-ref (rotate (tile-center-vertex tile)) 2))
            (for ([i tile-vertex-count])
              (cvector-set! vertices (+ i (* n tile-vertex-count)) (->gl-vertex (flvector 0.0 0.0 0.0)
                                                                                color)))
            (begin
              (cvector-set! vertices (* n tile-vertex-count) (->gl-vertex (stereographic-projection (rotate (tile-center-vertex tile)))
                                                                          color))
              (for ([i (* 4 edge-vertex-count)])
                (cvector-set! vertices (+ (* n tile-vertex-count) 1 i) (->gl-vertex (stereographic-projection (rotate (vector-ref (tile-edge-vertices tile) i)))
                                                                                    color)))))))
    (set-gl-vertex-buffer! 'tile-vertices vertices)))

(define frame
  (new frame%
       [label "rubik"]
       [width window-width]
       [height window-height]
;       [style '(no-resize-border
;                no-caption
;                no-system-menu)]
       ))

(define (screen-to-viewport v)
  (flvector
   (fl* (fl (/ (- (vector-ref v 0) (/ display-width 2)) display-width scale)) (exact->inexact (/ display-width display-height)))
   (fl (/ (- (vector-ref v 1) (/ display-height 2)) display-height scale))))

(define (mouse-to-sphere event)
  (quaternion-vector-product
   (quaternion-inverse (rotation))
   (inverse-stereographic-projection
    (screen-to-viewport (vector (send event get-x)
                                (send event get-y))))))

(define (closest-tile v)
  (let ([distance (build-flvector (* 9 6) (lambda (n) (flvector3-distance-squared v (tile-center-vertex (vector-ref tiles n)))))])
    (foldl (lambda (n closest)
             (if (< (flvector-ref distance n)
                    (flvector-ref distance closest))
                 n
                 closest))
           0
           (range (vector-length tiles)))))

(define canvas
  (new
   (class* canvas% ()
     (inherit with-gl-context swap-gl-buffers)
     (define/override (on-paint)
       (with-gl-context
        (thunk
         (let* ([v (screen-to-viewport (vector display-width
                                               display-height))]
                [mx (flvector-ref v 0)]
                [my (flvector-ref v 1)])
           (set-gl-ortho-projection (- mx) mx my (- my) -2.0 2.0))
         (gl-clear (list 0.0 0.0 0.0 0.0))
         (gl-draw 'tile-vertices
                  'tile-indices)
         (swap-gl-buffers))))
     (define/override (on-size width height)
       (set! display-width width)
       (set! display-height height)
       (with-gl-context
        (thunk
         (set-gl-viewport 0 0 width height))))
     (define (repaint!)
       (when (fl< milliseconds-between-frames
                  (fl- (current-inexact-milliseconds) last-draw))
         (begin
           (on-paint)
           (set! last-draw (current-inexact-milliseconds)))))
     (define (on-left-mouse-move event)
       (void))
     (define (on-right-mouse-move event)
       (let* ([v (mouse-to-sphere event)]
              [angle (flvector3-angle v mouse-down-vector)])
         (set! current-rotation
               (quaternion-normal
                (quaternion-product
                 (rotation)
                 (axis-angle->quaternion
                  (flvector3-cross-product mouse-down-vector v)
                  angle)))))
       (with-gl-context (thunk (update-vertices!)))
       (repaint!))
     (define (on-mouse-move event)
       (match mouse-button
         ['left (on-left-mouse-move event)]
         ['right (on-right-mouse-move event)]
         [#f (void)])
       (set! last-mouse-x (send event get-x))
       (set! last-mouse-y (send event get-y)))
     (define (on-mouse-click event)
       (void))
     (define (on-left-mouse-down event)
       (unless mouse-button
         (set! mouse-button 'left))
       (on-mouse-down event))
     (define (on-right-mouse-down event)
       (unless (and mouse-moving?
                    mouse-button)
         (set! mouse-button 'right)
         (set! mouse-down-vector (mouse-to-sphere event)))
       (on-mouse-down event))
     (define (on-mouse-down event)
       (set! mouse-down-x (send event get-x))
       (set! mouse-down-y (send event get-y)))
     (define (on-mouse-up event)
       (set! mouse-button #f))
     (define (on-left-mouse-up event)
       (on-mouse-up event))
     (define (on-right-mouse-up event)
       (on-mouse-up event))
     (define (zoom-in!)
       (set! scale
             (min scale-max
                  (* scale 1.05)))
       (repaint!))
     (define (zoom-out!)
       (set! scale
             (max scale-min
                  (/ scale 1.05)))
       (repaint!))
     (define/override (on-char event)
       (define key-code (send event get-key-code))
       (match key-code
         ['escape (exit)]
         [_ (void)]))
     (define/override (on-event event)
       (match (send event get-event-type)
         ['left-down (on-left-mouse-down event)]
         ['left-up (on-left-mouse-up event)]
         ['right-down (on-right-mouse-down event)]
         ['right-up (on-right-mouse-up event)]
         ['motion (on-mouse-move event)]
         [_ (void)]))
     (super-instantiate () (style '(gl))))
   [parent frame]))

(define gl-context canvas)
(send canvas with-gl-context (thunk (make-tile-buffers!)
                                    (update-vertices!)))

(send frame maximize #t)
(send frame show #t)
(send canvas focus)
(send canvas on-paint)
