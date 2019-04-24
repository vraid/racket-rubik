#lang racket

(require racket/gui/base
         math/flonum
         "flvector3.rkt"
         "matrix3.rkt"
         "quaternion.rkt"
         "discrete-algebra.rkt"
         "color.rkt"
         "tile.rkt"
         "geometry.rkt"
         "spin.rkt"
         "polygon.rkt"
         "stereographic-projection.rkt"
         "opengl.rkt"
         "render.rkt")

(define vertex-count 16)
(define tile-vertex-count (+ 1 (* 4 vertex-count)))

(define current-rotation (quaternion-identity))
(define next-rotation (quaternion-identity))
(define (rotation) current-rotation)
(define in-current-spin? (thunk* #f))
(define spin-rotation (quaternion-identity))
(define scale 0.13)

(define top-tile #f)
(define animating? #f)
(define mouse-down-tile #f)
(define mouse-down-vector #f)
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
  (vector
   (flcolor 1.0 1.0 0.0 1.0)
   (flcolor 1.0 0.0 0.0 1.0)
   (flcolor 0.0 1.0 0.0 1.0)
   (flcolor 1.0 0.0 1.0 1.0)
   (flcolor 0.0 0.0 1.0 1.0)
   (flcolor 1.0 1.0 1.0 1.0)))

(define faces
  (apply vector-append
         (map (λ (n)
                (make-vector 9 n))
              (range 6))))

(define face-color (curry vector-ref rubik-colors))
(define (tile-color n)
  (face-color (vector-ref faces n)))

(define tiles (make-tiles vertex-count))

(define normal (curry vector-ref (vector-map tile-normal tiles)))
(define position (curry vector-ref (vector-map tile-position tiles)))

(define (spin-tiles! in-spin? rotate)
  (let* ([tile-range (range (vector-length tiles))]
         [post-spin (post-spin-tile rotate normal position)]
         [permutation (post-spin-permutation tile-range in-spin? post-spin)])
    (set! faces (list->vector
                 (map (curry vector-ref faces)
                      permutation)))))

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

(define (closest-tile tiles event)
  (let ([v (mouse-to-sphere (rotation) event)])
    (argmin (λ (t)
              (flvector3-distance-squared (quaternion-vector-product (tile-rotation t) v) (tile-center-vertex t)))
            tiles)))

(define (neighbouring-tiles tile)
  (define (neighbours? t)
    (>= (if (equal? (tile-normal t)
                    (tile-normal tile))
            1
            0)
        (discrete-vector-distance (tile-position t)
                                  (tile-position tile))))
  (filter neighbours? (vector->list tiles)))

(define (rotation-axis a b)
  (if (equal? (tile-normal a)
              (tile-normal b))
      (discrete-vector-cross-product
       (discrete-vector-subtract (tile-position a)
                                 (tile-position b))
       (discrete-vector-product
        (discrete-vector-product (tile-normal a) (tile-normal a))
        (tile-position a)))
      (discrete-vector-cross-product
       (tile-normal b)
       (tile-normal a))))

(define (mouse-to-sphere rotation event)
  (quaternion-vector-product
   (quaternion-inverse rotation)
   (inverse-stereographic-projection
    (screen-to-viewport (vector (send event get-x)
                                (send event get-y))))))

(define (rotate-tile tile)
  (let ([m (quaternion->matrix3 (quaternion-product 
                                 (if (in-current-spin? tile)
                                     (quaternion-product (rotation) spin-rotation)
                                     (rotation))
                                 (quaternion-inverse (tile-rotation tile))))])
    (curry matrix3-vector3-product m)))

(define update-vertices
  (thunk
   (set-top-tile!)
   (set-gl-vertex-buffer! 'tile-vertices ((updated-vertices tiles tile-color vertex-count) top-tile rotate-tile))))

(define (set-top-tile!)
  (letrec ([rec (λ (n)
                  (if (= n (* 6 9))
                      #f
                      (let* ([tile (vector-ref tiles n)]
                             [rotate (rotate-tile tile)])
                        (if (and (fl< 0.8 (flvector-ref (rotate (tile-center-vertex tile)) 2))
                                 (point-in-polygon? (flvector 0.0 0.0 0.0)
                                                    (vector-map rotate (tile-edge-vertices tile))))
                            n
                            (rec (+ n 1))))))])
    (set! top-tile (rec 0))))

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
            (gl-clear (if top-tile
                          (flcolor->list (tile-color top-tile))
                          (list 0.0 0.0 0.0 0.0)))
            (gl-draw 'top-tile-vertices
                     'top-tile-indices)
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
           (set! current-rotation next-rotation)
           (with-gl-context update-vertices)
           (on-paint)
           (set! last-draw (current-inexact-milliseconds)))))
     
     (define (on-left-mouse-move event)
       (void))
     
     (define (on-right-mouse-move event)
       (let* ([v (mouse-to-sphere next-rotation event)]
              [angle (flvector3-angle v mouse-down-vector)])
         (set! next-rotation
               (quaternion-normal
                (quaternion-product
                 next-rotation
                 (axis-angle->quaternion
                  (flvector3-cross-product v mouse-down-vector)
                  angle)))))
       (repaint!))
     
     (define (on-mouse-move event)
       (match mouse-button
         ['left (on-left-mouse-move event)]
         ['right (on-right-mouse-move event)]
         [#f (void)]))
     
     (define (on-left-mouse-down event)
       (when (or (not mouse-button)
                 (eq? mouse-button 'left))
         (set! mouse-button 'left)
         (set! mouse-down-tile (closest-tile (vector->list tiles) event))))
     
     (define (on-right-mouse-down event)
       (set! mouse-button 'right)
       (set! mouse-down-vector (mouse-to-sphere (rotation) event)))
     
     (define (on-mouse-up event)
       (set! mouse-button #f))
     
     (define (on-left-mouse-up event)
       (when (eq? mouse-button 'left)
         (let* ([t (closest-tile (neighbouring-tiles mouse-down-tile) event)]
                [normal (tile-normal t)])
           (unless (or animating?
                       (eq? t mouse-down-tile))
             (set! animating? #t)
             (let* ([axis (rotation-axis t mouse-down-tile)]
                    [v (discrete-vector-product axis (tile-position t))]
                    [in-rotation? (λ (tile)
                                    (equal? v (discrete-vector-product axis (tile-position tile))))])
               (if (= 1 (discrete-vector-distance (vector 0 0 0) axis))
                   (letrec ([t (new timer%
                                    [notify-callback (thunk
                                                      (if (finished?)
                                                          (on-finish)
                                                          (on-step)))]
                                    [just-once? #f])]
                            [acceleration (* 0.000002 pi)]
                            [distance (* 0.5 pi)]
                            [start-time (current-inexact-milliseconds)]
                            [elapsed-time (thunk (- (current-inexact-milliseconds)
                                                    start-time))]
                            [time (* 2 (sqrt (/ (/ distance 2) acceleration)))]
                            [on-step (thunk
                                      (set! in-current-spin? in-rotation?)
                                      (set! spin-rotation (axis-angle->quaternion
                                                           (vector->flvector axis)
                                                           (* 0.5 acceleration (expt (elapsed-time) 2.0))))
                                      (repaint!))]
                            [finished? (thunk (<= time (elapsed-time)))]
                            [on-finish (thunk
                                        (send t stop)
                                        (spin-tiles! (compose in-rotation? (curry vector-ref tiles))
                                                     (curry discrete-matrix-vector-product (discrete-rotation-matrix axis 1)))
                                        (set! in-current-spin? (thunk* #f))
                                        (set! animating? #f)
                                        (with-gl-context update-vertices)
                                        (on-paint))])
                     (send t start (inexact->exact (round milliseconds-between-frames))))
                   (set! animating? #f))))))
       (set! mouse-down-tile #f)
       (on-mouse-up event))
     
     (define (on-right-mouse-up event)
       (set! next-rotation current-rotation)
       (on-mouse-up event))
     
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

(send canvas with-gl-context (thunk (set-gl-vertex-buffer! 'top-tile-vertices
                                                           (make-tile-vertices vertex-count))))
(send canvas with-gl-context (thunk (set-gl-index-buffer! 'top-tile-indices
                                                          (make-tile-indices vertex-count))))

(send canvas with-gl-context (thunk (make-tile-buffers tiles vertex-count)
                                    (update-vertices)))

(send frame maximize #t)
(send frame show #t)
(send canvas focus)
(send canvas on-paint)
