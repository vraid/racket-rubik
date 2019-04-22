#lang racket

(require racket/gui/base
         math/flonum
         "flvector3.rkt"
         "stereographic-projection.rkt"
         "geometry.rkt"
         "quaternion.rkt"
         "matrix3.rkt"
         "color.rkt"
         "spin.rkt"
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
(define scale-max 100.0)
(define scale-min 0.5)

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

(define tiles (make-tiles vertex-count))

(define (spin-tiles! axis in-spin? rotate)
  (set! tiles (spin tiles axis in-spin? rotate)))

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
              (flvector3-distance-squared (matrix3-vector3-product (tile-rotation t) v) (tile-center-vertex t)))
            tiles)))

(define (vector-distance v u)
  (vector-sum (vector-map abs (vector-subtract v u))))

(define (neighbouring-tiles tile)
  (define (neighbours? t)
    (>= (if (equal? (tile-normal t)
                    (tile-normal tile))
            1
            0)
        (vector-distance (tile-position t)
                         (tile-position tile))))
  (filter neighbours? (vector->list tiles)))

(define (rotation-axis a b)
  (if (equal? (tile-normal a)
              (tile-normal b))
      (vector-cross-product
       (vector-subtract (tile-position a)
                        (tile-position b))
       (vector-product
        (vector-product (tile-normal a) (tile-normal a))
        (tile-position a)))
      (vector-cross-product
       (tile-normal b)
       (tile-normal a))))

(define (mouse-to-sphere rotation event)
  (quaternion-vector-product
   (quaternion-inverse rotation)
   (inverse-stereographic-projection
    (screen-to-viewport (vector (send event get-x)
                                (send event get-y))))))

(define (rotate-tile tile)
  (let ([m (matrix3-product (quaternion->matrix3
                             (if (in-current-spin? tile)
                                 (quaternion-product (rotation) spin-rotation)
                                 (rotation)))
                            (tile-rotation tile))])
    (curry matrix3-vector3-product m)))

(define update-vertices
  (thunk
   (set-top-tile!)
   (set-gl-vertex-buffer! 'tile-vertices ((updated-vertices tiles vertex-count) top-tile rotate-tile))))

(define (point-in-polygon? point polygon)
  (define (x v)
    (flvector-ref v 0))
  (define (y v)
    (flvector-ref v 1))
  (define (rec n b)
    (if (= n (vector-length polygon))
        b
        (let ([v2 (vector-ref polygon n)]
              [v1 (vector-ref polygon (modulo (+ 1 n) (vector-length polygon)))])
          (if (and (xor (fl> (y v1) (y point))
                        (fl> (y v2) (y point)))
                   (fl< (x point)
                        (fl+ (x v1)
                             (fl/ (fl* (fl- (x v2) (x v1))
                                       (fl- (y point) (y v1)))
                                  (fl- (y v2)
                                       (y v1))))))
              (rec (+ 1 n) (not b))
              (rec (+ 1 n) b)))))
  (rec 0 #f))

(define (set-top-tile!)
  (letrec ([rec (λ (n)
                  (if (= n (* 6 9))
                      #f
                      (let* ([tile (vector-ref tiles n)]
                             [rotate (rotate-tile tile)])
                        (if (and (fl< 0.8 (flvector-ref (rotate (tile-center-vertex tile)) 2))
                                 (point-in-polygon? (flvector 0.0 0.0 0.0)
                                                    (vector-map rotate (tile-edge-vertices tile))))
                            tile
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
                    [v (vector-product axis (tile-position t))]
                    [in-rotation? (λ (t) (equal? v (vector-product axis (tile-position t))))])
               (if (= 1 (vector-distance (vector 0 0 0) axis))
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
                                        (spin-tiles! axis
                                                     in-rotation?
                                                     (curry matrix-vector-product (rotation-matrix axis 1)))
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

(define gl-context canvas)

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
