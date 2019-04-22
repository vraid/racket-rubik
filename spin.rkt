#lang racket

(require "tile.rkt")

(provide spin)

(define (face-of tiles position normal)
  (define (find n)
    (let ([t (vector-ref tiles n)])
      (if (and (equal? normal (tile-normal t))
               (equal? position (tile-position t)))
          (tile-face t)
          (find (+ n 1)))))
  (find 0))

(define (spin tiles axis in-spin? rotate)
  (vector-map (λ (t)
                (if (not (in-spin? t))
                    t
                    (let ([position (tile-position t)]
                          [normal (tile-normal t)])
                      (tile
                       (face-of tiles (rotate position) (rotate normal))
                       position
                       normal
                       (tile-rotation t)
                       (tile-center-vertex t)
                       (tile-edge-vertices t)))))
              tiles))
