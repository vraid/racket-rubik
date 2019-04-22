#lang racket

(require "tile.rkt")

(provide spin)

(define (color-of tiles position normal)
  (define (find n)
    (let ([t (vector-ref tiles n)])
      (if (and (equal? normal (tile-normal t))
               (equal? position (tile-position t)))
          (tile-color t)
          (find (+ n 1)))))
  (find 0))

(define (spin tiles axis in-spin? rotate)
  (vector-map (λ (t)
                (if (not (in-spin? t))
                    t
                    (let ([position (tile-position t)]
                          [normal (tile-normal t)])
                      (tile
                       (color-of tiles (rotate position) (rotate normal))
                       position
                       normal
                       (tile-rotation t)
                       (tile-center-vertex t)
                       (tile-edge-vertices t)))))
              tiles))
