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
                    (struct-copy tile t
                                 [face (face-of tiles (rotate (tile-position t)) (rotate (tile-normal t)))])))
              tiles))
