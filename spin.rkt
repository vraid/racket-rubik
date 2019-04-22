#lang racket

(require "tile.rkt")

(provide spin)

(define (face-of tiles faces position normal)
  (define (find n)
    (let ([t (vector-ref tiles n)])
      (if (and (equal? normal (tile-normal t))
               (equal? position (tile-position t)))
          (vector-ref faces n)
          (find (+ n 1)))))
  (find 0))

(define (spin tiles faces axis in-spin? rotate)
  (map (λ (n)
         (let ([t (vector-ref tiles n)])
           (if (not (in-spin? t))
               (vector-ref faces n)
               (face-of tiles faces (rotate (tile-position t)) (rotate (tile-normal t))))))
       (range (vector-length tiles))))
