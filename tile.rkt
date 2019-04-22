#lang typed/racket

(require "color.rkt")

(provide (struct-out tile))

(struct tile
  ([color : flcolor]
   [position : (Vectorof Integer)]
   [normal : (Vectorof Integer)]
   [rotation : FlVector]
   [center-vertex : FlVector]
   [edge-vertices : (Vectorof FlVector)]))
