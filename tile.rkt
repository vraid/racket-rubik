#lang typed/racket

(require "color.rkt"
         "quaternion.rkt")

(provide (struct-out tile))

(struct tile
  ([color : flcolor]
   [position : (Vectorof Integer)]
   [normal : (Vectorof Integer)]
   [rotation : quaternion]
   [center-vertex : FlVector]
   [edge-vertices : (Vectorof FlVector)]))
