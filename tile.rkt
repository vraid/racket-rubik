#lang typed/racket

(require "quaternion.rkt")

(provide (struct-out tile))

(struct tile
  ([position : (Vectorof Integer)]
   [normal : (Vectorof Integer)]
   [rotation : quaternion]
   [center-vertex : FlVector]
   [edge-vertices : (Vectorof FlVector)]))
