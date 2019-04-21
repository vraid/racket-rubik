#lang typed/racket

(require "flvector3-local.rkt"
         math/flonum)

(provide (all-defined-out))

(: flvector3-zero (-> FlVector))
(define (flvector3-zero)
  (flvector 0.0 0.0 0.0))

(: flvector3-zero? (FlVector -> Boolean))
(define (flvector3-zero? v)
  (zero? (flvector3-length-squared v)))

(: flvector3-negative (FlVector -> FlVector))
(define (flvector3-negative v)
  (flvector3-scale -1.0 v))

(: flvector3-length-squared (FlVector -> Float))
(define (flvector3-length-squared v)
  (flvector-sum
   (flvector-sqr v)))

(: flvector3-length (FlVector -> Float))
(define (flvector3-length v)
  (flsqrt 
   (flvector3-length-squared v)))

(: flvector3-distance-squared (FlVector FlVector -> Float))
(define (flvector3-distance-squared u v)
  (flvector3-length-squared (flvector- v u)))

(: flvector3-distance (FlVector FlVector -> Float))
(define (flvector3-distance u v)
  (flvector3-length (flvector- v u)))

(: flvector3-scale (Float FlVector -> FlVector))
(define (flvector3-scale a v)
  (flvector-scale v a))

(: flvector3-scale-to (Float FlVector -> FlVector))
(define (flvector3-scale-to a v)
  (flvector3-scale (/ a (flvector3-length v))
                   v))

(: flvector3-normal (FlVector -> FlVector))
(define (flvector3-normal a)
  (if (zero? (flvector3-length-squared a))
      a
      (flvector3-scale (/ (flvector3-length a)) a)))

(: flvector3-sum (FlVector * -> FlVector))
(define (flvector3-sum . vecs)
  (foldl flvector+
         (flvector3-zero) vecs))

(: flvector3-subtract (FlVector FlVector -> FlVector))
(define (flvector3-subtract a b)
  (flvector- a b))

(: flvector3-subtract-by (FlVector FlVector -> FlVector))
(define (flvector3-subtract-by a b)
  (flvector- b a))

(: flvector3-dot-product (FlVector FlVector -> Float))
(define (flvector3-dot-product u v)
  (flvector-sum
   (flvector-map * u v)))

(: flvector3-cross-product (FlVector FlVector -> FlVector))
(define (flvector3-cross-product u v)
  (let* ([m (vector 1 2 0)]
         [n (vector 2 0 1)])
    (flvector- (col u m v n)
               (col u n v m))))

(: flvector3-angle (FlVector FlVector -> Float))
(define (flvector3-angle a b)
  (flacos (fl/ (flvector3-dot-product a b)
               (flsqrt (* (flvector3-length-squared a)
                          (flvector3-length-squared b))))))

(: flvector3-projection (FlVector FlVector -> FlVector))
(define (flvector3-projection target v)
  (let ([n (flvector3-normal target)])
    (flvector3-scale (flvector3-dot-product n v) n)))

(: flvector3-rejection (FlVector FlVector -> FlVector))
(define (flvector3-rejection rejector v)
  (flvector- v (flvector3-projection rejector v)))
