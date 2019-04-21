#lang typed/racket

(provide (all-defined-out))

(require math/flonum)

(struct: byte-color
  ([red : Byte]
   [green : Byte]
   [blue : Byte])
  #:transparent)

(struct: flcolor
  ([red : Flonum]
   [green : Flonum]
   [blue : Flonum]
   [alpha : Flonum])
  #:transparent)

(: flcolor->list (flcolor -> (Listof Flonum)))
(define (flcolor->list color)
  (list (flcolor-red color)
        (flcolor-green color)
        (flcolor-blue color)
        (flcolor-alpha color)))

(: flcolor->byte (Flonum -> Byte))
(define (flcolor->byte c)
  (let ([b (max 0
                (min 255
                     (inexact->exact
                      (round (fl* 255.0 c)))))])
    (if (byte? b)
        b
        0)))

(: flcolor->byte-color (flcolor -> byte-color))
(define (flcolor->byte-color c)
  (byte-color
   (flcolor->byte (flcolor-red c))
   (flcolor->byte (flcolor-green c))
   (flcolor->byte (flcolor-blue c))))

(define flcolor-interpolate
  (λ ([col-one : flcolor]
      [col-two : flcolor]
      [d : Flonum])
    (let* ([1-d (fl- 1.0 d)]
           [f (λ ([f : (flcolor -> Flonum)])
                (fl+ (fl* 1-d (f col-one))
                     (fl* d (f col-two))))])
      (flcolor (f flcolor-green)
               (f flcolor-blue)
               (f flcolor-red)
               (f flcolor-alpha)))))
