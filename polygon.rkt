#lang typed/racket

(require math/flonum)

(provide point-in-polygon?)

(: x (-> FlVector Flonum))
(define (x v)
  (flvector-ref v 0))

(: y (-> FlVector Flonum))
(define (y v)
  (flvector-ref v 1))

(: point-in-polygon? (-> FlVector (Vectorof FlVector) Boolean))
(define (point-in-polygon? point polygon)
  (: f (-> Integer Boolean Boolean))
  (define (f n b)
    (if (= n (vector-length polygon))
        b
        (let* ([v2 (vector-ref polygon n)]
               [v1 (vector-ref polygon (modulo (+ 1 n) (vector-length polygon)))]
               [intersects? (and (xor (fl> (y v1) (y point))
                                      (fl> (y v2) (y point)))
                                 (fl< (x point)
                                      (fl+ (x v1)
                                           (fl/ (fl* (fl- (x v2) (x v1))
                                                     (fl- (y point) (y v1)))
                                                (fl- (y v2)
                                                     (y v1))))))])
          (f (+ 1 n)
             (if intersects? (not b) b)))))
  (f 0 #f))
