#lang racket

(provide (all-defined-out))
; We export all the defined modules present in this file.
; Exported Modules: coord-x, coord-y, vect-sum, vect-sum*, vect-scalar, vect-norm, vect-unit


; Vector2D abstract type constructor.
; R * R --> Vect2D
(define (make-vect x y)
  (if (and (number? x) (number? y))
      (cons x y)
      (error "Vector components need to be numbers!")))

; x coordinate of a vector.
; Vect2D --> R
(define (coord-x v)
  (car v))

; x coordinate of a vector
; Vect2D --> R
(define (coord-y v)
  (cdr v))

; The sum of two vectors
; Vect2D * Vect2D --> Vect2D
(define (vect-sum v1 v2)
  (make-vect (+ (coord-x v1) (coord-x v2)) (+ (coord-y v1) (coord-y v2))))

; The sum of 1 or multiple vectors.
; Vect2D ^ N* --> Vect2D (Where N is the number of arguments that the function takes.)
(define (vect-sum* . L)
  (if (empty? L)
      (error "vect-sum* takes at least 1 argument!")
      (make-vect (apply + (map (lambda (v) (coord-x v)) L)) (apply + (map (lambda (v) (coord-y v)) L)))))

; Vector multiplied by some constant k.
; R * Vect2D --> Vect2D
(define (vect-scalar k v)
  (make-vect (* k (coord-x v)) (* k (coord-y v))))

; Vector's norm.
; Vect2D --> R
(define (vect-norm v)
  (sqrt (+ (* (coord-x v) (coord-x v)) (* (coord-y v) (coord-y v)))))

; Unit vector which norm is equal to 1.
; Vect2D --> Vect2D
(define (vect-unit v)
  (vect-scalar (/ 1 (vect-norm v)) v))

; Square distance between two vectors.
(define (distance^2 vect1 vect2)
  (+ (sqr (- (coord-x vect1) (coord-x vect2)))
     (sqr (- (coord-y vect1) (coord-y vect2)))))