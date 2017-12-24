#lang racket
(require "Vector2D.rkt")
(provide (all-defined-out))

; Vector3D abstract type constructor.
; R * R * R --> Vect3D
(define (make-vect3D x y z)
  (if (and (number? x) (number? y) (number? z))
      (vector x y z)
      (error "Vector3D components need to be numbers!")))

; x coordinate of a 3D vector.
; Vect3D --> R
(define (coord-x3D v)
  (vector-ref v 0))

; y coordinate of a 3D vector.
; Vect3D --> R
(define (coord-y3D v)
  (vector-ref v 1))

; z coordinate of a 3D vector.
; Vect3D --> R
(define (coord-z3D v)
  (vector-ref v 2))

; The sum of two 3D vectors
; Vect3D * Vect3D --> Vect3D
(define (vect-sum3D v1 v2)
  (make-vect3D (+ (coord-x3D v1) (coord-x3D v2)) (+ (coord-y3D v1) (coord-y3D v2)) (+ (coord-z3D v1) (coord-z3D v2))))

; The sum of 1 or multiple 3D vectors.
; Vect3D ^ N* --> Vect3D (Where N is the number of arguments that the function takes.)
(define (vect-sum3D* . L)
  (if (empty? L)
      (error "vect-sum3D* takes at least 1 argument!")
      (make-vect3D (apply + (map (lambda (v) (coord-x3D v)) L))
                   (apply + (map (lambda (v) (coord-y3D v)) L))
                   (apply + (map (lambda (v) (coord-z3D v)) L)))))

; 3D Vector multiplied by some constant k.
; R * Vect3D --> Vect3D
(define (vect-scalar3D k v)
  (make-vect3D (* k (coord-x3D v))
               (* k (coord-y3D v))
               (* k (coord-z3D v))))

; 3D Vector's norm.
; Vect3D --> R
(define (vect-norm3D v)
  (sqrt (+ (* (coord-x3D v) (coord-x3D v))
           (* (coord-y3D v) (coord-y3D v))
           (* (coord-z3D v) (coord-z3D v)))))

; Unit vector which norm is equal to 1.
; Vect3D --> Vect3D
(define (vect-unit3D v)
  (vect-scalar3D (/ 1 (vect-norm3D v)) v))

; Square distance between two 3D vectors.
(define (distance3D^2 vect1 vect2)
  (+ (sqr (- (coord-x3D vect1) (coord-x3D vect2)))
     (sqr (- (coord-y3D vect1) (coord-y3D vect2)))
     (sqr (- (coord-z3D vect1) (coord-z3D vect2)))))