#lang racket

(require "../Dependencies/Vector3D.rkt")
(require rackunit)


;; make-vect3D tests
(check-equal? (make-vect3D 4 5 1) (vector 4 5 1))
(check-equal? (make-vect3D -10 23 40) (vector -10 23 40))
(check-equal? (make-vect3D -10.99 23.11 40.0) (vector -10.99 23.11 40.0))
(check-exn exn:fail? (lambda () (make-vect3D "a" #\x 'z)))

;; coord-x3D, coord-y3D, coord-z3D tests

(define v1 (make-vect3D 4 5 1))
(define v2 (make-vect3D 0 -1 1))
(define v3 (make-vect3D -10.99 23.11 40.0))
(define v4 (make-vect3D -0.01 0.89 0))


(check-equal? (coord-x3D v1) 4)
(check-equal? (coord-y3D v1) 5)
(check-equal? (coord-z3D v1) 1)

(check-equal? (coord-x3D v3) -10.99)
(check-equal? (coord-y3D v3) 23.11)
(check-equal? (coord-z3D v3) 40.0)

;; vect-sum3D test

(check-equal? (vect-sum3D v1 v2) (vector 4 4 2))
(check-equal? (vect-sum3D v3 v4) (vector -11.0 24.0 40.0))

;; vect-sum3D* test

(check-exn exn:fail? (lambda () (vect-sum3D*)))
(check-equal? (vect-sum3D* v1 v2) (vect-sum3D v1 v2))
(check-equal? (vect-sum3D* v2 v4) (vect-sum3D v2 v4))
(check-equal? (vect-sum3D* v3 v4) (vect-sum3D v3 v4))
(check-equal? (vect-sum3D* v2) v2)
(check-equal? (vect-sum3D* v3) v3)
(check-equal? (vect-sum3D* v4) v4)

(check-= (coord-x3D (vect-sum3D* v1 v2 v3 v4))
         (coord-x3D (vect-sum3D
                   (vect-sum3D v1 v2)
                   (vect-sum3D v3 v4))) 1e-5)

(check-= (coord-y3D (vect-sum3D* v1 v2 v3 v4))
         (coord-y3D (vect-sum3D
                   (vect-sum3D v1 v2)
                   (vect-sum3D v3 v4))) 1e-5)

;; vect-scalar3D tests

(check-equal? (vect-scalar3D -1 v1) (vector -4 -5 -1))
(check-equal? (vect-scalar3D 0.5 (make-vect3D 0 0 0)) (vector 0  0 0))
(check-equal? (vect-scalar3D (/ 2) (make-vect3D 1 1 0)) (vector 1/2 1/2 0))
(check-equal? (vect-scalar3D (/ -2) (make-vect3D -2 -4 1)) (vector 1 2 -1/2))
(check-equal? (vect-scalar3D pi (make-vect3D 1 1 -1)) (vector pi pi (- pi)))

;; vect-norm3D and vect-unit3D tests

(check-= (vect-norm3D (vect-unit3D v1)) 1 1e-10)
(check-= (vect-norm3D (vect-unit3D v2)) 1 1e-12)
(check-= (vect-norm3D (vect-unit3D v3)) 1 1e-14)
(check-= (vect-norm3D (vect-unit3D v4)) 1 1e-16)

;; distance3D^2 tests

(check-= (sqrt (distance3D^2 (vector 0 0 0) v1)) (vect-norm3D v1) 1e-10)
(check-= (sqrt (distance3D^2 (vector 0 0 0) v2)) (vect-norm3D v2) 1e-10)
(check-= (sqrt (distance3D^2 (vector 0 0 0) v3)) (vect-norm3D v3) 1e-10)
(check-= (distance3D^2 (make-vect3D 0 1 1) (make-vect3D 1 0 1)) 2 1e-10)
(check-= (distance3D^2 (make-vect3D 0 1 1) (make-vect3D 2 0 1)) 5 1e-10)