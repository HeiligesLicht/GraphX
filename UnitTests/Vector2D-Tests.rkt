#lang racket

(require "../Dependencies/Vector2D.rkt")
(require rackunit)


;; make-vect tests
(check-equal? (make-vect 4 5) '(4 . 5))
(check-equal? (make-vect -1000 -20428304) '(-1000 . -20428304))
(check-equal? (make-vect -1000.9999999 -20428304) '(-1000.9999999 . -20428304))
(check-exn exn:fail? (lambda () (make-vect "a" #\x)))

;; coord-x, coord-y tests

(define v1 (make-vect -1000 -20428304))
(define v2 (make-vect 0 -1))
(define v3 (make-vect -0.000001 0.000001))
(define v4 (make-vect -0.999999 12.111119))


(check-equal? (coord-x v1) -1000)
(check-equal? (coord-y v1) -20428304)

(check-equal? (coord-x v2) 0)
(check-equal? (coord-y v2) -1)

;; vect-sum test

(check-equal? (vect-sum v1 v2) '(-1000 . -20428305))
(check-equal? (vect-sum v3 v4) '(-1.0 . 12.11112))

;; vect-sum* test

(check-exn exn:fail? (lambda () (vect-sum*)))
(check-equal? (vect-sum* v1 v2) (vect-sum v1 v2))
(check-equal? (vect-sum* v2 v4) (vect-sum v2 v4))
(check-equal? (vect-sum* v3 v4) (vect-sum v3 v4))
(check-equal? (vect-sum* v2) v2)
(check-equal? (vect-sum* v3) v3)
(check-equal? (vect-sum* v4) v4)

(check-= (coord-x (vect-sum* v1 v2 v3 v4))
         (coord-x (vect-sum
                   (vect-sum v1 v2)
                   (vect-sum v3 v4))) 1e-5)

(check-= (coord-y (vect-sum* v1 v2 v3 v4))
         (coord-y (vect-sum
                   (vect-sum v1 v2)
                   (vect-sum v3 v4))) 1e-5)

;; vect-scalar tests

(check-equal? (vect-scalar -1 v1) '(1000 . 20428304))
(check-equal? (vect-scalar 0.5 (make-vect 0 0)) '(0 . 0))
(check-equal? (vect-scalar (/ 2) (make-vect 1 1)) '(1/2 . 1/2))
(check-equal? (vect-scalar (/ -2) (make-vect -2 -4)) '(1 . 2))
(check-equal? (vect-scalar pi (make-vect 1 1)) `(,pi . ,pi))

;; vect-norm and vect-unit tests

(check-= (vect-norm (vect-unit v1)) 1 1e-10)
(check-= (vect-norm (vect-unit v2)) 1 1e-12)
(check-= (vect-norm (vect-unit v3)) 1 1e-14)
(check-= (vect-norm (vect-unit v4)) 1 1e-16)

;; distance^2 tests

(check-= (sqrt (distance^2 '(0 . 0) v1)) (vect-norm v1) 1e-10)
(check-= (sqrt (distance^2 '(0 . 0) v2)) (vect-norm v2) 1e-10)
(check-= (sqrt (distance^2 '(0 . 0) v3)) (vect-norm v3) 1e-10)
(check-= (distance^2 (make-vect 0 1) (make-vect 1 0)) 2 1e-10)
(check-= (distance^2 (make-vect 0 1) (make-vect 2 0)) 5 1e-10)