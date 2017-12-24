#lang racket

(require "../Dependencies/Positioning.rkt")
(require "../Dependencies/Vector2D.rkt")
(require "../Dependencies/Vector3D.rkt")
(require rackunit)


(check-equal? (positioning) (make-hash))

(define pos-test1 (make-hash (list (cons 0 (make-vect 5 9)) (cons 1 (make-vect 9 9)))))
(check-equal? (apply-positioning pos-test1 0) (make-vect 5 9)) 

(define pos-test2 (make-hash (list (cons 0 (make-vect 5 9)) (cons 1 (make-vect 9 9)))))
(define list-test1 (list (cons 0 (make-vect 5 9)) (cons 1 (make-vect 9 9))))
(check-equal? (positioning-of-asso-list list-test1) pos-test2)

(random-positioning-of-node-list 1 1 '(0 1)) ; -> rend une fonction positioning avec a chaque une position aléatoire -> ok

(define pos (random-positioning-of-node-list 10 10 '(0 1)))
(define posi (hash-copy pos))  ; define the same positioning than pos
(positioning-move-node! pos 0 (make-vect 1 1)) ; execution of the fonction on pos
(hash-set! posi 0 (vect-sum (make-vect 1 1) (hash-ref posi 0))) ;move node ourself on posi
;expect pos = posi
(check-equal?  pos posi) ; -> ok

(print-positioning '(0 1) pos) ; -> expect: node    position..  -> ok


(define pos3D (make-hash (list (cons 0 (make-vect3D 0 0 0)) (cons 1 (make-vect3D 2 2 2))))) ;exactly the same test for vect3D
(define posi3D (hash-copy pos3D))
(positioning-move-node-3D! pos3D 0 (make-vect3D 1 1 1)) ;function
(hash-set! posi3D 0 (vect-sum3D (hash-ref posi3D 0) (make-vect3D 1 1 1))) ;manual
(check-equal? pos3D posi3D) ; -> ok

