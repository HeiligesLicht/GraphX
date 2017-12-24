#lang racket

(require rackunit)
(require "../Dependencies/Relaxation.rkt")
(require "../Dependencies/Positioning.rkt")
(require "../Dependencies/GraphGenerators.rkt")

(define relax-test (new-relaxator))

;check default value of the relaxator

(check-equal? (relax-test 'get-c1) 2)
(check-equal? (relax-test 'get-c2) 20)
(check-equal? (relax-test 'get-c3) 400)
(check-equal? (relax-test 'get-c4) 1)

;check value change

(relax-test 'set-c1 10)
(check-equal? (relax-test 'get-c1) 10)
(relax-test 'set-c2 30)
(check-equal? (relax-test 'get-c2) 30)
(relax-test 'set-c3 900)
(check-equal? (relax-test 'get-c3) 900)
(relax-test 'set-c4 0.5)
(check-equal? (relax-test 'get-c4) 0.5)

;check if positioning move the node

(define pos1 (random-positioning-of-node-list 40 40 '(0 1 2 3)))
(define pos2 (hash-copy pos1))
(check-equal? pos1 pos2)
(relax-test 'relax (grid-graph 2 2) pos1 (mutable-set))
(check-equal? (equal? pos1 pos2) #f)

