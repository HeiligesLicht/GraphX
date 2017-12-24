#lang racket

(require "../Dependencies/GraphGenerators.rkt")
(require rackunit)

;chain-graph test
(define graph-test1 (make-hash (list (cons 0 (mutable-set 1)) (cons 1 (mutable-set 0 2)) (cons 2 (mutable-set 1)))))
(check-equal? (chain-graph 3) graph-test1)
(check-equal? (chain-graph 0) (make-hash))

;cyclic graph test
(define graph-test2 (make-hash (list (cons 0 (mutable-set 1 2)) (cons 1 (mutable-set 0 2)) (cons 2 (mutable-set 0 1)))))
(check-equal? (cyclic-graph 3) graph-test2)
(check-equal? (cyclic-graph 0) (make-hash))

;complete tree graph
(define graph-test3 (make-hash (list (cons 0 (mutable-set 1 2)) (cons 1 (mutable-set 3 4 0)) (cons 2 (mutable-set 5 6 0))
                                     (cons 3 (mutable-set 1)) (cons 4 (mutable-set 1)) (cons 5 (mutable-set 2)) (cons 6 (mutable-set 2)))))
(check-equal? (complete-tree-graph 2 2) graph-test3)
(check-equal? (complete-tree-graph 0 0) (make-hash (list (cons 0 (mutable-set)))))

;grid graph
(define graph-test4 (make-hash (list (cons 0 (mutable-set 1 2)) (cons 1 (mutable-set 0 3)) (cons 2 (mutable-set 0 3)) (cons 3 (mutable-set 1 2)))))
(check-equal? (grid-graph 2 2) graph-test4)
(check-equal? (grid-graph 0 0) (make-hash))

;clique-graph
(define graph-test5 (make-hash (list (cons 0 (mutable-set 1 2 3)) (cons 1 (mutable-set 0 2 3)) (cons 2 (mutable-set 0 1 3)) (cons 3 (mutable-set 0 1 2)))))
(check-equal? (clique-graph 4) graph-test5)
(check-equal? (clique-graph 0) (make-hash))