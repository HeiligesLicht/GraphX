#lang racket

(require "../Dependencies/Graph.rkt")
(require rackunit)


(check-equal? (empty-graph) (make-hash))

(check-equal? (empty-graph? (empty-graph)) #t)
(check-equal? (empty-graph? (make-hash '( (cons 0 (mutable-set))))) #f)

; test for add-node

(define graph-test (empty-graph))
(define graph-res (make-hash (list (cons 0 (mutable-set)))))

;test if both graph aren't equals
(check-equal? (equal? graph-test graph-res) #f)

;test if after we add-node to the first one both graph are equals
(add-node! graph-test 0)
(check-equal? graph-test graph-res)

; test if after we add the same node than before, both graph are equals again
(add-node! graph-test 0)
(check-equal? graph-test graph-res)

;test if both graph are equal 
(define graph-test2 (empty-graph))
(define graph-res2 (make-hash (list (cons 0 (mutable-set 1)) (cons 1 (mutable-set 0)))))
(check-equal? (equal? graph-test2 graph-res2) #f)

;test if after we add edges the two nodes are creating and both graph equal
(add-edge! graph-test2 0 1)
(check-equal? graph-test2 graph-res2)

;test if both graph are equal
(define graph-test3 (make-hash (list (cons 0 (mutable-set 1)))))
(define graph-res3 (make-hash (list (cons 0 (mutable-set 1)) (cons 1 (mutable-set 0)))))
(check-equal? (equal? graph-test3 graph-res3) #f)

;test if after we add edgse one of two nodes is created and both graph equal
(add-edge! graph-test3 0 1)
(check-equal? graph-test3 graph-res3)

;test if both graph are equal
(define graph-test4 (make-hash (list (cons 0 (mutable-set 1)))))
(define graph-res4 (make-hash (list (cons 0 (mutable-set 1)) (cons 1 (mutable-set 0)))))
(check-equal? (equal? graph-test4 graph-res4) #f)

;test after we add edge
(add-edge! graph-test4 0 1)
(check-equal? graph-test4 graph-res3)

;test of the get-node function
(check-equal? (get-nodes graph-test4) '(0 1))
(check-equal? (get-nodes (empty-graph)) '())

 ;test of the get neighbors function
(check-equal? (get-neighbors graph-test4 0) '(1))
(check-equal? (get-neighbors graph-test4 1) '(0))

;test of the get edges function
(check-equal? (list->set (get-edges graph-test4)) (list->set(list (cons 0 1) (cons 1 0))))

;test of the remove node function
(rm-node! graph-test4 0)
(check-equal? graph-test4 (make-hash (list (cons 1 (mutable-set)))))
(rm-node! graph-test4 1)
(check-equal? graph-test4 (make-hash))

;test of the remove edge function
(rm-edge! graph-res4 0 1)
(check-equal? graph-res4 (make-hash (list (cons 1 (mutable-set)) (cons 0 (mutable-set)))))

;print graph function

(print-graph graph-res3) ;expect node -> neighbors
;ok