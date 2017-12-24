#lang racket

(provide (all-defined-out))

; Empty Graph; G=(V={}, E={})
(define (empty-graph)
  (make-hash))

; Checks if graph is empty or not.
(define (empty-graph? graph)
  (hash-empty? graph))

; Add a node to a graph.
(define (add-node! graph node-id)
  (when (not (hash-has-key? graph node-id))
    (hash-set! graph node-id (mutable-set))))

; Add an edge to a graph (creates nodes if they don't exist).
(define (add-edge! graph node-id1 node-id2)
  (when (not (equal? node-id1 node-id2))
    (add-node! graph node-id1)
    (add-node! graph node-id2)
    (set-add! (hash-ref graph node-id1) node-id2)
    (set-add! (hash-ref graph node-id2) node-id1)))

; Returns the list of all the nodes in the graph.
(define (get-nodes graph)
  (hash-keys graph))

; Returns the list of all neighbors of a node in the graph.
(define (get-neighbors graph node-id)
  (set->list (hash-ref graph node-id)))

; Returns a list of node1-node2 pairs if node2 is node1's neighbor. 
(define (get-edges graph)
  (let ((L '()))
    (for ([(k v) (in-hash graph)])
      (for ([(n) (in-set v)])
        (set! L (cons (cons k n) L))))
    L))

; Removes a node from the graph and all its connecting edges.
(define (rm-node! graph node-id)
  (hash-remove! graph node-id)
  (for ([(k st) (in-hash graph)])
    (set-remove! st node-id)))

; Removes an edge from the graph.
(define (rm-edge! graph node-id1 node-id2)
  (set-remove! (hash-ref graph node-id1) node-id2)
  (set-remove! (hash-ref graph node-id2) node-id1))

; Prints the graph.
(define (print-graph graph)
  (for ([(k v) (in-hash graph)])
    (printf "~a — ( " k)
    (for ([(n) (in-set v)])
      (printf "~a " n))
    (printf ")\n")))