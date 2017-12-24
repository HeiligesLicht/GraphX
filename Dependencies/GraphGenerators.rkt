#lang racket

(require "Graph.rkt")
(provide (all-defined-out))

; Chain Graph = n nodes, (n-1) edges.

(define (chain-graph n)
  (define G (empty-graph))
  (for ([i (in-range n)])
    (add-edge! G i (+ i 1)))
  (rm-node! G n)
  G)

; Cyclic Graph = n nodes, n edges.

(define (cyclic-graph n)
  (define G (chain-graph n))
  (when (> n 1)
    (add-edge! G (- n 1) 0))
  G)

; Complete Tree Graph = (a^(d+1) - 1)/(a - 1) nodes, ((# of Nodes) - 1) edges. (where a is the tree's arity and d is its depth)

(define (complete-tree-graph arity depth)
  (define G (empty-graph))
  (cond [(or (< arity 0) (< depth 0)) (error "Arity and Depth must be greater than 0.")]
        [(= depth 0) (add-node! G 0)] ; Regardless of Arity, it's just a single node.
        [(= arity 1) (set! G (chain-graph depth))] ; If arity is 1, it's a chain graph with "depth" nodes
        [else (let ([n 0]
                    [edges (inexact->exact (- (/ (- (expt arity (+ depth 1)) 1) (- arity 1)) 1))])
                (for ([i (in-range edges)])
                  (add-edge! G n (+ i 1))
                  (when (zero? (modulo (+ i 1) arity))
                    (set! n (+ n 1)))))])
  G)

; Grid Graph = n*m nodes, (n*m)+1 edges.

(define (grid-graph n m)
  (define G (empty-graph))
  (if (= m n 1)
      (add-node! G 0)
      (for ([i (in-range n)])
        (for ([j (in-range m)])
          (when (< j (- m 1))
            (add-edge! G (+ (* i m) j) (+ (* i m) j 1)))
          (when (< i (- n 1))
            (add-edge! G (+ (* i m) j) (+ (* i m) j m))))))
  G)

; Clique Graph = n nodes, n*(n-1)/2 edges.

(define (clique-graph n)
  (define G (empty-graph))
  (if (= n 1)
      (add-node! G 0)
      (for ([i (in-range n)])
        (for ([j (in-range n)])
          (when (not (= i j))
            (add-edge! G i j)))))
  G)