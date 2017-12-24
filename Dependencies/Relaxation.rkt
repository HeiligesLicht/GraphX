#lang racket

(require "Vector2D.rkt")
(require "Vector3D.rkt")
(require "Positioning.rkt")
(require "Graph.rkt")
(provide (all-defined-out))

(define (new-relaxator)
  (let* ([c1 2]
         [c2 20]
         [c3 400]
         [c4 1]
         [zoom 1]
         [N 0]
         [final-state? #f]
         [err 10])

    ; Mechanical Force Vector between node-id1 and node-id2
    (define (force-m positioning node-id1 node-id2)
      (define vect1 (hash-ref positioning node-id1))
      (define vect2 (hash-ref positioning node-id2))
      (define d^2 (distance^2 vect1 vect2))
      (vect-scalar (* c1 (log (/ d^2 (sqr (- c2 (* c2 (- 1 zoom)))))))
                   (vect-unit (vect-sum (vect-scalar -1 vect1)
                                        vect2))))

    ; Electrical Force Vector between node-id1 and node-id2
    (define (force-e positioning node-id1 node-id2)
      (define vect1 (hash-ref positioning node-id1))
      (define vect2 (hash-ref positioning node-id2))
      (define d^2 (distance^2 vect1 vect2))
      (vect-scalar (/ (- (* zoom c3)) d^2)
                   (vect-unit (vect-sum (vect-scalar -1 vect1)
                                        vect2))))

    ; Mechanical Force 3D Vector between node-id1 and node-id2
    (define (force-m-3D positioning node-id1 node-id2)
      (define vect1 (hash-ref positioning node-id1))
      (define vect2 (hash-ref positioning node-id2))
      (define d^2 (distance3D^2 vect1 vect2))
      (vect-scalar3D (* c1 (log (/ d^2 (* c2 c2))))
                     (vect-unit3D (vect-sum3D (vect-scalar3D -1 vect1)
                                              vect2))))

    ; Mechanical Force 3D Vector between node-id1 and node-id2
    (define (force-e-3D positioning node-id1 node-id2)
      (define vect1 (hash-ref positioning node-id1))
      (define vect2 (hash-ref positioning node-id2))
      (define d^2 (distance3D^2 vect1 vect2))
      (vect-scalar3D (/ (- c3) d^2)
                     (vect-unit3D (vect-sum3D (vect-scalar3D -1 vect1) vect2))))
    
    (define (this method . Largs)
      (case method
        ['final-state? final-state?]
        ['set-c1 (set! c1 (car Largs))]
        ['get-c1 c1]
        ['set-c2 (set! c2 (car Largs))]
        ['get-c2 c2]
        ['set-c3 (set! c3 (car Largs))]
        ['get-c3 c3]
        ['set-c4 (set! c4 (car Largs))]
        ['get-c4 c4]
        ['set-tolerated-error (set! err (/ (car Largs) 100))]
        ['get-tolerated-error err]
        ['set-zoom (set! zoom (car Largs))]
        ['get-zoom zoom]
        ['relax (let* ([graph (car Largs)]
                       [pos (cadr Largs)]
                       [ignore (caddr Largs)]
                       [nodes (get-nodes graph)]
                       [final? #t])
                  
                  (for ([node1 nodes])
                    (when (not (set-member? ignore node1))
                      (let ([Ft (make-vect 0 0)])
                        (for ([node2 nodes])
                          (when (not (equal? node1 node2))
                            (set! Ft (vect-sum Ft (force-e pos node1 node2))))
                          (when (set-member? (hash-ref graph node1) node2)
                            (set! Ft (vect-sum Ft (force-m pos node1 node2)))
                            (set! final? (and final? (<= (/ (distance^2 (apply-positioning pos node1)
                                                                              (apply-positioning pos node2))
                                                            (* c2 c2)) (* err err))))))
                        (positioning-move-node! pos node1 (vect-scalar c4 Ft)))))
                  (set! final-state? final?))]
        
        ['relax-3D (let* ([graph (car Largs)]
                          [pos (cadr Largs)]
                          [ignore (caddr Largs)]
                          [nodes (get-nodes graph)]
                          [final? #t])
                     
                     (for ([node1 nodes])
                       (when (not (set-member? ignore node1))
                         (let ([Ft (make-vect3D 0 0 0)])
                           (for ([node2 nodes])
                             (when (not (equal? node1 node2))
                               (set! Ft (vect-sum3D Ft (force-e-3D pos node1 node2))))
                             (when (set-member? (hash-ref graph node1) node2)
                               (set! Ft (vect-sum3D Ft (force-m-3D pos node1 node2)))
                               (set! final? (and final? (<= (/ (distance3D^2 (apply-positioning pos node1)
                                                                              (apply-positioning pos node2))
                                                            (* c2 c2)) (* err err))))))
                           (positioning-move-node-3D! pos node1 (vect-scalar3D c4 Ft))))))]))
    this))