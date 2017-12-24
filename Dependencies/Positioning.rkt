#lang racket

(require "Vector2D.rkt")
(require "Vector3D.rkt")
(provide (all-defined-out))

; An initially empty Positioning function, which is a hash set.

(define (positioning)
  (make-hash))

; Adds vect to positioning with node-id as its key.

(define (apply-positioning positioning node-id)
  (hash-ref positioning node-id))

; Takes a list of (node-id . vect) pairs and create a positioning function from it.

(define (positioning-of-asso-list L)
  (make-hash L))

; Takes the (w)idth and (h)eight of a canvas as well as a (L)ist of nodes
; and returns a positioning function constructed with nodes and a random
; vector2D attributed to each node from the list L.

(define (random-positioning-of-node-list w h L)
  (let [(pos (positioning))]
    (for ([node L])
      (hash-set! pos node (make-vect (+ (random w) (random)) (+ (random h) (random)))))
    pos))

; Translates the node-id's old-vect by a "vect".

(define (positioning-move-node! positioning node-id vect)
  (when (hash-has-key? positioning node-id)
    (let* ([old-vect (apply-positioning positioning node-id)]
           [new-vect (vect-sum old-vect vect)])
      (hash-set! positioning node-id new-vect))))

; Prints a positioning function.

(define (print-positioning id-list positioning)
  (for ([node id-list])
    (when (hash-has-key? positioning node)
      (printf "~a\t~a\n" node (hash-ref positioning node)))))


;; Translates the node-id's old-vect by a "vect" in 3D mode.

(define (positioning-move-node-3D! positioning node-id vect)
  (when (hash-has-key? positioning node-id)
    (let* ([old-vect (apply-positioning positioning node-id)]
           [new-vect (vect-sum3D old-vect vect)])
      (hash-set! positioning node-id new-vect))))