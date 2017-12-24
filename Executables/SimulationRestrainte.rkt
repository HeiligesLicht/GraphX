#lang racket

(require "../Dependencies/Relaxation.rkt")
(require "../Dependencies/Positioning.rkt")
(require "../Dependencies/GraphGenerators.rkt")
(require "../Dependencies/Graph.rkt")
(require "../Dependencies/Vector2D.rkt")

(define CANVAS-W+H 5)

(define Graph (grid-graph 3 3))
(define GraphPositioning (random-positioning-of-node-list CANVAS-W+H CANVAS-W+H (get-nodes Graph)))
(define Relaxator (new-relaxator))
(Relaxator 'set-tolerated-error 36) ; On aura jamais une erreur de 10%. Essayez avec 'set-tolerated-error.

(define N 0)

(do ()
  ((Relaxator 'final-state?))
  (Relaxator 'relax Graph GraphPositioning (mutable-set))
  (set! N (+ N 1)))

(printf "Pour une erreur de ~a%, Relaxator a itéré ~a fois avant d'atteindre un état d'équilibre total.\n" (* (Relaxator 'get-tolerated-error) 100) N)
(print-graph Graph)
(print-positioning (get-nodes Graph) GraphPositioning)

; Pour détecter l'état d'équilbre du système (qui d'ailleurs n'est pas unique pour cette solution),
; on peut compter si les normes des vecteurs forces totales appliquées à chaque vecteur
; sont tous très proches de 0 à epsilon près.
; On ne va pas implémenter cette solution, on gardera celle décrite dans l'énoncé
; de projet.