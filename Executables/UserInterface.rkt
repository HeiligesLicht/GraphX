#lang racket/gui
(require pict3d)

;; Imports

(require "../Dependencies/Graph.rkt")
(require "../Dependencies/Positioning.rkt")
(require "../Dependencies/Vector2D.rkt")
(require "../Dependencies/Vector3D.rkt")
(require "../Dependencies/GraphGenerators.rkt")
(require "../Dependencies/Relaxation.rkt")
(require racket/random)

;; GUI CONSTANTS

(define CANVAS_SIZE 600) ; Size of a Squared Canvas
(define UPPER_HPANEL_HEIGHT 36)
(define LEFT_VPANEL_WIDTH 36)
(define RIGHT_VPANEL_WIDTH 200)

;; Graph, Positioning and Relaxator for Canvas Testing.
;; Mode: "2D" for 2D Canvas, "3D" for 3D Canvas (Pict3D).


(define Mode "2D") ; 2D Mode is default

(define Graph (empty-graph)) ; Initial State of Last Graph

(define GraphPositioning (random-positioning-of-node-list CANVAS_SIZE
                                                          CANVAS_SIZE
                                                          (get-nodes Graph))) ; Default Positioning of the Graph

; Ignore Nodes Set was added as part of the Relaxator to make things easier
; when adding additionnal features.
(define IgnoreNodes (mutable-set))

; Black Holes!
(define BlackHolePositioning (make-hash))

; Soft object instance: Relaxator.
(define Relaxator (new-relaxator))

; Tolerated distance error between two neighboring nodes.
(Relaxator 'set-tolerated-err 50)

; List of Previous Positionings
(define PreviousPositionings '())

;; Drawing Function and Relaxator for 2D Canvas.

(define (Draw-Canvas-2D dc)
  ; A sad attempt on making a O(n²) algorithm run a little faster.
  (let ([parallel-relax (future (lambda () (Relaxator 'relax-3D Graph GraphPositioning IgnoreNodes)))])
    (Draw-Graph Graph GraphPositioning IgnoreNodes dc BITMAP-DC)
    (when (not Timer-Stopped?)
      (when (not (zero? (hash-count BlackHolePositioning)))
        (Calculate-Gravitation))
      (touch parallel-relax)
      (when (and (not (Relaxator 'final-state?)) (< (length PreviousPositionings) 1000))
        (set! PreviousPositionings (cons (cons (hash-copy Graph) (hash-copy GraphPositioning)) PreviousPositionings))))))

;; Drawing Function and Relaxator for 3D Canvas. 

(define (Draw-Canvas-3D)
  (let ([parallel-relax (future (lambda () (Relaxator 'relax-3D Graph GraphPositioning IgnoreNodes)))])
    (Draw-Graph-3D Graph GraphPositioning)
    (when (not Timer-Stopped?)
      (touch parallel-relax))))


;; Black Holes Function

(define (Gravitation node-vect hole-position)
  (define CONS-GRAVIT 6.67e-11)
  (define vect (make-vect (- (coord-x hole-position) (coord-x node-vect)) (- (coord-y hole-position) (coord-y node-vect))))
  (define hole-mass 1e9)
  (define node-mass 2e5)
  (define force (* CONS-GRAVIT (/ (* node-mass hole-mass)  (distance^2 node-vect hole-position))))
  (vect-scalar force (vect-unit vect)))

(define (Calculate-Gravitation)
  (when (not (hash-empty? BlackHolePositioning))
    (for ([(hole position)(in-hash BlackHolePositioning)])
      (define GraphPos (hash-copy GraphPositioning))
      (for ([(node vect)(in-hash GraphPos)])
        (define grav (Gravitation vect position))
        (hash-set! GraphPositioning node (vect-sum vect grav))
        (when (< (distance^2 position vect) 400)
          (rm-node! Graph node)
          (hash-remove! GraphPositioning node))))))

;; Mode Transition Functions

;; 2D to 3D
(define (2D->3D)
  (set! Mode "3D") ; Mode is 3D now!
  ; Adds random Z component to existing 2D Vectors as a Vector3D for each node's positioning.
  (hash-for-each GraphPositioning
                 (lambda (node vect) (hash-set! GraphPositioning node (make-vect3D (coord-x vect)
                                                                                   (coord-y vect)
                                                                                   (+ (random 600) (random))))))
  ; Removes the old Canvas
  (send MIDDLE_VPANEL delete-child
        (list-ref (send MIDDLE_VPANEL get-children) 1))

  ; Adds a pict3d Canvas
  (set! CANVAS (new Eventful-3D-Canvas%
                    [parent MIDDLE_VPANEL]
                    [min-height CANVAS_SIZE]
                    [min-width CANVAS_SIZE]
                    [stretchable-width #f]
                    [stretchable-height #f]
                    [pict3d (Draw-Graph-3D Graph GraphPositioning)]))

  ; New TIMER calls set-pict3d method instead of paint-callback method now.
  (set! TIMER (new timer%
                   [notify-callback (lambda () 
                                      (let ([parallel-relax (future (lambda () (Relaxator 'relax-3D Graph GraphPositioning IgnoreNodes)))])
                                        (send CANVAS set-pict3d (Draw-Graph-3D Graph GraphPositioning))
                                        (when (not Timer-Stopped?)
                                          (touch parallel-relax))))])))

;; 3D to 2D

(define (3D->2D)
  (set! Mode "2D") ; Mode is 2D now!

  ; Removes Z component to existing 3D Vectors as a Vector2D for each node's positioning.
  (hash-for-each GraphPositioning
                 (lambda (node vect) (hash-set! GraphPositioning node (make-vect (coord-x3D vect)
                                                                                 (coord-y3D vect)))))
  ; Removes pict3d Canvas
  (send MIDDLE_VPANEL delete-child
        (list-ref (send MIDDLE_VPANEL get-children) 1))

  ; Adds a normal Canvas
  (set! CANVAS (new Eventful-2D-Canvas%
                    [parent MIDDLE_VPANEL]
                    [min-height CANVAS_SIZE]
                    [min-width CANVAS_SIZE]
                    [style '(border)]
                    [stretchable-width #f]
                    [stretchable-height #f]
                    [paint-callback (lambda (obj dc) (Draw-Canvas-2D dc))]))
  
  ; TIMER now calls on-paint method of CANVAS.
  (set! TIMER (new timer%
                   [notify-callback (lambda () (send CANVAS on-paint))])))


; Color Vizualizer

; Gets the current selected color from the sliders
; Function returns 4 results.
(define (Get-Slider-Color)
  (define R (send RED_SLIDER get-value))
  (define G (send GREEN_SLIDER get-value))
  (define B (send BLUE_SLIDER get-value))
  (define A (/ (send ALPHA_SLIDER get-value) 1000))
  (values R G B A))

; Updates the Color Vizualiser
(define (Update-Color-Vizualizer)
  (define r 0)
  (define g 0)
  (define b 0)
  (define a 0)
  (set!-values (r g b a) (Get-Slider-Color))
  (send (send COLOR_VIZUALIZER get-dc) set-background (make-object color% r g b a))
  (send (send COLOR_VIZUALIZER get-dc) clear))

; Center Graph
; Calculates the Barycenter of all nodes
; and translates all vectors in a positioning
; by a vector MM'.
; (Where M is the barycenter, and M' the center of the 2D Canvas)
(define (Center-Graph)
  (when (not (hash-empty? Graph))
    (define CenterOfAllNodes (apply vect-sum* (hash-map GraphPositioning (λ (node vect) vect))))
    (set! CenterOfAllNodes (vect-scalar (/ (hash-count GraphPositioning)) CenterOfAllNodes))
    (for ([(node vect) (in-hash GraphPositioning)])
      (hash-set! GraphPositioning
                 node
                 (vect-sum* vect (vect-scalar -1 CenterOfAllNodes) (make-vect (/ CANVAS_SIZE 2) (/ CANVAS_SIZE 2)))))))

; Reset Graph
; Resets everything except zoom.
; Graph, Positioning, etc...
(define (Reset-Graph)
  (hash-clear! Graph)
  (hash-clear! GraphPositioning)
  (set-clear! IgnoreNodes)
  (set! BlackHolePositioning (make-hash))
  (send REMOVE-BLACKHOLE_BUTTON enable #f)
  (set! PreviousPositionings '()))

;; Zoom-In Function
;; Zoom-In Limit is 40%
;; We added a zoom parameter in Relaxator to keep track of the current zoom.
;; For each Vector2D in a Positioning, we multiply by 0.9 and add an small offset
;; which is equal to 10% of the X and Y coordinated of the CANVAS center.
(define (ZOOM-)
  (when (> (Relaxator 'get-zoom) 0.4)
    (define tX (* CANVAS_SIZE 0.05))
    (define tY (* CANVAS_SIZE 0.05))
    (for ([(node vect) (in-hash GraphPositioning)])
      (define x (coord-x vect))
      (define y (coord-y vect))
      (hash-set! GraphPositioning node (make-vect (+ (* 0.9 x) tX) (+ (*  y 0.9) tY))))
    (Relaxator 'set-zoom (- (Relaxator 'get-zoom) 0.1))
    (set! NODES_SIZE (- NODES_SIZE (* NODES_SIZE 0.1)))
    (set! EDGES_THICKNESS (- EDGES_THICKNESS (* EDGES_THICKNESS 0.1)))
    (when Timer-Stopped?
      (send CANVAS on-paint))))


;; Zoom-Out Function
;; Zoom-Out Limit is 150%
;; For each Vector2D in a Positioning, we multiply by 1.1 and add an small offset
;; which is equal to 10% of the X and Y coordinated of the CANVAS center.
(define (ZOOM+)
  (when (< (Relaxator 'get-zoom) 1.5)
    (define tX (* CANVAS_SIZE -0.05))
    (define tY (* CANVAS_SIZE -0.05))
    (for ([(node vect) (in-hash GraphPositioning)])
      (define x (coord-x vect))
      (define y (coord-y vect))
      (hash-set! GraphPositioning node (make-vect (+ (* 1.1 x) tX) (+ (* 1.1 y) tY))))
    (Relaxator 'set-zoom (+ (Relaxator 'get-zoom) 0.1))
    (set! NODES_SIZE (+ NODES_SIZE (* NODES_SIZE 0.1)))
    (set! EDGES_THICKNESS (+ EDGES_THICKNESS (* EDGES_THICKNESS 0.1)))
    (when Timer-Stopped?
      (send CANVAS on-paint))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; EVENTFUL CANVAS 2D MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Eventful-Canvas%:
;; A subclass of canvas% used to handle mouse and keyboard events depending on Toolbox Selection.

(define Eventful-2D-Canvas%
  (class canvas%
    (define Node #f)  ; Current clicked Node is false
    (define AuxNode #f) ; Current last clicked Node (AuxNode) is false

    ; Check if a x,y position is inside a node.
    ; Returns node's id if one is found else returns false.
    (define (In-Node x y)
      (let/ec return
        (for ([(node pos) (in-hash GraphPositioning)])
          (when (<= (distance^2 pos (make-vect x y)) (/ (* NODES_SIZE NODES_SIZE) 4))
            (return node)))
        (return #f)))

    ; Gets a non-existing node id ready to be added into the graph.
    (define (Get-Valid-Node-Id)
      (define node-id #f)
      (define nodes-List (hash-keys Graph))
      (if (and (not (empty? nodes-List))
               (andmap number? nodes-List))
          (set! node-id (+ (apply max (filter number? nodes-List)) 1))
          (set! node-id 0))
      node-id)

    ; Mouse events management.
    (define/override (on-event evt)
      (case (send evt get-event-type)
        ;; If we're left-clicking on the canvas.
        [(left-down) (case Toolbox_Selection
                       ; If Handtool is selected.
                       [(0) (set! Node (make-vect (send evt get-x) (send evt get-y)))] ; Node now equals the current clicked point on canvas

                       ; If Selection Tool is selected
                       [(1) (set! Node (In-Node (send evt get-x) (send evt get-y))) ; Node gets value from In-Node function.
                            (when Node ; If Node is not false
                              (set-add! IgnoreNodes Node))]
                       ; Node is added to IgnoreNodes set, this will stop all force calculations applied to this node.

                       ; If Freeze Node is selected

                       [(2) (set! Node (In-Node (send evt get-x) (send evt get-y))) ; Node gets value from In-Node function.
                            (when Node ; If Node is not false
                              (set-add! IgnoreNodes Node))]

                       ; If Add Node Tool is selected
                       [(3) (set! Node (Get-Valid-Node-Id)) ; Node gets a valid node id.
                            (add-node! Graph Node) ; Node is added to the Graph
                            (hash-set! GraphPositioning Node (make-vect (+ (send evt get-x) (random))
                                                                        (+ (send evt get-y) (random))))]
                       ; Node is added to the GraphPositioning with the current clicked point on canvas as its coordinates,
                       ; plus an offset close to 0 to avoid a division by zero if 2 added nodes are overlapping.

                       ; If Add Edge Tool is selected
                       [(4) (set! Node (In-Node (send evt get-x) (send evt get-y))) ; Node gets value from In-Node function.
                            (when Node ; If Node is not false
                              (if AuxNode ; If AuxNode is not false
                                  (begin
                                    (when (hash-has-key? Graph AuxNode) ; If AuxNode is in the Graph
                                      (add-edge! Graph AuxNode Node)) ; Adds an Edge between Node and AuxNode
                                    (set! AuxNode #f)) ; After the Edge is added, AuxNode is set to false.
                                  (set! AuxNode Node))) ; If AuxNode is false, AuxNode is set to Node.
                            
                            (when (not Node) ; If Node is false. (We're not clicking on a Node)
                              (set! Node (Get-Valid-Node-Id)) ; Node gets a valid node id.
                              (add-node! Graph Node) ; Node is added to Graph.
                              (hash-set! GraphPositioning Node (make-vect (+ (send evt get-x) (random))
                                                                          (+ (send evt get-y) (random))))
                              ; Node is added to the GraphPositioning with the current clicked point on canvas as its coordinates,
                              ; plus an offset close to 0 to avoid a division by zero if 2 added nodes are overlapping.

                              (if AuxNode ; If AuxNode is not false.
                                  (begin
                                    (when (hash-has-key? Graph AuxNode) ; If AuxNode is in the Graph
                                      (add-edge! Graph AuxNode Node)) ; Adds an Edge between Node and AuxNode
                                    (set! AuxNode #f)) ; AuxNode is set to false.
                                  (set! AuxNode Node)))] ; If AuxNode is false, AuxNode is set to Node.

                       ; If Remove Node Tool is selected.
                       [(5) (set! Node (In-Node (send evt get-x) (send evt get-y))) ; Node gets value from In-Node function.
                            (when Node ; If Node is not false
                              (rm-node! Graph Node) ; Removes Node from Graph
                              )] ; We'll keep the Node but we'll ignore it.
                       
                       [(6) (set! Node (In-Node (send evt get-x) (send evt get-y))) ; Node gets value from In-Node function.
                            (when Node ; If Node is not false
                              (if AuxNode ; If AuxNode is not false
                                  (begin
                                    (when (hash-has-key? Graph AuxNode) ; If AuxNode is in the Graph.
                                      (rm-edge! Graph Node AuxNode)) ; Removes Edge between AuxNode and Node.
                                    (set! AuxNode #f)) ; Sets AuxNode to false.
                                  (set! AuxNode Node)))])] ; Sets AuxNode to Node.

        ; If we're no longer left-clicking on the canvas. 
        [(left-up) (case Toolbox_Selection
                     [(1) (set! Node (In-Node (send evt get-x) (send evt get-y))) ; Node gets value from In-Node function.
                          (when Node ; If Node is not false
                            (set-remove! IgnoreNodes Node) ; Removes Node from IgnoreNodes, does nothing if Node is not in it.
                            (set! Node #f))] ; Sets Node to false.
                     [else (set! Node #f)])] ; Sets Node to false.

        ; Released a Frozen node with right click.
        [(right-down) (case Toolbox_Selection
                        [(2) (set! Node (In-Node (send evt get-x) (send evt get-y))) ; Node gets value from In-Node function.
                             (when Node ; If Node is not false
                               (set-remove! IgnoreNodes Node)
                               (set! Node #f))])]
        
        ; If the mouse is moving on the canvas.
        [else (when Node ; If Node is not false then another mouse event is taking place!
                (case Toolbox_Selection
                  ; If the other event is the HandTool
                  [(0) (define tV (vect-sum (vect-scalar -1 Node) (make-vect (send evt get-x) (send evt get-y))))
                       ; tV is an infinitesimal translation 2D vector.
                       (for ([(node vect) (in-hash GraphPositioning)])
                         (positioning-move-node! GraphPositioning node tV)) ; Moves each Node in a GraphPositioning by tV
                       (set! Node (make-vect (send evt get-x) (send evt get-y)))] ; Node now equals to current clicked point on canvas.
                  ; If the other event is the Selection Tool
                  [(1) (when (hash-has-key? GraphPositioning Node) ; 
                         (hash-set! GraphPositioning Node (make-vect (send evt get-x) (send evt get-y))))] ; Overrides Node's positioning with current clicked point on canvas.
                  
                  ; If the other event is Freeze Node.
                  [(2) (set! Node #f)] ; If cursor is moving, don't remember Node's ID. (Avoids overlapping events between this and Selection Tool
                  ;; Nothing to do when adding/removing nodes/edges here.
                  ;; Except, probably, to display a preview of the node(s) to add/remove.
                  ))])
      (when Timer-Stopped?
        (send this on-paint)))
    (super-new)))


; Canvas related functions and variables

; Bitmap DC for flickering (Used as a temporary buffer to speed up rendering)

(define BITMAP (make-object bitmap% CANVAS_SIZE CANVAS_SIZE)) ; An empty bitmap
(define BITMAP-DC (new bitmap-dc% (bitmap BITMAP))) ; Temporary Buffer

; 2D Canvas Drawing Function

(define NODES_SIZE 20) ; Diameter of Nodes
(define EDGES_THICKNESS 2) ; Edges Thickness
(define HOLES_SIZE 30) ; Diameter of a BlackHole


(define NODES_COLOR (make-object color% "red")) ; Nodes Default Color
(define EDGES_COLOR (make-object color% "black")) ; Edges Default Color
(define HOLES_COLOR (make-object color% "black")) ; Black Holes Color

(define PEN-FOR-BLACK-HOLES (make-object pen% HOLES_COLOR HOLES_SIZE 'solid))

; Graph Drawing function 
(define (Draw-Graph graph positioning ignore dc b-dc)

  ; BlackHoles Drawing function
  (define (draw-blackholes)
    (send b-dc set-pen PEN-FOR-BLACK-HOLES)
    (for ([(hole position) (in-hash BlackHolePositioning)])
      (when (hash-has-key? BlackHolePositioning hole)
        (send b-dc draw-point (coord-x position) (coord-y position)))))

  ; Nodes Drawing function
  (define (draw-nodes positioning)
    (define PEN-FOR-NODES (make-object pen% NODES_COLOR NODES_SIZE 'solid))
    (send b-dc set-pen PEN-FOR-NODES)
    (for ([(node vect) (in-hash positioning)])
      (when (hash-has-key? graph node)
        (send b-dc draw-point (coord-x vect) (coord-y vect)))))

  ; drawn-edges: prevents the drawing of an edge twice.
  (define drawn-edges (mutable-set)) 

  ; Edges Drawing function
  (define (draw-edges graph)
    (define PEN-FOR-EDGES (make-object pen% EDGES_COLOR EDGES_THICKNESS 'solid))
    (send b-dc set-pen PEN-FOR-EDGES)
    (for ([(node1 neighbors) (in-hash graph)])
      (for ([(node2) (in-set neighbors)])
        (when (and (not (set-member? drawn-edges (list node2 node1)))
                   (hash-has-key? positioning node1)
                   (hash-has-key? positioning node2))
          (let ([pos1 (apply-positioning positioning node1)]
                [pos2 (apply-positioning positioning node2)])
            (send b-dc draw-line (coord-x pos1) (coord-y pos1)
                  (coord-x pos2) (coord-y pos2))))
        (set-add! drawn-edges (list node1 node2)))))
  
  (send b-dc clear) ; Clears bitmap-dc
  (send b-dc set-smoothing 'smoothed) ; Enable anti-aliasing

  ; Parallelly draws nodes, edges and blackholes.
  (when (or (not (hash-empty? positioning))
            (not (hash-empty? BlackHolePositioning)))
    (let ([parallel-draw-edges (future (lambda () (draw-edges graph)))]
          [parallel-draw-nodes (future (lambda () (draw-nodes positioning)))]
          [parallel-draw-blackholes (future (lambda () (draw-blackholes)))])
      (touch parallel-draw-edges)
      (touch parallel-draw-nodes)
      (touch parallel-draw-blackholes)))
  (send dc draw-bitmap BITMAP 0 0 'solid))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; EVENTFUL CANVAS 3D MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define Camera-Looking-At-Point (pos 600 600 600)) ; Default point in space the camera is pointing at.
(define Initial-Camera-Affine (point-at (pos 0 0 0) Camera-Looking-At-Point))
; Affine is a structure describing a position and 3 axis of a 3D object.
; By default, the Camera's z-axis is forward, x-axis is right, y-axis is down.
; Default Camera Affine, Camera is at origin, pointing towards Camera-Looking-At-Point

(define Eventful-3D-Canvas% ; Sub-class of pict3d-canvas%
  (class pict3d-canvas%
    
    (define rX 0)
    (define rY 0)
    (define V #f) ; FPS Mode is Off By Default.
    
    (define/override (on-event evt)
      (case (send evt get-event-type)
        ; FPS MODE IS ON!
        [(left-down) (set! rX (send evt get-x)) ; rX is current clicked point x coordinate.
                     (set! rY (send evt get-y)) ; rY is current clicked point y coordinate.
                     (set! V (make-vect rX rY)) ; V is now a Vector2D with rX, rY as its x and y coordinates
                     (send this set-cursor (make-object cursor% 'blank))] ; Cursor is annoying.
        
        [(left-up) (set! rX 0) ; rX is now set to 0
                   (set! rY 0) ; rX is now set to 0
                   (set! V #f) ; V is now back to false
                   (send this set-cursor (make-object cursor% 'arrow))] ; Cursor is appearing again.
        
        [else (when V ; If then is not false
                (set! V (vect-sum (vect-scalar -1 V) (make-vect (send evt get-x) (send evt get-y))))
                ; V is now an infinitesimal translation vector.
                (define Next-Camera-Affine ; Calculating Next Camera Affine
                  (affine-compose Initial-Camera-Affine ; Affine compose the Initial-Camera-Affine with
                                  (rotate-y (/ (coord-x V) 2))
                                  ; A small rotation applied to the camera's y-axis by an angle of x-coord of tV divided by a constant
                                  ; We can use this constant as the mouse sensability.
                                  (rotate-x (/ (- (coord-y V)) 2))
                                  ; A small rotation applied to the camera's x-axis by an angle of y-coord of tV divided by a constant
                                  ; We can use this constant as the mouse sensability.
                                  ))
                (set! Initial-Camera-Affine (point-at (affine-origin Initial-Camera-Affine) (affine-z-axis Next-Camera-Affine)))
                ; the Initial Camera Affine is now pointiong at the Z-axis (forward) of the previously calculated Next-Camera-Affine
                (set! rY (send evt get-y)) ; Updating rX x-coordinate
                (set! rX (send evt get-x)) ; Updating rY y-coordinate
                (set! V (make-vect rX rY)))]) ; Updating V to updated rX and rY.
      (when Timer-Stopped?
        (send this set-pict3d (Draw-Graph-3D Graph GraphPositioning)))) 


    ; A set of Affine transformations.
    (define MultipleInputs (mutable-set))
    
    ; INPUT KEYS
    
    (define/override (on-char car)
      (define Event (send car get-key-code)) ; Key Push Event

      ; Z, z, ↑ (Up Arrow) are pushed.
      (when (and (or (equal? Event #\z)
                     (equal? Event #\Z)
                     (equal? Event 'up))
                 (not (set-member? MultipleInputs Event)))
        (set-add! MultipleInputs (move-z 5)))

      ; S, s, ↓ (Arrow Down) are pushed.
      (when (and (or (equal? Event #\s)
                     (equal? Event #\S)
                     (equal? Event 'down))
                 (not (set-member? MultipleInputs Event)))
        (set-add! MultipleInputs (move-z -5)))

      ; Q, q are pushed.
      (when (and (or (equal? Event #\q)
                     (equal? Event #\Q))
                 (not (set-member? MultipleInputs Event)))
        (set-add! MultipleInputs (move-x -5)))

      ; D, d are pushed.
      (when (and (or (equal? Event #\d)
                     (equal? Event #\D))
                 (not (set-member? MultipleInputs Event)))
        (set-add! MultipleInputs (move-x 5)))

      ; Space is pushed.
      (when (and (equal? Event #\Space)
                 (not (set-member? MultipleInputs Event)))
        (set-add! MultipleInputs (move-y -5)))

      ; Left Ctrl, Right Ctrl are pushed.
      (when (and (or (equal? Event 'control)
                     (equal? Event 'rcontrol))
                 (not (set-member? MultipleInputs Event)))
        (set-add! MultipleInputs (move-y 5)))

      ; ← (Left Arrow) is pushed.
      (when (and (equal? Event 'left)
                 (not (set-member? MultipleInputs Event)))
        (set-add! MultipleInputs (rotate-y -5)))

      ; → (Right Arrow) is pushed.
      (when (and (equal? Event 'right)
                 (not (set-member? MultipleInputs Event)))
        (set-add! MultipleInputs (rotate-y 5)))


      ; RELEASE KEYS

      (define REvent (send car get-key-release-code)) ; Key Release Event

      ; Z, z, ↑ (Up Arrow) are released.
      (when (or (equal? REvent #\z)
                (equal? REvent #\Z)
                (equal? REvent 'up))
        (set-remove! MultipleInputs (move-z 5)))

      ; S, s, ↓ (Arrow Down) are pushed.
      (when (or (equal? REvent #\s)
                (equal? REvent #\S)
                (equal? REvent 'down))
        (set-remove! MultipleInputs (move-z -5)))

      ; Q, q are released.
      (when (or (equal? REvent #\q)
                (equal? REvent #\Q))
        (set-remove! MultipleInputs (move-x -5)))

      ; D, d are released.
      (when (or (equal? REvent #\d)
                (equal? REvent #\D))
        (set-remove! MultipleInputs (move-x 5)))

      ; Space is released.
      (when (equal? REvent #\Space)
        (set-remove! MultipleInputs (move-y -5)))

      ; Left Ctrl and Right Ctrl are released.
      (when (or (equal? REvent 'control)
                (equal? REvent 'rcontrol))
        (set-remove! MultipleInputs (move-y 5)))
      
      ; ← (Left Arrow) is pushed.
      (when (equal? REvent 'left)
        (set-remove! MultipleInputs (rotate-y -5)))
      
      ; → (Right Arrow) is pushed.
      (when (equal? REvent 'right)
        (set-remove! MultipleInputs (rotate-y 5)))

      ; Moves the Camera.
      (set! Initial-Camera-Affine (apply affine-compose (cons Initial-Camera-Affine (set->list MultipleInputs))))

      (when Timer-Stopped?
        (send this set-pict3d (Draw-Graph-3D Graph GraphPositioning))))
    (super-new)))

;; 3D Canvas Drawing Function.

(define xc CANVAS_SIZE)
(define yc CANVAS_SIZE)
(define zc (/ CANVAS_SIZE 2))

(define NODES_COLOR_3D (list 1 0 0 1))
(define EDGES_COLOR_3D (list 0 0 1 1))

(define (Draw-Graph-3D graph positioning)
  
  (define (draw-edge vect1 vect2)
    (define x1 (coord-x3D vect1))
    (define y1 (coord-y3D vect1))
    (define z1 (coord-z3D vect1))
    (define x2 (coord-x3D vect2))
    (define y2 (coord-y3D vect2))
    (define z2 (coord-z3D vect2))
    (define v-norm (sqrt (distance3D^2 vect1 vect2)))
    (define init-pict (point-at (pos 0 0 v-norm) -z))
    (define new-pict (point-at (pos x1 y1 z1) (pos x2 y2 z2)))
    (define droite (set-emitted
                    (parameterize ([current-color (apply rgba EDGES_COLOR_3D)])
                      (cylinder origin (pos 2 2 v-norm)))
                    (emitted EDGES_COLOR_3D)))
    (relocate droite init-pict new-pict))

  (define (draw-node vect r)
    (define x (coord-x3D vect))
    (define y (coord-y3D vect))
    (define z (coord-z3D vect))
    (set-emitted
     (parameterize ([current-color (apply rgba NODES_COLOR_3D)])
       (with-material
           (material #:specular 1.0)
         (sphere (pos x y z) r)))
     (emitted NODES_COLOR_3D)))

  (define MainCamera
    (basis 'camera Initial-Camera-Affine))

  (define drawn-edges (mutable-set))
  (define edges-List '())
  (define nodes-List '())
  
  (for ([(node1 neighbors) (in-hash graph)])
    (for ([node2 (in-set neighbors)])
      (when (not (set-member? drawn-edges (list node2 node1)))
        (set! edges-List (cons (draw-edge (apply-positioning positioning node1) (apply-positioning positioning node2)) edges-List))
        (set-add! drawn-edges (list node1 node2)))))

  (for ([(node) (get-nodes graph)])
    (set! nodes-List (cons (draw-node (apply-positioning positioning node) 10) nodes-List)))

  (combine nodes-List edges-List MainCamera (light (pos 600 600 600))))

;; Toolbox Selection Options:
;; 0 : Hand Tool
;; 1 : Selection Tool
;; 2 : Freeze Node
;; 3 : Add Node
;; 4 : Add Edge
;; 5 : Remove Node
;; 6 : Remove Edge

(define Toolbox_Selection 0)

(define (Enable-Current-Selection Current-Button)
  (case Toolbox_Selection
    [(0) (send HANDTOOL_BUTTON enable #t)]
    [(1) (send SELECTION_BUTTON enable #t)]
    [(2) (send FREEZE-POSITION_BUTTON enable #t)]
    [(3) (send ADD-NODE_BUTTON enable #t)]
    [(4) (send ADD-EDGE_BUTTON enable #t)]
    [(5) (send REMOVE-NODE_BUTTON enable #t)]
    [(6) (send REMOVE-EDGE_BUTTON enable #t)])
  (send Current-Button enable #f))



; TIMER: Clock for CANVAS refresh.

(define TIMER (new timer%
                   [notify-callback (lambda () (send CANVAS on-paint))]))

(define Timer-Stopped? #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Main UI Frame



(define MAIN_UI_WIDTH (+ CANVAS_SIZE LEFT_VPANEL_WIDTH RIGHT_VPANEL_WIDTH))
(define MAIN_UI_HEIGHT (+ CANVAS_SIZE UPPER_HPANEL_HEIGHT))

(define MAIN_UI (new frame%
                     [label "GraphX"]
                     [height MAIN_UI_HEIGHT]
                     [width MAIN_UI_WIDTH]
                     [stretchable-width #f]
                     [stretchable-height #f]))

;; Horizontal Panel Grouping: (Filename_Space + Toolbox), (Controls + Canvas) and Color_Selector.

(define MAIN_HPANEL (new horizontal-panel%
                         [parent MAIN_UI]
                         [min-height MAIN_UI_HEIGHT]
                         [min-width MAIN_UI_WIDTH]
                         [stretchable-width #f]
                         [stretchable-height #f]))

;; Left Vertical Panel Grouping: Filename_Space and Toolbox.
  
(define LEFT_VPANEL (new vertical-panel%
                         [parent MAIN_HPANEL]
                         [min-height MAIN_UI_HEIGHT]
                         [min-width LEFT_VPANEL_WIDTH]
                         [stretchable-width #f]
                         [stretchable-height #f]))

;; Filename_Space

;; We dropped the additional feature that imports and exports the drawn graph into a .DOT file.
;; This is a left over from a prototype GUI.
(define FILENAME_SPACE (new message%
                            [parent LEFT_VPANEL]
                            [label ""]
                            [min-height UPPER_HPANEL_HEIGHT]
                            [min-width LEFT_VPANEL_WIDTH]
                            [stretchable-width #f]
                            [stretchable-height #f]))

;; Toolbox

(define TOOLBOX (new vertical-panel%
                     [parent LEFT_VPANEL]
                     [min-height CANVAS_SIZE]
                     [min-width LEFT_VPANEL_WIDTH]
                     [stretchable-width #f]
                     [stretchable-height #f]))

;; Toolbox Buttons

(define HANDTOOL_BM (read-bitmap "Images/handtool.png" 'png/alpha #f #f))
(define SELECTION_BM (read-bitmap "Images/selection-tool.png" 'png/alpha #f #f))
(define ADD-NODE_BM (read-bitmap "Images/add-node.png" 'png/alpha #f #f))
(define ADD-EDGE_BM (read-bitmap "Images/add-edge.png" 'png/alpha #f #f))
(define REMOVE-NODE_BM (read-bitmap "Images/remove-node.png" 'png/alpha #f #f))
(define REMOVE-EDGE_BM (read-bitmap "Images/remove-edge.png" 'png/alpha #f #f))
(define ADD-BLACKHOLE_BM (read-bitmap "Images/add-blackhole.png" 'png/alpha #f #f))
(define REMOVE-BLACKHOLE_BM (read-bitmap "Images/remove-blackhole.png" 'png/alpha #f #f))
(define FREEZ-POSITION_BM (read-bitmap "Images/freeze.png" 'png/alpha #f #f))

(define HANDTOOL_BUTTON (new button%
                             [parent TOOLBOX]
                             [label HANDTOOL_BM]
                             [enabled #f]
                             [callback (λ (obj evt)
                                         (Enable-Current-Selection obj)
                                         (set! Toolbox_Selection 0))]))

(define SELECTION_BUTTON (new button%
                              [parent TOOLBOX]
                              [label SELECTION_BM]
                              [callback (λ (obj evt)
                                          (Enable-Current-Selection obj)
                                          (set! Toolbox_Selection 1))]))

(define FREEZE-POSITION_BUTTON (new button%
                                    [parent TOOLBOX]
                                    [label FREEZ-POSITION_BM]
                                    [callback (λ (obj evt)
                                                (Enable-Current-Selection obj)
                                                (set! Toolbox_Selection 2))]))

(define ADD-NODE_BUTTON (new button%
                             [parent TOOLBOX]
                             [label ADD-NODE_BM]
                             [callback (λ (obj evt)
                                         (Enable-Current-Selection obj)
                                         (set! Toolbox_Selection 3))]))

(define ADD-EDGE_BUTTON (new button%
                             [parent TOOLBOX]
                             [label ADD-EDGE_BM]
                             [callback (λ (obj evt)
                                         (Enable-Current-Selection obj)
                                         (set! Toolbox_Selection 4))]))

(define REMOVE-NODE_BUTTON (new button%
                                [parent TOOLBOX]
                                [label REMOVE-NODE_BM]
                                [callback (λ (obj evt)
                                            (Enable-Current-Selection obj)
                                            (set! Toolbox_Selection 5))]))

(define REMOVE-EDGE_BUTTON (new button%
                                [parent TOOLBOX]
                                [label REMOVE-EDGE_BM]
                                [callback (λ (obj evt)
                                            (Enable-Current-Selection obj)
                                            (set! Toolbox_Selection 6))]))

(define ADD-BLACKHOLE_BUTTON (new button%
                                  [parent TOOLBOX]
                                  [label ADD-BLACKHOLE_BM]
                                  [callback (λ (obj evt)
                                              (if (= (hash-count BlackHolePositioning) 0)
                                                  (begin
                                                    (hash-set! BlackHolePositioning 0 (make-vect (random CANVAS_SIZE) (random CANVAS_SIZE)))
                                                    (send REMOVE-BLACKHOLE_BUTTON enable #t))
                                                  (hash-set! BlackHolePositioning
                                                             (+ (apply max (hash-keys BlackHolePositioning)) 1)
                                                             (make-vect (random CANVAS_SIZE) (random CANVAS_SIZE))))
                                              (send CANVAS on-paint))]))

(define REMOVE-BLACKHOLE_BUTTON (new button%
                                     [parent TOOLBOX]
                                     [label REMOVE-BLACKHOLE_BM]
                                     [enabled #f]
                                     [callback (λ (obj evt)
                                                 (define rand (random-ref (hash-keys BlackHolePositioning)))
                                                 (hash-remove! BlackHolePositioning rand)
                                                 (when (= (hash-count BlackHolePositioning) 0)
                                                   (send obj enable #f))
                                                 (send CANVAS on-paint))]))

;; Middle Vertical Panel Grouping: Controls and Canvas.

(define MIDDLE_VPANEL (new vertical-panel%
                           [parent MAIN_HPANEL]
                           [min-height MAIN_UI_HEIGHT]
                           [min-width CANVAS_SIZE]
                           [stretchable-width #f]
                           [stretchable-height #f]))

;; Controls

(define CONTROLS (new horizontal-panel%
                      [parent MIDDLE_VPANEL]
                      [min-height UPPER_HPANEL_HEIGHT]
                      [min-width CANVAS_SIZE]
                      [alignment '(center center)]
                      [stretchable-width #f]
                      [stretchable-height #f]))

;; Controls Buttons

(define FAST_BACKWARD_BM (read-bitmap "Images/fast-backward.png" 'png/alpha #f #f))
(define PAUSE_PLAY_BM (read-bitmap "Images/pause-play-button.png" 'png/alpha #f #f))
(define RESET_BM (read-bitmap "Images/reset.png" 'png/alpha #f #f))
(define ZOOM_IN_BM (read-bitmap "Images/zoom-in.png" 'png/alpha #f #f))
(define ZOOM_OUT_BM (read-bitmap "Images/zoom-out.png" 'png/alpha #f #f))
(define CENTER_GRAPH_BM (read-bitmap "Images/center-graph.png" 'png/alpha #f #f))
(define 3D_BM (read-bitmap "Images/3D.png" 'png/alpha #f #f))
(define 2D_BM (read-bitmap "Images/2D.png" 'png/alpha #f #f))
(define SETTINGS_BM (read-bitmap "Images/settings.png" 'png/alpha #f #f))


(define RESET_BUTTON (new button%
                          [parent CONTROLS]
                          [label RESET_BM]
                          [callback (lambda (obj evt)
                                      (when (equal? Mode "3D")
                                        (3D->2D)
                                        (send SWITCH_MODE_BUTTON set-label "3D")
                                        (for ([button (send TOOLBOX get-children)])
                                          (send button enable #t))
                                        (send CENTER_GRAPH_BUTTON enable #t))
                                      (Reset-Graph)
                                      (when Timer-Stopped?
                                        (send CANVAS on-paint)))]))

(define FAST_BACKWARD_BUTTON (new button%
                                  [parent CONTROLS]
                                  [label FAST_BACKWARD_BM]
                                  [callback (λ (obj evt)
                                              (for ([Group PreviousPositionings])
                                                (set! Graph (car Group))
                                                (set! GraphPositioning (cdr Group))
                                                (send CANVAS on-paint)
                                                (sleep 0.002))
                                              (send TIMER stop)
                                              (set! Timer-Stopped? #t)
                                              (set! PreviousPositionings '()))]))

(define PAUSE_PLAY_BUTTON (new button%
                               [parent CONTROLS]
                               [label PAUSE_PLAY_BM]
                               [callback (lambda (obj evt)
                                           (if Timer-Stopped?
                                               (send TIMER start 16)
                                               (send TIMER stop))
                                           (set! Timer-Stopped? (not Timer-Stopped?)))]))


(define ZOOM_OUT_BUTTON (new button%
                             [parent CONTROLS]
                             [label ZOOM_OUT_BM]
                             [callback (lambda (obj evt)
                                         (ZOOM-))]))

(define ZOOM_IN_BUTTON (new button%
                            [parent CONTROLS]
                            [label ZOOM_IN_BM]
                            [callback (lambda (obj evt)
                                        (ZOOM+))]))

(define CENTER_GRAPH_BUTTON (new button%
                                 [parent CONTROLS]
                                 [label CENTER_GRAPH_BM]
                                 [enabled #t]
                                 [callback (lambda (obj evt)
                                             (Center-Graph))]))

(define SWITCH_MODE_BUTTON (new button%
                                [parent CONTROLS]
                                [label 3D_BM]
                                [callback (lambda (obj evt)
                                            (case Mode
                                              [("2D") (send TIMER stop)
                                                      (set! Timer-Stopped? #t)
                                                      (hash-clear! BlackHolePositioning)
                                                      (2D->3D)
                                                      (send obj set-label 2D_BM)
                                                      (for ([button (send TOOLBOX get-children)])
                                                        (send button enable #f))
                                                      (for ([button (send CONTROLS get-children)])
                                                        (when (and (not (equal? button PAUSE_PLAY_BUTTON))
                                                                   (not (equal? button SWITCH_MODE_BUTTON)))
                                                          (send button enable #f)))]
                                               
                                              [("3D") (set! Timer-Stopped? #t)
                                                      (send TIMER stop)
                                                      (3D->2D)
                                                      (send obj set-label 3D_BM)
                                                      (for ([button (send TOOLBOX get-children)])
                                                        (send button enable #t))
                                                      (for ([button (send CONTROLS get-children)])
                                                        (when (and (not (equal? button PAUSE_PLAY_BUTTON))
                                                                   (not (equal? button SWITCH_MODE_BUTTON)))
                                                          (send button enable #t)))]))]))

(define SETTINGS_BUTTON (new button%
                             [parent CONTROLS]
                             [label SETTINGS_BM]
                             [callback (lambda (obj evt)
                                         (send SETTINGS_POPUP show #t))]))


(define SETTINGS_POPUP (new dialog%
                            [label "Settings"]
                            [parent MAIN_UI]
                            [stretchable-height #f]
                            [stretchable-width #f]))

(define SettingsVPANEL (new vertical-panel%
                            [parent SETTINGS_POPUP]))

(define Type+SizeHPANEL (new horizontal-panel%
                             [parent SettingsVPANEL]))

(define GraphTypeGroupBox (new group-box-panel%
                               [label "Graph Type"]
                               [parent Type+SizeHPANEL]
                               [min-width 300]))

(define GraphTypeSelector (new radio-box%
                               [label ""]
                               [choices (list "Chain Graph" "Cyclic Graph" "Grid Graph" "Clique Graph" "Complete Tree Graph")]
                               [parent GraphTypeGroupBox]
                               [callback (λ (obj evt) (GraphSizeSelector (send obj get-selection)))]
                               [selection #f]))

(define GraphSizeGroupBox (new group-box-panel%
                               [label "Graph Size"]
                               [parent Type+SizeHPANEL]
                               [alignment '(center center)]
                               [min-width 200]))

(define (GraphSizeSelector n)
  (let* ([Input (new text-field%
                     [label "Number of Nodes:"]
                     [parent GraphSizeGroupBox]
                     [style '(single vertical-label)])]
         [Input-Height (new text-field%
                            [label "Number of Nodes in Height:"]
                            [parent GraphSizeGroupBox]
                            [style '(single vertical-label)])]
         [Input-Width (new text-field%
                           [label "Number of Nodes in Width:"]
                           [parent GraphSizeGroupBox]
                           [style '(single vertical-label)])]
         [Arity (new text-field%
                     [label "Arity:"]
                     [parent GraphSizeGroupBox]
                     [style '(single vertical-label)])]
         [Depth (new text-field%
                     [label "Depth:"]
                     [parent GraphSizeGroupBox]
                     [style '(single vertical-label)])]
         [PrevTextFields (send GraphSizeGroupBox get-children)])

    (case n
      [(0 1 3) (map (λ (child) (when (not (equal? child Input))
                                 (send GraphSizeGroupBox delete-child child))) PrevTextFields)
               Input]
      
      [(2) (map (λ (child) (when (not (or (equal? child Input-Height) (equal? child Input-Width)))
                             (send GraphSizeGroupBox delete-child child))) PrevTextFields)
           Input-Height
           Input-Width]
      
      [(4) (map (λ (child) (when (not (or (equal? child Arity) (equal? child Depth)))
                             (send GraphSizeGroupBox delete-child child))) PrevTextFields)
           Arity
           Depth])))

(define GraphSettingsChangeBUTTON (new button%
                                       [label "Save Graph Settings and Reset"]
                                       [parent SettingsVPANEL]
                                       [callback (λ (obj evt)
                                                   (define TypeSelection (send GraphTypeSelector get-selection))
                                                   (case TypeSelection
                                                     [(0 1 3) (let* ([Input (car (send GraphSizeGroupBox get-children))]
                                                                     [Nodes (send Input get-value)]
                                                                     [NodesVerif (Verify-Input* Nodes)])
                                                                (when NodesVerif
                                                                  (set! Nodes (string->number Nodes))
                                                                  (send TIMER stop)
                                                                  (set! Timer-Stopped? #t)
                                                                  (cond [(= TypeSelection 0) (set! Graph (chain-graph Nodes))]
                                                                        [(= TypeSelection 1) (set! Graph (cyclic-graph Nodes))]
                                                                        [(= TypeSelection 3) (set! Graph (clique-graph Nodes))])
                                                                  (set! GraphPositioning
                                                                        (random-positioning-of-node-list CANVAS_SIZE CANVAS_SIZE (get-nodes Graph)))
                                                                  (send CANVAS on-paint)))]
                                                     [(2) (let* ([Input-Height (car (send GraphSizeGroupBox get-children))]
                                                                 [Input-Width (cadr (send GraphSizeGroupBox get-children))]
                                                                 [H (send Input-Height get-value)]
                                                                 [W (send Input-Width get-value)]
                                                                 [HVerif (Verify-Input* H)]
                                                                 [WVerif (Verify-Input* W)])
                                                            (when (and HVerif WVerif)
                                                              (send TIMER stop)
                                                              (set! Timer-Stopped? #t)
                                                              (set! H (string->number (send Input-Height get-value)))
                                                              (set! W (string->number (send Input-Width get-value)))
                                                              (set! Graph (grid-graph H W))
                                                              (set! GraphPositioning
                                                                    (random-positioning-of-node-list CANVAS_SIZE CANVAS_SIZE (get-nodes Graph)))
                                                              (send CANVAS on-paint)))]
                                                     [(4) (let* ([Arity (car (send GraphSizeGroupBox get-children))]
                                                                 [Depth (cadr (send GraphSizeGroupBox get-children))]
                                                                 [A (send Arity get-value)]
                                                                 [D (send Depth get-value)]
                                                                 [AVerif (Verify-Input* A)]
                                                                 [DVerif (Verify-Input* D)])
                                                            (when (and AVerif DVerif)
                                                              (send TIMER stop)
                                                              (set! Timer-Stopped? #t)
                                                              (set! A (string->number (send Arity get-value)))
                                                              (set! D (string->number (send Depth get-value)))
                                                              (set! Graph (complete-tree-graph A D))
                                                              (set! GraphPositioning
                                                                    (random-positioning-of-node-list CANVAS_SIZE CANVAS_SIZE (get-nodes Graph)))
                                                              (send CANVAS on-paint)))])
                                                   (send SETTINGS_POPUP show #f))]))


(define ConstantsGroupBox (new group-box-panel%
                               [label "Constants"]
                               [parent SettingsVPANEL]))

(define ConstantsContainer (new horizontal-panel%
                                [parent ConstantsGroupBox]
                                [alignment '(center top)]))

(define C1-Edit (new text-field%
                     [label "c1 = "]
                     [parent ConstantsContainer]
                     [init-value (number->string (Relaxator 'get-c1))]))

(define C2-Edit (new text-field%
                     [label "c2 = "]
                     [parent ConstantsContainer]
                     [init-value (number->string (Relaxator 'get-c2))]))

(define C3-Edit (new text-field%
                     [label "c3 = "]
                     [parent ConstantsContainer]
                     [init-value (number->string (Relaxator 'get-c3))]))

(define C4-Edit (new text-field%
                     [label "c4 = "]
                     [parent ConstantsContainer]
                     [init-value (number->string (Relaxator 'get-c4))]))

(define ConstantsSettingsChangeBUTTON (new button%
                                           [label "Apply Constants"]
                                           [parent SettingsVPANEL]
                                           [callback (λ (obj evt)
                                                       (define C1-Input (send C1-Edit get-value))
                                                       (define C2-Input (send C2-Edit get-value))
                                                       (define C3-Input (send C3-Edit get-value))
                                                       (define C4-Input (send C4-Edit get-value))
                                                       (when (Verify-Input-Constants* C1-Input C2-Input C3-Input C4-Input)
                                                         (define New-C1 (string->number C1-Input))
                                                         (define New-C2 (string->number C2-Input))
                                                         (define New-C3 (string->number C3-Input))
                                                         (define New-C4 (string->number C4-Input))
                                                         (Relaxator 'set-c1 New-C1)
                                                         (Relaxator 'set-c2 New-C2)
                                                         (Relaxator 'set-c3 New-C3)
                                                         (Relaxator 'set-c4 New-C4)
                                                         (send SETTINGS_POPUP show #f)))]))

;; Sanitize User Input
;; Verify Input for Constants
(define (Verify-Input-Constants* . Inputs)
  (if (empty? Inputs)
      #f
      (andmap (λ (Input) (and (integer? (string->number Input)) (>= (string->number Input) 0))) Inputs)))

;; Verify Input for Graphs
(define (Verify-Input* . Inputs)
  (if (empty? Inputs)
      #f
      (andmap (λ (Input) (and (string->number Input) (>= (string->number Input) 0))) Inputs)))

;; Canvas

(define CANVAS (new Eventful-2D-Canvas%
                    [parent MIDDLE_VPANEL]
                    [min-height CANVAS_SIZE]
                    [min-width CANVAS_SIZE]
                    [style '(border)]
                    [stretchable-width #f]
                    [stretchable-height #f]
                    [paint-callback (lambda (obj dc) (Draw-Canvas-2D dc))]))

;; Right Vertical Panel Grouping: Color Selector and Size Selector.

(define RIGHT_VPANEL (new vertical-panel%
                          [parent MAIN_HPANEL]
                          [min-height MAIN_UI_HEIGHT]
                          [min-width RIGHT_VPANEL_WIDTH]
                          [stretchable-width #f]
                          [stretchable-height #f]))

(define COLOR_SELECTOR_CONTAINER (new group-box-panel%
                                      [label "Colors"]
                                      [parent RIGHT_VPANEL]))

(define COLOR_VIZUALIZER (new canvas%
                              [parent COLOR_SELECTOR_CONTAINER]
                              [min-height 20]
                              [min-width RIGHT_VPANEL_WIDTH]
                              [stretchable-width #f]
                              [stretchable-height #f]))

(define RED_SLIDER (new slider%
                        [label "Red"]
                        [init-value 255]
                        [min-value 0]	 
                        [max-value 255]
                        [style '(horizontal vertical-label)]
                        [parent COLOR_SELECTOR_CONTAINER]
                        [callback (lambda (obj evt)
                                    (Update-Color-Vizualizer))]))

(define GREEN_SLIDER (new slider%
                          [label "Green"]
                          [init-value 255]
                          [min-value 0]	 
                          [max-value 255]
                          [style '(horizontal vertical-label)]
                          [parent COLOR_SELECTOR_CONTAINER]
                          [callback (lambda (obj evt)
                                      (Update-Color-Vizualizer))]))

(define BLUE_SLIDER (new slider%
                         [label "Blue"]
                         [init-value 255]
                         [min-value 0]
                         [max-value 255]
                         [style '(horizontal vertical-label)]
                         [parent COLOR_SELECTOR_CONTAINER]
                         [callback (lambda (obj evt)
                                     (Update-Color-Vizualizer))]))

(define ALPHA_SLIDER (new slider%
                          [label "Alpha"]
                          [init-value 1000]
                          [min-value 0]	 
                          [max-value 1000]
                          [style '(horizontal plain)]
                          [parent COLOR_SELECTOR_CONTAINER]
                          [callback (lambda (obj evt)
                                      (Update-Color-Vizualizer))]))


(define NODES_COLOR_BUTTON (new button%
                                [label "Apply Color to Nodes"]
                                [parent COLOR_SELECTOR_CONTAINER]
                                [callback (lambda (obj evt)
                                            (let ([r 0]
                                                  [g 0]
                                                  [b 0]
                                                  [a 0])
                                              (set!-values (r g b a) (Get-Slider-Color))
                                              (set! NODES_COLOR (make-object color% r g b a))
                                              (set! NODES_COLOR_3D (list (/ r 255) (/ g 255) (/ b 255) a))
                                              (send CANVAS on-paint)))]))


(define EDGES_COLOR_BUTTON (new button%
                                [label "Apply Color to Edges"]
                                [parent COLOR_SELECTOR_CONTAINER]
                                [callback (lambda (obj evt)
                                            (let ([r 0]
                                                  [g 0]
                                                  [b 0]
                                                  [a 0])
                                              (set!-values (r g b a) (Get-Slider-Color))
                                              (set! EDGES_COLOR (make-object color% r g b a))
                                              (set! EDGES_COLOR_3D (list (/ r 255) (/ g 255) (/ b 255) a))
                                              (send CANVAS on-paint)))]))

(send MAIN_UI set-icon (read-bitmap "Images/graphx.png"))
(send MAIN_UI center)
(send MAIN_UI show #t)