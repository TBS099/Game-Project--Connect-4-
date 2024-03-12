#lang racket/gui
(require racket/draw)

;OPENING FRAME START
;Creating the start frame
(define start-frame (new frame%
                         [label "CONNECT FOUR MENU"]
                     [width 400]
                     [height 200]
                     ))

;Creating a vertical panel for the frame
(define start-vert-panel (new vertical-panel%
                              [parent start-frame]
                              [alignment '(center center)]
                              [horiz-margin 50]
                              [spacing 20]
                              ))


;Creating a title for the start frame to display which game it is
(define start-title (new message%
                   [label "Connect 4"]
                   [parent start-vert-panel]
                   [font (make-object font% 25 'default)]
                   ))

;Creating start button
(define start-btn (new button%
                       [parent start-vert-panel]
                       [label "START"]
                       [min-width 125] 
                       [min-height 25]
                       [callback (lambda (button event)
                                   (send start-frame show #f)
                                   (send game-frame show #t)
                                   )]
                       ))

;Creating an Exit button
(define exit-btn (new button%
                      [parent start-vert-panel]
                      [label "EXIT"]
                      [min-width 125] 
                      [min-height 25]
                      [callback (lambda (button event)
                                  (send start-frame show #f)
                                  )]
                      ))
;OPENING FRAME END


;GAME FRAME START
;Creating the game frame
(define game-frame (new frame%
                        [label "CONNECT 4"]
                        [width 800]
                        [height 650]
                        ))
;GAME FRAME END


;FUNCTIONS START
;Function to create vector table
(define (create-2d-vector row col init)
  (build-vector row (λ (x) (make-vector col init)))
  )

;Function to get specified vectors
(define (2d-vector-find vector row col)
  (vector-ref (vector-ref vector row) col)
  )

;Function to change vector values
(define (set-2d-vector vec row col val)
  (let ((v (vector-ref vec row)))
    (begin
      (vector-set! v col val)
      (vector-set! vec row v)
      )
    )
  )

;Creating vector for the game
(define connect4-vector (create-2d-vector 6 7 #f))

;Variable to calculate total moves
(define total-moves 0)

