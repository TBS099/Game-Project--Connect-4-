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

;Creating a horizontal panel for buttons
(define btn-panel (new horizontal-panel%
                       [parent game-frame]
                       [alignment '(center center)]
                       ))

;Creating a vertical panel in the game frame
(define vert-game-panel (new vertical-panel%
                            [parent game-frame]
                            [alignment '(center center)]
                            ))

;Function to create button for column 0
(define button0 (new button%
                     [parent btn-panel]
                     [label ""]
                     [min-width 80]
                     [min-height 60]
                     ))

;Function to create button for column 1
(define button1 (new button%
                     [parent btn-panel]
                     [label ""]
                     [min-width 80]
                     [min-height 60]
                     ))

;Function to create button for column 2
(define button2 (new button%
                     [parent btn-panel]
                     [label ""]
                     [min-width 80]
                     [min-height 60]
                     ))

;Function to create button for column 3
(define button3 (new button%
                     [parent btn-panel]
                     [label ""]
                     [min-width 80]
                     [min-height 60]
                     ))

;Function to create button for column 4
(define button4 (new button%
                     [parent btn-panel]
                     [label ""]
                     [min-width 80]
                     [min-height 60]
                     ))

;Function to create button for column 5
(define button5 (new button%
                     [parent btn-panel]
                     [label ""]
                     [min-width 80]
                     [min-height 60]
                     ))

;Function to create button for column 6
(define button6 (new button%
                     [parent btn-panel]
                     [label ""]
                     [min-width 80]
                     [min-height 60]
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

;WIN PATTERN CHECK START
;Function to check pattern horizontally
(define (horizontal-win vec i j)
  (cond
    [(and (number? (vector-ref (vector-ref vec i) j))
          (number? (vector-ref (vector-ref vec i) (+ j 1)))
          (number? (vector-ref (vector-ref vec i) (+ j 2)))
          (number? (vector-ref (vector-ref vec i) (+ j 3))))
     (cond
       [(= (vector-ref (vector-ref vec i) j)
           (vector-ref (vector-ref vec i) (+ j 1))
           (vector-ref (vector-ref vec i) (+ j 2))
           (vector-ref (vector-ref vec i) (+ j 3))
           1)1]
       [(= (vector-ref (vector-ref vec i) j)
           (vector-ref (vector-ref vec i) (+ j 1))
           (vector-ref (vector-ref vec i) (+ j 2))
           (vector-ref (vector-ref vec i) (+ j 3))
           0)0]
       [else #f])]
    [else #f]
    )
  )

;Function to check for pattern vertically
(define (vertical-win vec i j)
  (cond
    [(and (number? (vector-ref (vector-ref vec i) j))
          (number? (vector-ref (vector-ref vec (+ i 1)) j))
          (number? (vector-ref (vector-ref vec (+ i 2)) j))
          (number? (vector-ref (vector-ref vec (+ i 3)) j)))
     (cond
       [(= (vector-ref (vector-ref vec i) j)
           (vector-ref (vector-ref vec (+ i 1)) j)
           (vector-ref (vector-ref vec (+ i 2)) j)
           (vector-ref (vector-ref vec (+ i 3)) j)
           1)1]
       [(= (vector-ref (vector-ref vec i) j)
           (vector-ref (vector-ref vec (+ i 1)) j)
           (vector-ref (vector-ref vec (+ i 2)) j)
           (vector-ref (vector-ref vec (+ i 3)) j)
           0)0]
       [else #f])]
    [else #f]
    )
  )

;Function to check for pattern from bottom-left to top-right
(define (right-top-win vec i j)
  (cond
    [(and (number? (vector-ref (vector-ref vec i) j))
          (number? (vector-ref (vector-ref vec (+ i 1)) (+ j 1)))
          (number? (vector-ref (vector-ref vec (+ i 2)) (+ j 2)))
          (number? (vector-ref (vector-ref vec (+ i 3)) (+ j 3))))
     (cond
       [(= (vector-ref (vector-ref vec i) j)
           (vector-ref (vector-ref vec (+ i 1)) (+ j 1))
           (vector-ref (vector-ref vec (+ i 2)) (+ j 2))
           (vector-ref (vector-ref vec (+ i 3)) (+ j 3))
           1)1]
       [(= (vector-ref (vector-ref vec i) j)
           (vector-ref (vector-ref vec (+ i 1)) (+ j 1))
           (vector-ref (vector-ref vec (+ i 2)) (+ j 2))
           (vector-ref (vector-ref vec (+ i 3)) (+ j 3))
           0)0]
       [else #f])]
    [else #f]
    )
  )

;Fucntion to check for pattern from top-left to bottom-right
(define (left-top-win vec i j)
  (cond
    [(and (number? (vector-ref (vector-ref vec i) j))
          (number? (vector-ref (vector-ref vec (+ i 1)) (+ j 1)))
          (number? (vector-ref (vector-ref vec (+ i 2)) (+ j 2)))
          (number? (vector-ref (vector-ref vec (+ i 3)) (+ j 3))))
     (cond
       [(= (vector-ref (vector-ref vec i) j)
           (vector-ref (vector-ref vec (- i 1)) (+ j 1))
           (vector-ref (vector-ref vec (- i 2)) (+ j 2))
           (vector-ref (vector-ref vec (- i 3)) (+ j 3))
           1)1]
       [(= (vector-ref (vector-ref vec i) j)
           (vector-ref (vector-ref vec (- i 1)) (+ j 1))
           (vector-ref (vector-ref vec (- i 2)) (+ j 2))
           (vector-ref (vector-ref vec (- i 3)) (+ j 3))
           0)0]
       [else #f])]
    [else #f]
    )
  )
;WIN PATTERN CHECK END

;Function to check values in certain column and return the row of the last filled vector in the column
(define (check-column vec col)
  (define (check col-index counter)
    (cond
      ((< col-index 0) #f)
      ((number? (vector-ref (vector-ref vec col-index) col))
       counter)
      (else (check (- col-index 1) (+ counter 1)))))
  
  (check (- (vector-length vec) 1) 0))

;Starting the game
(send start-frame show #t)