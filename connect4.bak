#lang racket/gui
(require racket/draw)

;OPENING FRAME START
;Creating the start frame
(define start-frame (new frame%
                         [label "CONNECT FOUR MENU"]
                         [width 400]
                         [height 200]))

;Creating a vertical panel for the frame
(define start-vert-panel (new vertical-panel%
                              [parent start-frame]
                              [alignment '(center center)]
                              [horiz-margin 50]
                              [spacing 20]))


;Creating a title for the start frame to display which game it is
(define start-title (new message%
                         [label "Connect 4"]
                         [parent start-vert-panel]
                         [font (make-object font% 25 'default)]))

;Creating start button
(define start-btn (new button%
                       [parent start-vert-panel]
                       [label "START"]
                       [min-width 125] 
                       [min-height 25]
                       [callback (lambda (button event)
                                   (send start-frame show #f)
                                   (send game-frame show #t))]))

;Creating an Exit button
(define exit-btn (new button%
                      [parent start-vert-panel]
                      [label "EXIT"]
                      [min-width 125] 
                      [min-height 25]
                      [callback (lambda (button event)
                                  (send start-frame show #f))]))
;OPENING FRAME END


;GAME FRAME START
;Creating the game frame
(define game-frame (new frame%
                        [label "CONNECT 4"]
                        [width 800]
                        [height 650]))

;Creating a vertical panel in the game frame
(define vert-game-panel (new vertical-panel%
                             [parent game-frame]
                             [alignment '(center center)]))

;Creating a text message to alert whose turn it is
(define text (new message%
                  [label "Player 1's turn"]
                  [parent vert-game-panel]
                  [font (make-object font% 25 'default)]
                  ))

;Creating a text message to direct users on how to play the game
(define help-text (new message%
                  [label "Press on any button to make the X or O fall in that column"]
                  [parent vert-game-panel]
                  [font (make-object font% 15 'default)]
                  ))

;Creating a horizontal panel for buttons
(define btn-panel5 (new horizontal-panel%
                        [parent game-frame]
                        [alignment '(center center)]))

;Creating a horizontal panel for buttons
(define btn-panel4 (new horizontal-panel%
                        [parent game-frame]
                        [alignment '(center center)]))

;Creating a horizontal panel for buttons
(define btn-panel3 (new horizontal-panel%
                        [parent game-frame]
                        [alignment '(center center)]))

;Creating a horizontal panel for buttons
(define btn-panel2 (new horizontal-panel%
                        [parent game-frame]
                        [alignment '(center center)]))

;Creating a horizontal panel for buttons
(define btn-panel1 (new horizontal-panel%
                        [parent game-frame]
                        [alignment '(center center)]))

;Creating a horizontal panel for buttons
(define btn-panel0 (new horizontal-panel%
                        [parent game-frame]
                        [alignment '(center center)]))
;GAME FRAME END


;FUNCTIONS START
;Function to create vector table
(define (create-2d-vector row col init)
  (build-vector row (λ (x) (make-vector col init))))

;Function to get specified vectors
(define (2d-vector-find vector row col)
  (vector-ref (vector-ref vector row) col))

;Function to change vector values
(define (set-2d-vector vec row col val)
  (let ((v (vector-ref vec row)))
    (begin
      (vector-set! v col val)
      (vector-set! vec row v))))

;Function to create buttons
(define (create-btn parent-panel row col btn-name)
  (define button
    (new button%
         [parent parent-panel]
         [label ""]
         [min-width 80]
         [min-height 60]
         [callback (λ (button event)
                     (define coord col)
                     (disable-btn buttons coord)
                     (set! total-moves (+ total-moves 1))
                     (check-win connect4-vector)
                     (cond
                       ((equal? player 1) (begin
                                            (set! player 2)
                                            (send text set-label "Player 2's turn")))
                       (else (begin
                               (set! player 1)
                               (send text set-label "Player 1's turn")))))]))
  (set! buttons (cons (cons btn-name button) buttons))
  btn-name)

;Function to make multiple buttons for each panel
(define (create-multiple-btn parent-panel row cols)
  (for([col (in-range cols)])
    (define btn-name (format "btn-~a~a" row col))
    (create-btn parent-panel row col btn-name)))

;Function to disable button by name. Format of button: btn-(row)(col)
(define (disable-btn list col)
  (define row-check (check-column connect4-vector col))
  (define name
    (cond
      ((and (number? row-check) (< row-check 5)) (format "btn-~a~a" (+ row-check 1) col))
      ((not (number? row-check)) (format "btn-0~a" col))))
  (define row
    (cond
      ((number? row-check) (+ row-check 1))
      (else 0)))
  (for-each
   (λ (pair)
     (when (equal? (car pair) name)
       (send (cdr pair) enable #f)
       (cond
         ((equal? player 1)
          (begin
            (vector-set! (vector-ref connect4-vector row) col 0)
            (send (cdr pair) set-label "X")))
         (else
          (begin
            (vector-set! (vector-ref connect4-vector row) col 1)
            (send (cdr pair) set-label "O"))))))
   list))

;Function to disable all buttons
(define (disable-all list)
  (for-each
   (λ (pair)
     (send (cdr pair) enable #f))
   list))

;Function to check values in certain column and return the row of the last filled vector in the column
(define (check-column vec col)
  (define (check col-index counter)
    (cond
      ((< col-index 0) #f)
      ((number? (vector-ref (vector-ref vec col-index) col))
       counter)
      (else (check (- col-index 1) (- counter 1)))))
  
  (check (- (vector-length vec) 1) 5))

;WIN PATTERN CHECK START
;Function to check if vertices shown are in bounds of the vertex grid
(define(in-bound? vec x y)
  (define width (vector-length (vector-ref vec 0)))
  (define height (vector-length vec))
  (and (>= x 0) (< x height) (>= y 0) (< y width)))

;Function to check pattern horizontally
(define (horizontal-win vec i j)
  (cond
    [(and (in-bound? vec i j)
          (in-bound? vec i (+ j 3))
          (number? (vector-ref (vector-ref vec i) j))
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
    [else #f]))

;Function to check for pattern vertically
(define (vertical-win vec i j)
  (cond
    [(and (in-bound? vec i j)
          (in-bound? vec (+ i 3) j)
          (number? (vector-ref (vector-ref vec i) j))
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
    [else #f]))

;Function to check for pattern from bottom-left to top-right
(define (right-top-win vec i j)
  (cond
    [(and (in-bound? vec i j)
          (in-bound? vec (+ i 3) (+ j 3))
          (number? (vector-ref (vector-ref vec i) j))
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
    [else #f]))

;Fucntion to check for pattern from top-left to bottom-right
(define (left-top-win vec i j)
  (cond
    [(and (in-bound? vec i j)
          (in-bound? vec (- i 3) (+ j 3))
          (number? (vector-ref (vector-ref vec i) j))
          (number? (vector-ref (vector-ref vec (- i 1)) (+ j 1)))
          (number? (vector-ref (vector-ref vec (- i 2)) (+ j 2)))
          (number? (vector-ref (vector-ref vec (- i 3)) (+ j 3))))
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
    [else #f]))

;Function to check all win conditions
(define (check-win vec)
  (define width (vector-length (vector-ref vec 0)))
  (define height (vector-length vec))
  
  (define (in-bounds? i j)
    (and (>= i 0) (< i height) (>= j 0) (< j width)))
  
  (define (check-at i j)
    (cond
      [(and (in-bounds? i j) (horizontal-win vec i j)) #t]
      [(and (in-bounds? i j) (vertical-win vec i j)) #t]
      [(and (in-bounds? i j) (right-top-win vec i j)) #t]
      [(and (in-bounds? i j) (left-top-win vec i j)) #t]
      [else #f]))
  (define (iterate-positions)
    (define result #f)
    (define i 0)
    (define j 0)
    (define end-i height)
    (define end-j width)
    (do ((i i (+ i 1)))
      ((= i end-i) result)
      (do ((j j (+ j 1)))
        ((= j end-j) (set! j 0))
        (set! result (or result (check-at i j)))))
    (if (= total-moves 42)
      "Draw"
      result))
  (let ((winner (iterate-positions)))
    (if winner
        (show-winner winner)
        #f)))

;Function to declare winner
(define (show-winner winner)
  (define msg
    (cond
      ((equal? winner #t) (format "Player ~a won!" player))
      (else "DRAW")))
  
  ;Creating a result end frame
  (define end-frame (new frame%
                         [label "Result"]
                         [min-width 400]
                         [min-height 300]
                         [alignment '(center center)]))

  ;Creating vertical panel
  (define end-vert-panel (new vertical-panel%
                              [parent end-frame]
                              [alignment '(center center)]))

  ;Creating message boz to display who won
  (define msg-label (new message%
                         [parent end-vert-panel]
                         [label msg]
                         [font (make-object font% 25 'default)]))

  ;Creating an Exit button for the frame
  (define end-exit-btn (new button%
                            [parent end-vert-panel]
                            [label "EXIT"]
                            [vert-margin 25]
                            [min-width 125] 
                            [min-height 25]
                            [callback (lambda (button event)
                                        (send game-frame show #f)
                                        (send end-frame show #f))]))
  
  (disable-all buttons)
  (send end-frame show #t))
;WIN PATTERN CHECK END
;FUNCTIONS END


;INITIALIZATION
;Creating vector for the game
(define connect4-vector (create-2d-vector 6 7 #f))

;Variable to calculate total moves
(define total-moves 0)

;List to store button name and button in
(define buttons '())

;Calling function to create buttons for each panel
(create-multiple-btn btn-panel0 0 7)
(create-multiple-btn btn-panel1 1 7)
(create-multiple-btn btn-panel2 2 7)
(create-multiple-btn btn-panel3 3 7)
(create-multiple-btn btn-panel4 4 7)
(create-multiple-btn btn-panel5 5 7)

;Creating variable to track player
(define player 1)

;Starting the game
(send start-frame show #t)