;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; to run the game with the scenario for task 7 call pipe-fantasy with GAMESTATE-S

(require 2htdp/image)
(require 2htdp/universe)

(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A #true for 
;; one of top, bot, left, right indicates an opening in that direction. 

(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TR (make-pipe #true #false #false #true))
(define PIPE-BL (make-pipe #false #true #true #false))
(define PIPE-BR (make-pipe #false #true #false #true))
(define PIPE-TB (make-pipe #true #true #false #false))
(define PIPE-LR (make-pipe #false #false #true #true))  
(define PIPE-TBLR (make-pipe #true #true #true #true))
(define PIPE-T (make-pipe #true #false #false #false))
(define PIPE-B (make-pipe #false #true #false #false))
(define PIPE-L (make-pipe #false #false #true #false)) 
(define PIPE-R (make-pipe #false #false #false #true))

(define (pipe-temp p)
  (... (pipe-top p) ... (pipe-bot p) ... (pipe-left p) ... (pipe-right) ...))

(define ALL-PIPES (list PIPE-TL PIPE-TR PIPE-BL PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR))  

;; pipe->image: Pipe Integer Integer Boolean Pipe -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length.
;; If filled? then draw the pipe with goo.
(define (pipe->image pipe tile-side-length pipe-width filled? gfp)
  (place-image
   (if (and filled? (pipe-top gfp)) (rectangle pipe-width (* 2(/ tile-side-length 3)) "solid" "green")
       empty-image)
   (/ tile-side-length 2) (/ tile-side-length 3)
   (place-image
    (if (and filled? (pipe-bot gfp)) (rectangle pipe-width (* 2(/ tile-side-length 3)) "solid" "green")
        empty-image)
    (/ tile-side-length 2) (* (/ tile-side-length 6) 4) 
    (place-image
     (if (and filled? (pipe-left gfp)) (rectangle (* 2(/ tile-side-length 3)) pipe-width "solid" "green")
         empty-image)
     (/ tile-side-length 3) (/ tile-side-length 2)
     (place-image
      (if (and filled? (pipe-right gfp)) (rectangle (* 2(/ tile-side-length 3)) pipe-width "solid" "green")
          empty-image)
      (* (/ tile-side-length 6) 4) (/ tile-side-length 2)
      (place-image
       (if (pipe-top pipe) (rectangle pipe-width (* 2(/ tile-side-length 3)) "solid" "black") 
           empty-image)
       (/ tile-side-length 2) (/ tile-side-length 3)
       (place-image
        (if (pipe-bot pipe) (rectangle pipe-width (* 2(/ tile-side-length 3)) "solid" "black")
            empty-image)
        (/ tile-side-length 2) (* (/ tile-side-length 6) 4) 
        (place-image
         (if (pipe-left pipe) (rectangle (* 2(/ tile-side-length 3)) pipe-width "solid" "black")
            empty-image)
         (/ tile-side-length 3) (/ tile-side-length 2)
         (place-image
          (if (pipe-right pipe) (rectangle (* 2(/ tile-side-length 3)) pipe-width "solid" "black")
              empty-image)
          (* (/ tile-side-length 6) 4) (/ tile-side-length 2)
          (square tile-side-length "solid" "gray")))))))))) 

(define-struct pipe-pos [r c pipe])
; A pipe-pos is a (make-pipe-pos NAT NAT Pipe)
; r - is the row of the grid
; c - is the column of the grid
; pipe - is a Pipe
; A pipe with its position in the grid

(define (pipe-pos-temp p)
  (... (pipe-pos-r p) ... (pipe-pos-c p) ... (pipe-temp (pipe-pos-pipe p)) ...))

(define PIPE-POS-1 (make-pipe-pos 1 1 PIPE-TR))
(define PIPE-POS-2 (make-pipe-pos 1 2 PIPE-TB))
(define PIPE-POS-3 (make-pipe-pos 2 6 PIPE-LR))
(define PIPE-POS-4 (make-pipe-pos 3 0 PIPE-R))

(define-struct grid [size placedL])
; A grid is a (make-grid NAT [ListOfPipes-pos])
; size - number for the n by n for the grid
; placedL - A list of each placed pipe in the grid, either empty '() or a list of pipe-posns

(define (grid-temp p)
  (... (grid-size p) ... (grid-placedL p) ...))

(define GRID-1-L (cons PIPE-POS-1 (list PIPE-POS-2)))  
(define GRID-3-L (cons PIPE-POS-1 (cons PIPE-POS-2 (list PIPE-POS-3))))

(define GRID-1 (make-grid 5 GRID-1-L))
(define GRID-2 (make-grid 4 '()))
(define GRID-3 (make-grid 7 GRID-3-L))

(define STARTING-GRID (make-grid 7 (list PIPE-POS-4))) 

;; Design data called GooFlow that represents the path taken by the goo and the direction in which it is flowing.
;; You can modify existing data if you wish, but it isn’t necessary to do so.

(define-struct GooFlow [path direction])
; A GooFlow is a (make-GooFlow [ListOfPipes-pos] string)
; path - A list of the placed pipe-posns in the grid that have been filled in with goo
; direction - a string representing the direction in which the goo is flowing - one of:
; "top"
; "bot"
; "left"
; "right"

(define (GooFlow-temp g)
  (... (GooFlow-path g) ... (GooFlow-direction g) ...))

(define GOO-1-L (cons PIPE-POS-4 (cons PIPE-POS-1 (list PIPE-POS-2))))
(define GOO-2-L (list PIPE-POS-4))
(define GOO-3-L (cons PIPE-POS-4 (cons PIPE-POS-1 (cons PIPE-POS-2 (list PIPE-POS-3)))))

(define GOO1 (make-GooFlow GOO-1-L "top"))
(define GOO2 (make-GooFlow GOO-3-L "bot"))
(define GOO3 (make-GooFlow GOO-2-L "right"))

; grid-goo-propagate : GooFlow Grid -> GooFlow
; moves the goo forward by one tile. If the goo is stuck, produce the same goo.

(check-expect (grid-goo-propagate GOO3 (make-grid 7 (list (make-pipe-pos 3 1 PIPE-LR) PIPE-POS-4)))
              (make-GooFlow (list (make-pipe-pos 3 1 PIPE-LR) PIPE-POS-4) "right"))
(check-expect (grid-goo-propagate GOO3 (make-grid 7 (list (make-pipe-pos 3 1 PIPE-TL) PIPE-POS-4)))
              (make-GooFlow (list (make-pipe-pos 3 1 PIPE-TL) PIPE-POS-4) "top"))
(check-expect (grid-goo-propagate GOO3 (make-grid 7 (list (make-pipe-pos 3 1 PIPE-BL) PIPE-POS-4)))
              (make-GooFlow (list (make-pipe-pos 3 1 PIPE-BL) PIPE-POS-4) "bot"))

(define (grid-goo-propagate goo grid)
  (local [(define TOP (pipe-at grid (- (pipe-pos-r (first (GooFlow-path goo))) 1) (pipe-pos-c (first (GooFlow-path goo)))))
          (define BOT  (pipe-at grid (+ (pipe-pos-r (first (GooFlow-path goo))) 1) (pipe-pos-c (first (GooFlow-path goo)))))
          (define LEFT (pipe-at grid (pipe-pos-r (first (GooFlow-path goo))) (- (pipe-pos-c (first (GooFlow-path goo))) 1)))
          (define RIGHT (pipe-at grid (pipe-pos-r (first (GooFlow-path goo))) (+ (pipe-pos-c (first (GooFlow-path goo))) 1)))
          ; buildc : string -> [ListOf Pipe-Pos]
          ; helper to build the list of pipe-posn for GooFlow
          (define (buildc d)
            (cond [(string=? d "top")
                   (cons (make-pipe-pos (- (pipe-pos-r (first (GooFlow-path goo))) 1) (pipe-pos-c (first (GooFlow-path goo)))
                                        (cross-pipe TOP d)) (GooFlow-path goo))]
                  [(string=? d "bot")
                   (cons (make-pipe-pos (+ (pipe-pos-r (first (GooFlow-path goo))) 1) (pipe-pos-c (first (GooFlow-path goo)))
                                        (cross-pipe BOT d)) (GooFlow-path goo))]
                  [(string=? d "left")
                   (cons (make-pipe-pos (pipe-pos-r (first (GooFlow-path goo))) (- (pipe-pos-c (first (GooFlow-path goo))) 1)
                                        (cross-pipe LEFT d)) (GooFlow-path goo))] 
                  [(string=? d "right")
                   (cons (make-pipe-pos (pipe-pos-r (first (GooFlow-path goo))) (+ (pipe-pos-c (first (GooFlow-path goo))) 1)
                                        (cross-pipe RIGHT d)) (GooFlow-path goo))]))
          ; cross-pipe : Pipe direction -> Pipe
          ; helper that checks if there is a cross pipe and only updates it as filled for the direction of the GooFlow
          (define (cross-pipe p d)
            (cond [(and (or (string=? d "top") (string=? d "bot")) (pipe-top p) (pipe-bot p) (pipe-left p))
                   (make-pipe #t #t #f #f)]
                  [(and (or (string=? d "left") (string=? d "right")) (pipe-top p) (pipe-bot p) (pipe-left p)) 
                   (make-pipe #f #f #t #t)] 
                  [else p]))
          ; move-up Pipe -> GooFlow
          ; updates the GooFlow for upward direction 
          (define (move-up up)
            (cond [(and (pipe-top up) (pipe-bot up)) (make-GooFlow (buildc (GooFlow-direction goo)) (GooFlow-direction goo))]
                  [(and (pipe-left up) (pipe-bot up)) (make-GooFlow (buildc (GooFlow-direction goo)) "left")] 
                  [(and (pipe-right up) (pipe-bot up)) (make-GooFlow (buildc (GooFlow-direction goo)) "right")]
                  [else goo]))
          ; move-down Pipe -> GooFlow
          ; updates the GooFlow for downward direction
          (define (move-down down)
            (cond [(and (pipe-top down) (pipe-bot down)) (make-GooFlow (buildc (GooFlow-direction goo)) (GooFlow-direction goo))]
                  [(and (pipe-left down) (pipe-top down)) (make-GooFlow (buildc (GooFlow-direction goo)) "left")]
                  [(and (pipe-right down) (pipe-top down)) (make-GooFlow (buildc (GooFlow-direction goo)) "right")]
                  [else goo]))
          ; move-left Pipe -> GooFlow
          ; updates the GooFlow for leftward direction
          (define (move-left left)
            (cond [(and (pipe-left left) (pipe-right left)) (make-GooFlow (buildc (GooFlow-direction goo)) (GooFlow-direction goo))]
                  [(and (pipe-top left) (pipe-right left)) (make-GooFlow (buildc (GooFlow-direction goo)) "top")]
                  [(and (pipe-bot left) (pipe-right left)) (make-GooFlow (buildc (GooFlow-direction goo)) "bot")]
                  [else goo]))
          ; move-right Pipe -> GooFlow
          ; updates the GooFlow for rightward direction
          (define (move-right right)
            (cond [(and (pipe-left right) (pipe-right right)) (make-GooFlow (buildc (GooFlow-direction goo)) (GooFlow-direction goo))]
                  [(and (pipe-top right) (pipe-left right)) (make-GooFlow (buildc (GooFlow-direction goo)) "top")]
                  [(and (pipe-bot right) (pipe-left right)) (make-GooFlow (buildc (GooFlow-direction goo)) "bot")]
                  [else goo]))]
    (cond [(and (string=? (GooFlow-direction goo) "top") (pipe? TOP)) (move-up TOP)] 
          [(and (string=? (GooFlow-direction goo) "bot") (pipe? BOT)) (move-down BOT)]
          [(and (string=? (GooFlow-direction goo) "left") (pipe? LEFT)) (move-left LEFT)]
          [(and (string=? (GooFlow-direction goo) "right") (pipe? RIGHT)) (move-right RIGHT)] 
          [else goo])))             

;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
(check-expect (place-pipe GRID-1 PIPE-TB 3 4) (make-grid 5 (list (make-pipe-pos 3 4 PIPE-TB) PIPE-POS-1 PIPE-POS-2)))
(check-expect (place-pipe GRID-1 PIPE-TB 6 6) (make-grid 5 (list (make-pipe-pos 6 6 PIPE-TB) PIPE-POS-1 PIPE-POS-2)))
(check-expect (place-pipe GRID-1 PIPE-TB 1 1) (make-grid 5 (list (make-pipe-pos 1 1 PIPE-TB) PIPE-POS-2)))

(define (place-pipe grid pipe row col)
  (local [; pipe-pos=? : Pipe-Pos Pipe-Pos -> Boolean
          ; determines if two pipe-pos's are the same
          (define (pipe-pos=? p1 p2)
            (and (= (pipe-pos-r p1) (pipe-pos-r p2))
                 (= (pipe-pos-c p1) (pipe-pos-c p2))
                 (boolean=? (pipe-top (pipe-pos-pipe p1)) (pipe-top (pipe-pos-pipe p2)))
                 (boolean=? (pipe-bot (pipe-pos-pipe p1)) (pipe-bot (pipe-pos-pipe p2)))
                 (boolean=? (pipe-left (pipe-pos-pipe p1)) (pipe-left (pipe-pos-pipe p2)))
                 (boolean=? (pipe-right (pipe-pos-pipe p1)) (pipe-right (pipe-pos-pipe p2)))))
          ; remove : (x) x [ListOf x] -> [ListOf X] 
          ; removes an element if it is in the list and returns the updated list
          (define (remove-pipe-pos p l)
            (cond [(empty? l) '()]
                  [(pipe-pos=? (first l) p) (remove-pipe-pos p (rest l))]
                  [else (cons (first l) (remove-pipe-pos p (rest l)))]))]
  (if (pipe? (pipe-at grid row col))
      (make-grid (grid-size grid)
                 (cons (make-pipe-pos row col pipe) (remove-pipe-pos (make-pipe-pos row col (pipe-at grid row col)) (grid-placedL grid))))  
      (make-grid (grid-size grid) (cons (make-pipe-pos row col pipe) (grid-placedL grid)))))) 

;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
(check-expect (pipe-at GRID-1 1 1) PIPE-TR)
(check-expect (pipe-at GRID-1 1 2) PIPE-TB)
(check-expect (pipe-at GRID-1 1 3) #f)

(define (pipe-at grid row col)
  (cond [(empty? (grid-placedL grid)) #f]
        [(cons? (grid-placedL grid))
         (if (and (= (pipe-pos-r (first (grid-placedL grid))) row) 
                  (= (pipe-pos-c (first (grid-placedL grid))) col))
                  (pipe-pos-pipe (first (grid-placedL grid)))
                  (pipe-at (make-grid (grid-size grid) (rest (grid-placedL grid))) row col))]))                                          

;; grid->image: Grid Integer Integer GooFlow -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.
(define (grid->image grid tile-side-length pipe-width gf)
  (local [; NAT -> image
          ; creates a row
          (define (make-row row col)
            (if (= col (grid-size grid))
                empty-image
                (beside (box->image (pipe-at grid row col) tile-side-length pipe-width row col) (make-row row (+ col 1)))))
          ; cell->image : [optionalPipe] NAT NAT -> image
          ; makes an optional pipe into an image
          (define (box->image pipe sl w r c)
            (if (false? pipe) (square sl "outline" "black")
                (overlay (pipe->image pipe sl w (goo? r c (GooFlow-path gf)) (gfp r c (GooFlow-path gf))) (square sl "outline" "black"))))
          ; NAT -> image
          ; creates the rest of the rows
          (define (make-grid row)
            (if (= row (grid-size grid))
                empty-image
                (above (make-row row 0) (make-grid (+ row 1)))))
          ; goo? : Nat Nat [ListOf Pipe-Pos] -> boolean 
          ; is a certain pipe contained in GooFlow?
          (define (goo? r c gfl)
            (cond [(empty? gfl) #f] 
                  [(cons? gfl) 
                   (if (and (= r (pipe-pos-r (first gfl))) (= c (pipe-pos-c (first gfl))))
                       #t
                       (goo? r c (rest gfl)))]))
          ; gfp : Nat Nat [ListOf Pipe-Pos] -> [Optional Pipe]
          ; If the certain pipe is contained in the GooFLow it will be returned otherwise #f
          (define (gfp r c gfl)
            (cond [(empty? gfl) #f] 
                  [(cons? gfl) 
                   (if (and (= r (pipe-pos-r (first gfl))) (= c (pipe-pos-c (first gfl))))
                       (cross-full (first gfl) r c (rest gfl))
                       (gfp r c (rest gfl)))]))
          ; cross-full Nat Nat [ListOf Pipe-Pos] -> Pipe
          ; if a pipe appears at the same location in the GooFlow more than once then it creates a new full cross pipe
          (define (cross-full p r c gfl)
            (cond [(empty? gfl) (pipe-pos-pipe p)]
                  [(cons? gfl)
                   (if (and (= r (pipe-pos-r (first gfl))) (= c (pipe-pos-c (first gfl))))
                       (make-pipe #t #t #t #t)
                       (cross-full p r c (rest gfl)))]))] 
    (make-grid 0)))  

(define-struct GameState [grid incoming-pipesL tile-side-length pipe-width GooFlow replaced tick])
;; A GameState is a (make-gamestate Grid [ListOfPipes] Nat Nat GooFlow Nat Nat)
;; grid - is a grid
;; incoming-pipesL - is a list of incoming pipes
;; tile-side-length - is the length of the boxes
;; pipe-width - is the width of the pipes
;; GooFlow - is a GooFlow
;; replaced - is the number of pipes that have been replaced
;; tick - is the amount of time (in ticks) that has passed since the game has started
;; Interpretation: The state of the current game with the placed pipes and remaing pipes on a grid and status of the GooFlow and score

(define (GameState-temp p)
  (... (grid-temp (GameState-grid p))
       ... (GameState-incoming-pipesL p)
       ... (GameState-tile-side-length p)
       ... (GameState-pipe-width p)
       ... (GooFlow-temp (GameState-GooFlow))))

(define GAMESTATE-1 (make-GameState GRID-1 (list PIPE-TL PIPE-TR PIPE-BL PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR) 80 40 GOO1 0 140))
(define GAMESTATE-2 (make-GameState GRID-2 (list PIPE-TL PIPE-TR PIPE-BL PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR) 80 40 GOO2 0 140)) 
(define GAMESTATE-3 (make-GameState STARTING-GRID ALL-PIPES 60 20 GOO3 0 140))

;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState`
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.

(check-expect (place-pipe-on-click GAMESTATE 90 90 "button-down")
              (make-GameState (make-grid 7 (list (make-pipe-pos 1 1 PIPE-TL) PIPE-POS-4)) (rest ALL-PIPES) 90 30 GOO3 0 140))
(check-expect (place-pipe-on-click GAMESTATE 180 90 "button-down")
              (make-GameState (make-grid 7 (list (make-pipe-pos 1 2 PIPE-TL) PIPE-POS-4)) (rest ALL-PIPES) 90 30 GOO3 0 140))
(check-expect (place-pipe-on-click GAMESTATE 115 180 "button-down")
              (make-GameState (make-grid 7 (list (make-pipe-pos 2 1 PIPE-TL) PIPE-POS-4)) (rest ALL-PIPES) 90 30 GOO3 0 140))
(check-expect (place-pipe-on-click GAMESTATE 115 275 "button-down")
              (make-GameState (make-grid 7 (list (make-pipe-pos 3 1 PIPE-TL) PIPE-POS-4)) (rest ALL-PIPES) 90 30 GOO3 0 140))
               
(define (place-pipe-on-click GameState x y event)
  (local [; has-goo? : Nat Nat GooFlow -> Boolean
          ; tests if the given pipe has goo in it
          (define (has-goo? r c gf)
            (local [(define (check? pos)
                      (and (= r (pipe-pos-r pos))
                           (= c (pipe-pos-c pos))))]
              (ormap check? (GooFlow-path gf))))
          ; box-click: GameState Nat Nat -> GameState
          ; helper function for place-pipe-on-click that identifies which box is clicked and places the pipe
          (define (box-click gs r c)
            (cond [(and (pipe? (pipe-at (GameState-grid gs) r c)) (cons? (GameState-incoming-pipesL gs))
                     (not (has-goo? r c (GameState-GooFlow gs))))
                     (make-GameState
                      (place-pipe (GameState-grid gs) (first (GameState-incoming-pipesL gs)) r c)
                      (rest (GameState-incoming-pipesL gs)) (GameState-tile-side-length gs)
                      (GameState-pipe-width gs) (GameState-GooFlow gs) (add1 (GameState-replaced gs)) (GameState-tick gs))]
                    [(and (cons? (GameState-incoming-pipesL gs)) (not (has-goo? r c (GameState-GooFlow gs))))
                     (make-GameState
                      (place-pipe (GameState-grid gs) (first (GameState-incoming-pipesL gs)) r c)
                      (rest (GameState-incoming-pipesL gs)) (GameState-tile-side-length gs)
                      (GameState-pipe-width gs) (GameState-GooFlow gs) (GameState-replaced gs) (GameState-tick gs))]
                    [else gs]))]
    (cond [(string=? "button-down" event)
           (if (> y 629)
               GameState 
               (box-click GameState (floor (/ y (GameState-tile-side-length GameState)))
                          (floor (/ x (GameState-tile-side-length GameState)))))]
          [else GameState])))   

; get-score: GameState -> Integer
; calculates the current score in the game
(check-expect (get-score GAMESTATE) 0)
(check-expect (get-score GAMESTATE-1) 100) 
(check-expect (get-score GAMESTATE-2) 150)
(check-expect (get-score GAMESTATE-3) 0) 

(define (get-score GameState)
  (* 50 (- (sub1 (length (GooFlow-path (GameState-GooFlow GameState)))) (GameState-replaced GameState)))) 

; game-tick : GameState -> GameState
; handler function for on tick that activates the grid-goo-propagate
(check-expect (game-tick GAMESTATE)
              (make-GameState (make-grid 7 (list PIPE-POS-4)) ALL-PIPES 90 30 GOO3 0 139))
(check-expect (game-tick (make-GameState (make-grid 7 (list PIPE-POS-4)) ALL-PIPES 90 30 GOO3 0 1))
              (make-GameState (make-grid 7 (list PIPE-POS-4)) ALL-PIPES 90 30 GOO3 0 0))
(check-expect (game-tick (make-GameState (make-grid 7 (list (make-pipe-pos 3 1 PIPE-LR) PIPE-POS-4)) ALL-PIPES 90 30 GOO3 0 0))
              (make-GameState (make-grid 7 (list (make-pipe-pos 3 1 PIPE-LR) PIPE-POS-4)) ALL-PIPES 90 30
               (make-GooFlow (list (make-pipe-pos 3 1 PIPE-LR) PIPE-POS-4) "right") 0 28))
(check-expect (game-tick (make-GameState (make-grid 7 (list PIPE-POS-4)) ALL-PIPES 90 30 GOO3 0 0))
              (make-GameState (make-grid 7 (list PIPE-POS-4)) ALL-PIPES 90 30 GOO3 0 0))

(define (game-tick gs)
  (cond [(> (GameState-tick gs) 0)
         (make-GameState (GameState-grid gs) (GameState-incoming-pipesL gs) (GameState-tile-side-length gs) (GameState-pipe-width gs)
          (GameState-GooFlow gs) (GameState-replaced gs) (sub1 (GameState-tick gs)))]
        [(= (length (GooFlow-path (GameState-GooFlow gs)))
            (length (GooFlow-path (grid-goo-propagate (GameState-GooFlow gs) (GameState-grid gs))))) gs]
        [(= (GameState-tick gs) 0)
         (make-GameState (GameState-grid gs) (GameState-incoming-pipesL gs) (GameState-tile-side-length gs) (GameState-pipe-width gs)
                         (grid-goo-propagate (GameState-GooFlow gs) (GameState-grid gs)) (GameState-replaced gs) 28)]))      

; gamestate-init : Nat Nat Nat string [ListOf Pipes] -> GameState
; initializes a GameState based off of a grid size, starting position, direction and list of incoming pipes
(check-expect (gamestate-init 7 3 0 "right" ALL-PIPES) GAMESTATE) 
(check-expect (gamestate-init 6 3 0 "right" ALL-PIPES)
              (make-GameState (make-grid 6 (list PIPE-POS-4)) ALL-PIPES 90 30 GOO3 0 140))
(check-expect (gamestate-init 7 3 1 "right" ALL-PIPES)
              (make-GameState (make-grid 7 (list (make-pipe-pos 3 1 PIPE-R))) ALL-PIPES 90 30
                              (make-GooFlow (list (make-pipe-pos 3 1 PIPE-R)) "right") 0 140))  

(define (gamestate-init n x y direction incoming-pipes)
  (local [; int-pipe : string -> Pipe
          ; initializes the starting Pipe based off the directional string
          (define (int-pipe d)
            (cond [(string=? "top" d) (make-pipe #t #f #f #f)]    
                  [(string=? "bot" d) (make-pipe #f #t #f #f)] 
                  [(string=? "left" d) (make-pipe #f #f #t #f)]  
                  [(string=? "right" d) (make-pipe #f #f #f #t)]))]
    (make-GameState (make-grid n (list (make-pipe-pos x y (int-pipe direction)))) incoming-pipes 90 30 
                    (make-GooFlow (list (make-pipe-pos x y (int-pipe direction))) direction) 0 140))) 

(define GAMESTATE (gamestate-init 7 3 0 "right" ALL-PIPES)) 

(define START-P (make-pipe-pos 1 1 PIPE-R))
(define PIPE-POS-S1 (make-pipe-pos 1 2 PIPE-BL))
(define PIPE-POS-S2 (make-pipe-pos 2 2 PIPE-TBLR))
(define PIPE-POS-S3 (make-pipe-pos 3 2 PIPE-TB))
(define PIPE-POS-S4 (make-pipe-pos 4 2 PIPE-TL))
(define PIPE-POS-S5 (make-pipe-pos 4 1 PIPE-TR))
(define PIPE-POS-S6 (make-pipe-pos 3 1 PIPE-TBLR))
(define PIPE-POS-S7 (make-pipe-pos 2 1 PIPE-BR))
(define PIPE-POS-S8 (make-pipe-pos 2 3 PIPE-LR))
(define PIPE-POS-S9 (make-pipe-pos 2 4 PIPE-TL))
(define GOOS (make-GooFlow (list START-P) "right"))
(define LIST-S (list PIPE-POS-S9 PIPE-POS-S8 PIPE-POS-S7 PIPE-POS-S6 PIPE-POS-S5 PIPE-POS-S4 PIPE-POS-S3 PIPE-POS-S2 PIPE-POS-S1 START-P))
(define GAMESTATE-S (make-GameState (make-grid 7 LIST-S) (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL) 90 30 GOOS 0 0))   

; GameState->image : GameState -> image
; creates an image based of the current game state
(define (GameState->image gs)
  (above (grid->image (GameState-grid gs) (GameState-tile-side-length gs) (GameState-pipe-width gs) (GameState-GooFlow gs))
         (beside (if (<= 1 (length (GameState-incoming-pipesL gs)))
                     (pipe->image (first (GameState-incoming-pipesL gs)) (GameState-tile-side-length gs) (GameState-pipe-width gs) #f #f)
                     (square (GameState-tile-side-length gs) "outline" "black")) 
                 (if (<= 2 (length (GameState-incoming-pipesL gs)))
                     (pipe->image (second (GameState-incoming-pipesL gs)) (GameState-tile-side-length gs) (GameState-pipe-width gs) #f #f)
                     (square (GameState-tile-side-length gs) "outline" "black"))
                 (if (<= 3 (length (GameState-incoming-pipesL gs)))
                     (pipe->image (third (GameState-incoming-pipesL gs)) (GameState-tile-side-length gs) (GameState-pipe-width gs) #f #f)
                     (square (GameState-tile-side-length gs) "outline" "black"))
                 (if (<= 4 (length (GameState-incoming-pipesL gs)))
                     (pipe->image (fourth (GameState-incoming-pipesL gs)) (GameState-tile-side-length gs) (GameState-pipe-width gs) #f #f)
                     (square (GameState-tile-side-length gs) "outline" "black"))
                 (text (string-append "Score: " (number->string (get-score gs))) 50 "black"))))   

;; pipe-fantasy: GameState -> GameState
(define (pipe-fantasy initial-GameState)
  (big-bang initial-GameState
    [to-draw GameState->image]
    [on-mouse place-pipe-on-click]
    [on-tick game-tick]))     
