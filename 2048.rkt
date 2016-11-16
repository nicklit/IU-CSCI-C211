;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |2048|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image) (require 2htdp/universe) (require ffi/unsafe)

;;;;USE (play 4) below in DrRacket to start the game;;;;

;(* 8 (compiler-sizeof 'long))
;; 32 bit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;A10;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;Scroll down for A11;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;Problem 1;;
;;;;;;;;;;;;

; join-together : [ListOf X] [ListOf X] -> [List Of X] ; (join-together ls1 ls2) returns a list containing the top level ; elements of ls1 and ls2
(define (join-together ls1 ls2)
  (cond
    [(and (empty? ls1) (empty? ls2)) '()]
    [(empty? ls1) (cons (first ls2)
                        (join-together ls1 (rest ls2)))]
    [else  (cons (first ls1)
                 (join-together (rest ls1) ls2))]))

(check-expect (join-together '(a b c) '(d e f g h))
              (list 'a 'b 'c 'd 'e 'f 'g 'h))

(check-expect (join-together '() '(7 2 0 1 8 3 4))
              (list 7 2 0 1 8 3 4))


;;;;;;;;;;;;
;Problem 2;;
;;;;;;;;;;;;

;2a
; flatten/v1 : [ListOf [ListOf X]] -> [ListOf X] ; (flatten/v1 lls) returns one giant list of all lists in lls
(define (flatten/v1 lls)
  (cond
    [(empty? lls) '()]
    [else (join-together (first lls)
                         (flatten/v1 (rest lls)))]))

; flatten/v2 : [ListOf [ListOf X]] -> [ListOf X] ; (flatten/v2 lls) returns one giant list of all lists in lls
(define (flatten/v2 lls)
  (cond
    [(empty? lls) '()]
    [(empty? (rest lls)) (first lls)]
    [else (flatten/v2 (cons (join-together (first lls) (second lls))
                            (rest (rest lls))))]))

(check-expect (flatten/v2 '((a a a a) (b b) (c c c c c c) () (d)
                                      (e e e)))
              (list 'a 'a 'a 'a 'b 'b 'c 'c 'c 'c 'c 'c 'd 'e 'e 'e))


#|
2b

(flatten/v1 '((a b c) (d e) (f g h i)))
== (join-together '(a b c) (flatten/v1 '((d e) (f g h i)))) == (join-together '(a b c) (join-together '(d e) (flatten/v1
                                                  '(f g h i)))) == (join-together '(a b c) (join-together '(d e)
     (join-together '(f g h i) (flatten/v1 '())))) == (join-together '(a b c)
     (join-together '(d e)
       (join-together '(f g h i) '())))
== (join-together '(a b c) (join-together '(d e) '(f g h i))) == (join-together '(a b c)  '(d e f g h i)) == '(a b c d e f g h i)

|#

#|
2c

(flatten/v2 '((a b c) (d e) (f g h i)))
== (flatten/v2 (cons (join-together '(a b c) '(d e)) '((f g h i)))) == (flatten/v2 (cons '(a b c d e) (join-together '(f g h i)))) == (flatten/v2 (cons '(a b c d e f g h i) == (flatten/v2 (cons '(a b c d e f g h i '())

|#

;;;;;;;;;;;;
;Problem 3;;
;;;;;;;;;;;;

;32bit


(define (ignore x) #true)
;(make-list 5 (make-list 3 'a))
;(ignore (flatten/v1 (make-list 5 (make-list 3 'a))))

#|
   n    |  flatten/v1  | flatten/v2
====================================
   20               0             0
   50               0             0
  100              15            31
  200              62            47
  300             156           141
   :
 1000            1750          1672
 1100            2234          2110
 1200            2672          2359
 1300            3250          3078
 1400            3906          3516
 1500            4078          3875
 1600            5062          4656
 1700            5547          5062
 1800            6313          6015
|#


;3b
(define flatten flatten/v2)


;3c
#|
The flatten/v1 takes longer because each time it needs to add items to the list it calculates a new soltion and this happens many times to slow down how long it takes. The flatten/v2 has a solution and adds the lists to the solution rather than creating a new one at each step
|#


;;;;;;;;;;;;
;Problem 4;;
;;;;;;;;;;;;

; list-head : Nat [ListOf X] -> X
; (list-head n ls) returns the first n elements of ls
(define (list-head n ls)
  (cond
    [(zero? n) '()]
    [(<(length ls) n) (error 'list-head
                             (format "~s is too large for ~s"
                                     n ls))]
    [else (cons (first ls) (list-head (sub1 n) (rest ls)))]))

(check-expect (list-head 5 '(a b c d e f g h i))
              (list 'a 'b 'c 'd 'e))
(check-expect (list-head 5 '(a b c d e))
              (list 'a 'b 'c 'd 'e))
(check-error (list-head 5 '(a b c))
             "list-head: 5 is too large for (a b c)")


; list-tail : Nat [ListOf X] -> [ListOf X] ; (list-tail n ls) returns everything but the first n elements ; of ls
(define (list-tail n ls)
  (local
    [; tail-helper : Nat [ListOf X] -> [Maybe [ListOf X]]
     ; (tail-helper n-help ls-help) returns an error message if
     ; ls-help is empty, otherwise it may crete a list
     (define (tail-helper n-help ls-help)
       (cond
         [(zero? n-help) ls-help]
         [(empty? ls-help) (error 'list-tail
                                  (format "~s is too large for ~s"
                                          n ls))]
         [else
          (tail-helper (sub1 n-help) (rest ls-help))]))]
    (tail-helper n ls)))



(check-expect (list-tail 5 '(a b c d e f g h i))
              (list 'f 'g 'h 'i))
(check-error (list-tail 5 '(a b c))
             "list-tail: 5 is too large for (a b c)")



;;;;;;;;;;;;
;Problem 5;;
;;;;;;;;;;;;

; pop-up : Nat [ListOf X] -> [ListOf [ListOf X]] ; (pop-up n ls) returns the results of grouping runs of length n in ls ; into sublists
(define (pop-up n ls)
  (cond
    [(empty? ls) '()]
    [else (cons (list-head n ls)
                (pop-up n (list-tail n ls)))]))


(check-expect (pop-up 2 '(a b c d e f g h))
              (list (list 'a 'b) (list 'c 'd) (list 'e 'f)
                    (list 'g 'h)))

(check-expect (pop-up 3 '(a b c d e f))
              (list (list 'a 'b 'c) (list 'd 'e 'f)))

(check-expect (pop-up 5 (make-list 20 0))
              (list (list 0 0 0 0 0) (list 0 0 0 0 0)
                    (list 0 0 0 0 0) (list 0 0 0 0 0)))


;;;;;;;;;;;;
;Problem 6;;
;;;;;;;;;;;;


; overwrite/slow : [ListOf X] Nat X -> [ListOf X] ; (overwrite/slow ls i x) returns the list that results from ; replacing the element at index i in ls with x.


(define (overwrite/slow ls i x)
  (join-together (list-head i ls)
                 (join-together (list x) (list-tail (add1 i) ls))))

(check-expect (overwrite/slow '(a) 0 'b) '(b)) (check-expect (overwrite/slow '(a b c) 0 '_) '(_ b c)) (check-expect (overwrite/slow '(a b c) 1 '_) '(a _ c)) (check-expect (overwrite/slow '(a b c) 2 '_) '(a b _)) (check-expect (overwrite/slow '(x x x _ x x x x) 3 'x) (make-list 8 'x))


;6a
; overwrite : [ListOf X] Nat X -> [ListOf X] ; (overwrite ls i x) returns the list that results from ; replacing the element at index i in ls with x.
(define (overwrite ls i x)

  (local
    [; over-help : [ListOf X] Nat X -> [ListOf X]
     ; (over-help ls-help i-help x-help) returns the list needed for
     ; overwrite
     (define (over-help ls-help i-help x-help)
       (cond
         [(empty? ls-help)(error 'overwrite
                                 (format "~s is out of bounds for ~s"
                                         i ls))]
         [(zero? i-help) (cons x (list-tail (add1 i-help)
                                            ls-help))]
         [else (cons (first ls-help) (over-help
                                      (rest ls-help)
                                      (sub1 i-help) x-help))]))]
    (over-help ls i x)))
#|
(local [(define n 10000)
        (define ls (make-list n 0))]
  (list
   (ignore (time (overwrite ls (- n 1) 1)))
   (ignore (time (overwrite/slow ls (- n 1) 1)))))
|#
(check-expect (overwrite '(a) 0 'b) '(b)) (check-expect (overwrite '(a b c) 0 '_) '(_ b c)) (check-expect (overwrite '(a b c) 1 '_) '(a _ c)) (check-expect (overwrite '(a b c) 2 '_) '(a b _)) (check-expect (overwrite '(x x x _ x x x x) 3 'x) (make-list 8 'x))




#|
   n    |  overwrite/slow  | overwrite
====================================
 1000            15              0
 5000            16              0
10000            46             16
20000            78             31
|#






;;;;;;;;;;;;
;Problem 7;;
;;;;;;;;;;;;

;;;;;;;;;;;;;
;; The Model
;;;;;;;;;;;;;

;7a

#|
A TileValue is a
- BLANK
- 2
- 4
- 8
- 16
- 32
- 64
- 128
- 256
- 512
- 1024
- 2048
|#

;i
(define BLANK '_)

;ii
; blank? : X -> Bool
; (blank? x) return #true if x is BLANK
(define (blank? x)
  (equal? x BLANK))

(check-expect (blank? BLANK) #true)
(check-expect (blank? 2048) #false)
(check-expect (blank? blank?) #false)


;7b

#|
A Board is a
- '()
- (cons [ListOf TileValue] Board)
|#

(define b1 (list (list 64 32)
                 (list 16 16)))

(define b2 (list (list 2 2 2 2)
                 (list 4 '_ 4 '_)
                 (list '_ 8 8 8)
                 (list 16 '_ '_ 16)))

(define b3 (list (list 16 64 8 256 4)
                 (list 1024 1024 1024 32 128)
                 (list 64 32 128 '_ '_)
                 (list 4 4 32 '_ '_)
                 (list 2 '_ '_ 512 '_)))

(define b4 (list (list 64 32)
                 (list 16 2048)))

;7c
; board-full? : Board -> Bool
; (board-full? board) return #true iff the board is full
(define (board-full? board)
  (empty?(filter blank? (flatten/v2 board))))

(check-expect (board-full? b1) #true)
(check-expect (board-full? b2) #false)
(check-expect (board-full? b3) #false)



;7d
; add-new-tile : Board -> Board
; (add-new-tile board)
(define (add-new-tile board)
  (local
    [(define rows (length board))
     (define flat-board (flatten/v2 board))
     (define new-tile (list-ref '(2 2 2 2 4) (random 5)))
     ; get-blank-index-list : Nat [ListOf X] -> [ListOf X]
     (define (get-blank-index-list x flat)
       (local
         [; get-blank-index : Nat -> Nat
          (define get-index (random x))]
         (if (blank? (list-ref flat get-index))
             get-index
             (get-blank-index-list x flat))))]
    (if (board-full? board)
        board
        (pop-up rows
                (overwrite flat-board
                           (get-blank-index-list
                            (length flat-board)
                            flat-board)
                           new-tile)))))

(check-expect (add-new-tile '((4))) (list (list 4))) (check-expect (add-new-tile b1) (list (list 64 32) (list 16 16)))

;7e

;i
; iterate : [Any->Any] Nat Any -> Any
; (iterate f n x) returns the result
; of applying proc n times starting
; with x
(define (iterate f n x)
  (cond
    [(zero? n) x]
    [else (iterate f (sub1 n) (f x))]))

;ii
;Yes this function is tail recursive because the recursion is the ;last thing done by the function before returning and thus produces ;the function's value


;iii
; make-board : Nat Nat -> Board
; (make-board n m) returns an nxn board with m non-blank tiles
(define (make-board n m)
  (cond
    [(zero? n) '()]
    [else (iterate add-new-tile m
                   (pop-up n (make-list (* n n) '_)))]))


;7f
; board-square? : Board -> Bool
; (board-square? board) returns #true iff the board is nxn for some ;integer n

(define (board-square? board)
  (local [; help : Nat Bool -> [Any -> Any]
          (define (help e r)
            (and (equal? e (length board)) r))]
    (if (empty? board)
        #false
        (foldr help #true (map length board)))))


;7g
; game-won? Board -> Bool
; (game-won? board) returns #true iff there is a 2048 on the board
(define (game-won? board)
  (if (member? 2048 (flatten/v2 board))
      #true
      #false))
(check-expect (game-won? b1)
              #false)
(check-expect (game-won? b2)
              #false)
(check-expect (game-won? b3)
              #false)
(check-expect (game-won? b4)
              #true)


;;;;;;;;;;;;
;Problem 8;;
;;;;;;;;;;;;

;;;;;;;;;;;;;
;; The View
;;;;;;;;;;;;;

;8a



(define TILE-SIZE 100)
(define FONT-SIZE 30)
(define GRID-SPACING 10)
(define GRID-COLOR (make-color 186 172 160))




;8b
; title->image : TileValue Int Color Color -> Img ; (title->image tile fontsize fore back) returns an image of a title ; with the given properties
(define (tile->image tile fontsize fore back)
  (overlay
   (if (blank? tile) empty-image
   (text (number->string tile) fontsize fore))
   (square (- TILE-SIZE GRID-SPACING) "solid" back)
   (square TILE-SIZE "solid" GRID-COLOR)))

(check-satisfied (tile->image 64 FONT-SIZE
    (make-color 255 255 255) (make-color 246 94 59))
                 image?)

(check-satisfied (tile->image '_ FONT-SIZE
    (make-color 255 255 255) (make-color 246 94 59))
                 image?)


;8c
; val->image : TileValue -> Img
; (val->image tile) returns an image using game sizes and colors ; given tile

(define (val->image tile)
  (cond
    [(equal? tile BLANK)
     (tile->image tile FONT-SIZE (make-color 105 105 105)
                  (make-color 204 192 179))]
    [(equal? tile 2)
     (tile->image tile FONT-SIZE (make-color 105 105 105)
                  (make-color 238 228 218))]
    [(equal? tile 4)
     (tile->image tile FONT-SIZE (make-color 105 105 105)
                  (make-color 237 224 200))]
    [(equal? tile 8)
     (tile->image tile FONT-SIZE (make-color 225 225 225)
                  (make-color 242 177 121))]
    [(equal? tile 16)
     (tile->image tile FONT-SIZE (make-color 225 225 225)
                  (make-color 245 149 99))]
    [(equal? tile 32)
     (tile->image tile FONT-SIZE (make-color 225 225 225)
                  (make-color 246 124 95))]
    [(equal? tile 64)
     (tile->image tile FONT-SIZE (make-color 225 225 225)
                  (make-color 246 94 59))]
    [(equal? tile 128)
     (tile->image tile FONT-SIZE (make-color 225 225 225)
                  (make-color 237 207 114))]
    [(equal? tile 256)
     (tile->image tile FONT-SIZE (make-color 225 225 225)
                  (make-color 237 204 97))]
    [(equal? tile 512)
     (tile->image tile FONT-SIZE (make-color 225 225 225)
                  (make-color 237 200 80))]
    [(equal? tile 1024)
     (tile->image tile FONT-SIZE (make-color 225 225 225)
                  (make-color 237 197 63))]
    [(equal? tile 2048)
     (tile->image tile FONT-SIZE (make-color 225 225 225)
                  (make-color 237 194 46))]
    [else (error 'val->image
                 (format "unknown tile value ~s"
                         tile))]))

(check-error (val->image 255) "val->image: unknown tile value 255")


;8d
; board->image : Board -> Img
; (board->image board) returns the image of board
(define (board->image board)
  (foldr above empty-image
         (map board-helper
              (pop-up (length (first board))
                      (map val->image (flatten/v2 board))))))

; board-helper : Board -> Func
; (board-helper l) returns a function that helps map the problem
(define (board-helper l)
  (foldr beside empty-image l))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;A11;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;;;;;Problem 1;;;;;;
;;;;;;;;;;;;;;;;;;;;

; invert/v1 : [ListOf X] -> [ListOf X]
; (invert/v1 ls) returns the result of reversing the top-level ; elements of ls
(define (invert/v1 ls)
  (local [; help : [ListOf X] [ListOf X] -> [ListOf X]
          (define (help ls acc)
            (cond
             [(empty? ls) acc]
             [else (help (rest ls) (cons (first ls) acc))]))]
  (help ls '())))
(check-expect (invert/v1 '(a b c d e)) (list 'e 'd 'c 'b 'a))

; invert/v2 : [ListOf X] -> [ListOf X]
; (invert/v2 ls) returns the result of reversing the top-level ; elements of ls
(define (invert/v2 ls)
  (cond
   [(empty? ls) '()]
   [else (append (invert/v2 (rest ls)) (list (first ls)))]))


#|
1a

(invert/v1 '(a b c d e))
== (help '(b c d e) (cons '(a) '()))
== (help '(c d e) (cons '(b) '(a)))
== (help '(d e) (cons '(c) '(b a)))
== (help '(e) (cons 'd) '(c b a)))
== (help '() (cons 'e) '(d c b a)))
== (cons '() '(e d c b a)))
== '(e d c b a)



1b
(invert/v2 '(a b c d e))
== (append (invert/v2 '(b c d e)) (list ('(a)))) == (append (append (invert/v2 '(c d e)) (list ('(b)))) (list ('(a)))) == (append (append (append (invert/v2 '(d e)) (list ('(c)))) (list
                                               ('(b)))) (list ('(a)))) == (append (append (append (append (invert/v2 '(e)) (list ('(d)))) (list ('(c)))) (list ('(b)))) (list ('(a)))) == (append (append (append (append (append (invert/v2 '()) (list ('(e)))) (list ('(d)))) (list ('(c)))) (list ('(b)))) (list ('(a)))) == (append (append (append (append (append '() (list ('(e)))) (list ('(d)))) (list ('(c)))) (list ('(b)))) (list ('(a)))) == (append (append (append (append (list ('(e))) (list ('(d)))) (list ('(c)))) (list ('(b)))) (list ('(a)))) == (append (append (append (list ('(d e))) (list ('(c)))) (list ('(b)))) (list ('(a)))) == (append (append (list ('(c d e))) (list ('(b)))) (list ('(a)))) == (append (list ('(b c d e))) (list ('(a)))) == (list ('(a b c d e)))



1c

This function is tail recursive because it has to go through the entire function many times and then add the item you want at the end one at a time


1d

This function is not tail recursive because has to keep adding appends to the front which is not tail recursive



1e

(local [(define n 1000000)
        (define ls (make-list n 0))]
  (list
   (ignore (time (invert/v1 ls)))
   ;(ignore (time (invert/v2 ls)))
   (ignore (time (reverse ls)))))




     n       invert/v1     invert/v2     reverse
-------------------------------------------------
    100         0               0            0
   1000         0              63            0
  10000         0             765            0
 100000        78          123484            0
1000000       750        15 minutes +       78
|#

(define invert invert/v1)

;;;;;;;;;;;;;;;;;;;;
;;;;;Problem 2;;;;;;
;;;;;;;;;;;;;;;;;;;;

; slide-row-right : [ListOf TileValue] -> [ListOf TileValue] ; (slide-row-right ls) returns the result of sliding the row to the ;left combining similar TileValues that are not blank
#|
(define (slide-row-right ls)
  (local [(define (slide-help ls acc)
          (cond
            [(empty? ls)  (invert acc)]
            [else (slide-help (rest ls) (if
                                         (blank?(first ls))
                                                  (cons (first ls) acc)
                                           (append acc (list
                                                (first ls)))))]))]
    (slide-help ls '())))
|#
(define (slide-row-left ls)
  (local [(define (slide-help row last-number blanks)
            (cond
              [(empty? row) (if (false? last-number)
                                blanks
                                (cons last-number blanks))]
              [(false? last-number)
               (if (blank? (first row))
                   (slide-help (rest row) #false (cons '_ blanks))
                   (if (equal? (first row) last-number)
                       (cons (* (first row) 2)
                             (slide-help (rest row)
                                      #false (cons '_ blanks)))
                       (slide-help (rest row) (first row) blanks)))]

              [else (if (blank? (first row))
                        (slide-help (rest row) last-number
                                    (cons '_ blanks))
                        (if (equal? (first row) last-number)
                            (cons (* (first row) 2)(slide-help
                                                    (rest row)
                                                    #false
                                                    (cons '_ blanks)))
                            (cons last-number (slide-help
                                               (rest row)
                                               (first row) blanks))))]
            ))]
    (slide-help ls #false '())))

(check-expect (slide-row-left '()) '())
(check-expect (slide-row-left '(_)) (list '_)) (check-expect (slide-row-left '(2)) (list 2)) (check-expect (slide-row-left '(_ _)) (list '_ '_)) (check-expect (slide-row-left '(2 _)) (list 2 '_)) (check-expect (slide-row-left '(_ 2)) (list 2 '_)) (check-expect (slide-row-left '(2 2)) (list 4 '_)) (check-expect (slide-row-left '(2 4)) (list 2 4)) (check-expect (slide-row-left '(4 4)) (list 8 '_)) (check-expect (slide-row-left '(_ _ _)) (list '_ '_ '_)) (check-expect (slide-row-left '(2 _ _)) (list 2 '_ '_)) (check-expect (slide-row-left '(_ 2 _)) (list 2 '_ '_)) (check-expect (slide-row-left '(_ _ 2)) (list 2 '_ '_)) (check-expect (slide-row-left '(2 2 _)) (list 4 '_ '_)) (check-expect (slide-row-left '(2 _ 2)) (list 4 '_ '_)) (check-expect (slide-row-left '(_ 2 2)) (list 4 '_ '_)) (check-expect (slide-row-left '(2 4 _)) (list 2 4 '_)) (check-expect (slide-row-left '(2 _ 4)) (list 2 4 '_)) (check-expect (slide-row-left '(_ 2 4)) (list 2 4 '_)) (check-expect (slide-row-left '(2 2 2)) (list 4 2 '_)) (check-expect (slide-row-left '(4 2 2)) (list 4 4 '_)) (check-expect (slide-row-left '(2 4 2)) (list 2 4 2)) (check-expect (slide-row-left '(2 2 4)) (list 4 4 '_)) (check-expect (slide-row-left '(2 4 8)) (list 2 4 8))



;;;;;;;;;;;;;;;;;;;;
;;;;;Problem 3;;;;;;
;;;;;;;;;;;;;;;;;;;;

; slide-row-right : [ListOf TileValue] -> [ListOf TileValue] ; (slide-row-right ls) returns the result of sliding the row to the ;left combining similar TileValues that are not blank
(define (slide-row-right ls)
    (reverse (slide-row-left (reverse ls))))


(check-expect (slide-row-right '()) '()) (check-expect (slide-row-right '(_)) (list '_)) (check-expect (slide-row-right '(2)) (list 2)) (check-expect (slide-row-right '(_ _)) (list '_ '_)) (check-expect (slide-row-right '(2 _)) (list '_ 2)) (check-expect (slide-row-right '(_ 2)) (list '_ 2)) (check-expect (slide-row-right '(2 2)) (list '_ 4)) (check-expect (slide-row-right '(2 4)) (list 2 4)) (check-expect (slide-row-right '(4 4)) (list '_ 8)) (check-expect (slide-row-right '(_ _ _)) (list '_ '_ '_)) (check-expect (slide-row-right '(2 _ _)) (list '_ '_ 2)) (check-expect (slide-row-right '(_ 2 _)) (list '_ '_ 2)) (check-expect (slide-row-right '(_ _ 2)) (list '_ '_ 2)) (check-expect (slide-row-right '(2 2 _)) (list '_ '_ 4)) (check-expect (slide-row-right '(2 _ 2)) (list '_ '_ 4)) (check-expect (slide-row-right '(_ 2 2)) (list '_ '_ 4)) (check-expect (slide-row-right '(2 4 _)) (list '_ 2 4)) (check-expect (slide-row-right '(2 _ 4)) (list '_ 2 4)) (check-expect (slide-row-right '(_ 2 4)) (list '_ 2 4)) (check-expect (slide-row-right '(2 2 2)) (list '_ 2 4)) (check-expect (slide-row-right '(4 2 2)) (list '_ 4 4)) (check-expect (slide-row-right '(2 4 2)) (list 2 4 2)) (check-expect (slide-row-right '(2 2 4)) (list '_ 4 4)) (check-expect (slide-row-right '(2 4 8)) (list 2 4 8))


;;;;;;;;;;;;;;;;;;;;
;;;;;Problem 4;;;;;;
;;;;;;;;;;;;;;;;;;;;
; slide-left : Board -> Board
; (slide-left board) returns the board slided to the left
(define (slide-left board)
  (map slide-row-left board))

; slide-right : Board -> Board
; (slide-left board) returns the board slided to the right
(define (slide-right board)
  (map slide-row-right board))


;;;;;;;;;;;;;;;;;;;;
;;;;;Problem 5;;;;;;
;;;;;;;;;;;;;;;;;;;;

; transpose : [ListOf X] -> [ListOf X]
; (transpose lls) returns the result of reflecting lls along the main ; diagonal
(define (transpose lls)
  (cond
    [(empty? (first lls)) '()]
    [else (cons (map first lls) (transpose (map rest lls)))]))





(check-expect (transpose '(())) '())
(check-expect (transpose '((5)))
(list (list 5)))
(check-expect (transpose '((a b c) (d e f) (g h i))) (list (list 'a 'd 'g) (list 'b 'e 'h) (list 'c 'f 'i))) (check-expect (transpose '((1 2) (3 4) (5 6))) (list (list 1 3 5) (list 2 4 6))) (check-expect (equal? (transpose (transpose b2)) b2)
#true)
;;;;;;;;;;;;;;;;;;;;
;;;;;Problem 6;;;;;;
;;;;;;;;;;;;;;;;;;;;
; slide-up : Board -> Board
; (slide-up board) returns the board slided to the top
(define (slide-up board)
  (transpose (map slide-row-left (transpose board)))) ; slide-down : Board -> Board ; (slide-down board) returns the board slided to the bottom
(define (slide-down board)
  (transpose (map slide-row-right (transpose board))))

;;;;;;;;;;;;;;;;;;;;
;;;;;Problem 7;;;;;;
;;;;;;;;;;;;;;;;;;;;

;7a
; game-lost? : Board -> Bool
; (game-lost? board) returns #true iff it represents a losing ; configuration
(define (game-lost? board)
  (cond
    [(empty? board) #true]
    [(and
      (equal? (slide-left board) board)
      (equal? (slide-right board) board)
      (equal? (slide-up board) board)
      (equal? (slide-down board) board))
     #true]
    [else #false]))



(check-expect (game-lost? '((2))) #true) (check-expect (game-lost? '((2048))) #true) (check-expect (game-lost? '((2 4) (8 4))) #false) (check-expect (game-lost? '((2 4 8) (16 8 4) (8 4 2))) #true) (check-expect (game-lost? b1) #false)


;7b
; game-over? : Board -> Bool
; (game-over? board) returns #true if board represents a winning or ; losing configuration
(define (game-over? board)
  (or(game-won? board)(game-lost? board)))


(check-expect (game-over? '((1024))) #true) (check-expect (game-over? '((2048 4) (8 4))) #true) (check-expect (game-over? '((2 4 8) (16 8 4) (8 4 2))) #true)


;;;;;;;;;;;;;;;;;;;;
;;;;;Problem 8;;;;;;
;;;;;;;;;;;;;;;;;;;;

; key-handler : Board String -> Board
; (key-handler world key) returns a board based on the key and ; the previous board given
(define (key-handler board key)
  (cond
    [(equal? key "right")
     (if (not (equal? board (slide-right board)))
         (add-new-tile(slide-right board))
         board)]
    [(equal? key "left")
     (if (not (equal? board (slide-left board)))
     (add-new-tile(slide-left board))
     board)]
    [(equal? key "up")
     (if (not (equal? board (slide-up board)))
     (add-new-tile(slide-up board))
     board)]
    [(equal? key "down")
     (if (not (equal? board (slide-down board)))
     (add-new-tile(slide-down board))
     board)]
    [else board]))

(define b9 '((2 4) (_ _)))

(check-expect (key-handler b9 "up")
(list (list 2 4) (list '_ '_)))

(check-expect (key-handler b9 "left")
(list (list 2 4) (list '_ '_)))

(check-expect (key-handler b9 "right")
(list (list 2 4) (list '_ '_)))

(check-member-of (key-handler b9 "down") (list (list 2 '_) (list 2 4)) (list (list '_ 2) (list 2 4)) (list (list 4 '_) (list 2 4)) (list (list '_ 4) (list 2 4)))



;;;;;;;;;;;;;;;;;;;;
;;;;;Problem 9;;;;;;
;;;;;;;;;;;;;;;;;;;;

; play : PosInt -> BigBang
; (play n) returns a board using big bang on a preious board

(define (play n)
  (big-bang (make-board n (- n 2))
            [to-draw board->image]
            [on-key key-handler]
            [name "2048"]))

;;;;USE (play 4) below in DrRacket to start the game;;;;

