;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define handin "a14")
(define collaboration-statement "I worked alone")
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;
;;;Problem 1;;;
;;;;;;;;;;;;;;;

; 1a
; image-map [X -> Y] Img -> Img
; (image-map func image) returns image after using func on every pixel
; in image
(define (image-map func image)
  (color-list->bitmap
   (map func (image->color-list image))
   (image-width image)
   (image-height image)))

;(define prince (bitmap "prince.png"))
#|(list prince (image-map (lambda (color)
                            (if (equal? color (make-color 0 0 0))
                                (make-color 160 32 240)
                                color))
                          prince))|#
; 1b
; image-negative : Img -> Img
; (image-negative image) returns image with each color value subtracted
; by 255
(define (image-negative image)
  (image-map (lambda (color)
               (make-color (- 255 (color-red color))
                           (- 255 (color-green color))
                           (- 255 (color-blue color))))
             image))


;(define marilyn (bitmap "marilyn.png"))
;(list marilyn (image-negative marilyn))

;(define baboon (bitmap "baboon.png"))
;(list baboon (image-negative baboon))
;(define tiny-dino (bitmap "tiny-dino.png"))
;(define profile (bitmap "profile.png"))
;(define thinker (bitmap "thinker.png"))

; 1c
; image-spin : Img -> Img
; (image-spin img) returns a new image that spins img through
; the color values
(define (image-spin img)
  (image-map (lambda (color)
               (make-color (color-blue color)
                           (color-red color)
                           (color-green color)))
             img))
; (list marilyn (image-spin marilyn))

;;;;;;;;;;;;;;;
;;;Problem 2;;;
;;;;;;;;;;;;;;;

; warhol : Img PosInt -> Img
; (warhol image n) returns an image with four smaller images in a 2x2
; grid with the upper/lower left and lower right are a reduced version
; of the image while the upper right is a fracal of n-1 depth
(define (warhol image n)
  (cond
    [(zero? n) image]
    [else (local [(define sml-img (scale .5 image))]
            (above
             (beside sml-img (warhol sml-img (sub1 n)))
             (beside sml-img sml-img)))]))

;(define soup (bitmap "soup.png"))


;;;;;;;;;;;;;;;
;;;Problem 3;;;
;;;;;;;;;;;;;;;
; interleave : [ListOf X] [ListOf Y] -> [ListOf X Y]
; (interleave listone listtwo) returns a list consisting of alternating
; items of listone and listtwo
(define (interleave listone listtwo)
  (cond
    [(empty? listtwo) listone]
    [(empty? listone) listtwo]
    [else (cons (first listone)
                (cons (first listtwo)
                      (interleave (rest listone)
                                  (rest listtwo))))]))

(check-expect (interleave '() '())
              '())
(check-expect (interleave '(X X X X X X) '(O O O O O))
              (list 'X 'O 'X 'O 'X 'O 'X 'O 'X 'O 'X))
(check-expect (interleave '(1 2 3) '(a b c d e f))
              (list 1 'a 2 'b 3 'c 'd 'e 'f))
(check-expect (interleave '(1 2 3 4 5 6) '(a b))
              (list 1 'a 2 'b 3 4 5 6))


;;;;;;;;;;;;;;;
;;;Problem 4;;;
;;;;;;;;;;;;;;;
; join-together : [ListOf X] [ListOf X] -> [ListOf X]
; (join-together ls1 ls2) returns a list containing the top level
; elements of ls1 and ls2
(define (join-together ls1 ls2)
  (cond
    [(and (empty? ls1) (empty? ls2)) '()]
    [(empty? ls1) ls2]
    [else  (cons (first ls1)
                 (join-together (rest ls1) ls2))]))

(check-expect (join-together '(a b c) '(d e f g h))
              (list 'a 'b 'c 'd 'e 'f 'g 'h))

(check-expect (join-together '() '(7 2 0 1 8 3 4))
              (list 7 2 0 1 8 3 4))

; list-head : Nat [ListOf X] -> X
; (list-head n ls) returns the first n elements of ls
(define (list-head n ls)
  (cond
    [(zero? n) '()]
    [(<(length ls) n) (error 'list-head
                             (format "~s is too large for ~s"
                                     n ls))]
    [else (cons (first ls) (list-head (sub1 n) (rest ls)))]))

; list-tail : Nat [ListOf X] -> [ListOf X]
; (list-tail n ls) returns everything but the first n elements
; of ls
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

; pop-up : Nat [ListOf X] -> [ListOf [ListOf X]]
; (pop-up n ls) returns the results of grouping runs of length n in ls
; into sublists
(define (pop-up n ls)
  (cond
    [(empty? ls) '()]
    [else (cons (list-head n ls)
                (pop-up n (list-tail n ls)))]))

; flatten/v1 : [ListOf [ListOf X]] -> [ListOf X]
; (flatten/v1 lls) returns one giant list of all lists in lls
(define (flatten/v1 lls)
  (cond
    [(empty? lls) '()]
    [else (join-together (first lls)
                         (flatten/v1 (rest lls)))]))
  #|(foldr (lambda (acc r) (if (empty? acc)
                           '()
                           (append acc)))'() lls))|#
(check-expect (flatten/v1 '((a a a a) (b b) (c c c c c c) () (d)
                                      (e e e)))
              (list 'a 'a 'a 'a 'b 'b 'c 'c 'c 'c 'c 'c 'd 'e 'e 'e))

(define flatten flatten/v1)

; image-interleave : Image Image -> Image
; (image-interleave img1 img2) returns 
(define (image-interleave img1 img2)
  (local [(define width (image-width img1))
          (define height (+ (image-height img1)(image-height img2)))
          (define color-list-img1 (pop-up width
                                          (image->color-list img1)))
          (define color-list-img2 (pop-up width
                                          (image->color-list img2)))
          (define (flat ls1 ls2)
            (cond
              [(empty? ls1) ls2]
              [(empty? ls2) ls1]
              [else (cons (first ls1) (flat ls2 (rest ls1)))]))]
    (color-list->bitmap
     (flatten(flat color-list-img1 color-list-img2))
     width
     height)))
    
;;;;;;;;;;;;;;;
;;;Problem 5;;;
;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;;;Problem 10;;;
;;;;;;;;;;;;;;;;

; 10a 
; get-index Nat Nat Nat -> Nat
; (get-index x y width) returns the index of the pixel in the flat list 
; of colors
(define (get-index x y width)
  (+ x (* width y)))

(check-expect (get-index 0 0 1000)
              0)
(check-expect (get-index 0 20 100)
              2000)
(check-expect (get-index 20 0 100)
              20)
(check-expect (get-index 23 55 30)
              1673)
(check-expect (get-index 23 55 50)
              2773)

; 10b
; out-of-bounds? : Num Num Nat Nat -> Bool
; (out-of-bounds? x y width height) returns #true iff the coordinate is
; outside of the preimeter
(define (out-of-bounds? x y width height)
  (or (or (<= x 0) (<= y 0))
      (or (>= x width) (>= y height))))

(check-expect (out-of-bounds? -1 5 10 20)
              #true)
(check-expect (out-of-bounds? 1 -5 10 20)
              #true)
(check-expect (out-of-bounds? 10 4 10 20)
              #true)
(check-expect (out-of-bounds? 3 30 10 30)
              #true)
(check-expect (out-of-bounds? 3 35 10 30)
              #true)
(check-expect (out-of-bounds? 3 35 10 36)
              #false)

; 10c


; overwrite : [ListOf X] Nat X -> [ListOf X]
; (overwrite ls i x) returns the list that results from
; replacing the element at index i in ls with x.
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

; flood-fill : Img Nat Nat Symbol -> Img
; (flood-fill img x y color) returns the result of filling in the area
; around the pixel x y in img with color
(define (flood-fill img x y color)
  (local [(define color-list (image->color-list img))
          (define width (image-width img))
          (define height (image-height img))
          (define old-color (list-ref color-list (get-index x y width)))
          (define (fill x2 y2 ls)
              (cond
                [(out-of-bounds? x2 y2 width height) ls]
                [else (local [(define current-color
                                (list-ref ls
                                             (get-index x2 y2 width)))]
                        (cond
                          [(and(equal? (color-red current-color)0)
                               (equal? (color-green current-color)0)
                               (equal? (color-blue current-color)0)) ls]
                          [(equal? current-color color) ls]
                          [else (fill x2 (add1 y2)
                                      (fill x2 (sub1 y2)
                                            (fill (add1 x2) y2
                                                  (fill (sub1 x2) y2
   (overwrite ls (get-index x2 y2 width) color)))))]))]))]
    (color-list->bitmap (fill x y color-list)  width height)))

                  
          
#|
(define d1 (flood-fill tiny-dino 133 16 (make-color 135 206 235)))
(define d2 (flood-fill d1 25 119 (make-color 34 139 34)))
(define d3 (flood-fill d2 123 77 (make-color 210 180 140)))
(define d4 (flood-fill d3 46 69 (make-color 210 78 152)))
(define d5 (flood-fill d4 64 54 (make-color 67 119 201)))
(define d6 (flood-fill d5 37 42 (make-color 255 20 30)))
|#