;; Problem Set 10
; Alex Agudelo & Jesse Bates

(require 2htdp/image)

;****************************************************************
; Problem 1

;;; An Atom is one of:
;;; - Number
;;; - Symbol
;;; - String
;;;
;;; An SExp is one of:
;;; - Atom
;;; - [List-of SExp]

; Purpose:
; consume an s-expression and produce a string with
; the textual represenation of that string
; SExp -> String
(check-expect (sexp->string '(a (37 "foo") c)) "( a ( 37 \"foo\" ) c )")
(define (sexp->string sexp)
  (string-append "(" (sexp->stringBase sexp) " )"))


; Purpose:
; Convert the s-expression into a list of strings with correct format
; SExp -> String
(check-expect (sexp->stringBase '(a (37 "foo") c)) " a ( 37 \"foo\" ) c")
(define (sexp->stringBase sexp)
  (cond
    [(empty? sexp) ""]
    [(atom? (first sexp))
     (cond
       [(number? (first sexp)) (string-append " " (number->string (first sexp)) (sexp->stringBase (rest sexp)))]
       [(symbol? (first sexp)) (string-append " " (symbol->string (first sexp)) (sexp->stringBase (rest sexp)))]
       [(string? (first sexp)) (string-append " \"" (first sexp) "\"" (sexp->stringBase (rest sexp)))])]
    [(list? (first sexp)) (string-append " (" (sexp->stringBase (first sexp)) " )" (sexp->stringBase (rest sexp)))]))

; Purpose:
; Determine if a given expression is an atom or not
; SExp -> Boolean

(check-expect (atom? 5) true)
(check-expect (atom? 'foo) true)
(check-expect (atom? "hi") true)
(check-expect (atom? (make-posn 5 10)) false)

(define (atom? sexp)
  (or (number? sexp)
      (string? sexp)
      (symbol? sexp)))


;*****************************************************************************
; Problem 2

(define-struct lego (label color width))
; A Lego is a structure:
;    (make-lego Number Symbol Number)
; interpretation: (make-lego l c w) is the lego brick
; with label l, color c, and width w (in pixels).

#;(define (lego-tmpl a-lego)
    ... (lego-label a-lego) ...
    ... (lego-color a-lego) ...
    ... (lego-width a-lego) ...)
 
(define-struct bigger (lego left right))
; A LegoBldg (lego building) is one of:
; - Lego
; - (make-bigger Lego LegoBldg LegoBldg)
; interpretation: (make-bigger l lft rgt) makes a bigger
; lego building by putting a lego brick l on top of two lego
; buildings lft (left) and rgt (right).

#;(define (bigger-tmpl a-bigger)
    ... (bigger-lego a-bigger) ...
    ... (bigger-left a-bigger) ...
    ... (bigger-right a-bigger) ...)

(define lego1 (make-lego 1 'blue 2))
(define lego2 (make-lego 2 'green 4))
(define lego3 (make-lego 3 'red 4))
(define lego4 (make-lego 4 'blue 5))
(define lego5 (make-lego 5 'red 3))
(define Bldg1 (make-bigger lego3 lego2 lego1))
(define Bldg2 (make-bigger lego4 Bldg1 lego5))
(define Bldg4 (make-bigger lego4 lego3 Bldg1))

; Purpose:
; Counts the total number of lego bricks in the building
; LegoBldg -> Number
(check-expect (count-bricks Bldg2) 5)
(check-expect (count-bricks Bldg1) 3)

(define (count-bricks LegoB)
  (cond
    [(lego? LegoB) 1]
    [else 
     (+ 1 (count-bricks (bigger-left LegoB)) (count-bricks (bigger-right LegoB)))]))

;***********************************************************************************8
; Problem 3

; Purpose: 
; Determine how high a lego building is
; LegoBlg -> Number
(check-expect (how-high Bldg2) 30)
(check-expect (how-high Bldg1) 20)

(define (how-high LegoB)
  (cond
    [(lego? LegoB) 10]
    [(bigger? LegoB) 
     (+ 10 (cond
             [(> (how-high (bigger-left LegoB)) (how-high (bigger-right LegoB)))
              (how-high (bigger-left LegoB))]
             [else
              (how-high (bigger-right LegoB))]))]))

;**************************************************************************************8
; Problem 4

; Purpose: 
; takes a lego building and a color
; and determines whether the buliding contains a lego
; brick of the given color
; Legoblg Symbol -> Boolean
(check-expect (contains-colored-brick? Bldg2 'red) true)
(check-expect (contains-colored-brick? Bldg2 'purple) false)
(check-expect (contains-colored-brick? Bldg1 'blue) true)

(define (contains-colored-brick? LegoB aColor)
  (cond
    [(lego? LegoB)
     (symbol=? aColor (lego-color LegoB))]
    [(bigger? LegoB)
     (or (contains-colored-brick? (bigger-left LegoB) aColor)
         (contains-colored-brick? (bigger-right LegoB) aColor))]))

;***********************************************************************************
; Problem 5

; A MaybeLego is one of:
; - false
; - Lego

; Purpose:
; takes a lego building and a color and finds
; any lego with the given color in the building, or returns false if 
; there are no such legos
; LegoBld Symbol -> MaybeLego

(check-expect (find-colored-brick? Bldg2 'red) (make-lego 5 'red 3))
(check-expect (find-colored-brick? Bldg2 'purple) false)
(check-expect (find-colored-brick? Bldg1 'blue) (make-lego 1 'blue 2))
(check-expect (find-colored-brick? empty 'blue) false)
(check-expect (find-colored-brick? Bldg2 'blue) (make-lego 4 'blue 5))
(check-expect (find-colored-brick? Bldg4 'blue) (make-lego 4 'blue 5))

(define (find-colored-brick? LegoB aColor)
  (cond 
    [(empty? LegoB) false]
    [(lego? LegoB) (if (symbol=? aColor (lego-color LegoB)) 
                       LegoB
                       false)]
    [(bigger? LegoB)
     (if (symbol=? aColor (lego-color (bigger-lego LegoB)))
         (bigger-lego LegoB)
         (cond
           [(lego? (find-colored-brick? (bigger-right LegoB) aColor))
            (bigger-right LegoB)]
           [(lego? (find-colored-brick? (bigger-left LegoB) aColor))
            (bigger-left LegoB)]
           [else 
            (or (find-colored-brick? (bigger-left LegoB) aColor)
                (find-colored-brick? (bigger-right LegoB) aColor))]))])) 


;*****************************************************************************
; Problem 6

(define Bldg3 (make-bigger (make-lego 4 'purple 80)
             (make-bigger (make-lego 2 'blue 60)
                          (make-lego 1 'yellow 40)
                          (make-lego 3 'red 40))
             (make-bigger (make-lego 6 'orange 60)
                          (make-lego 5 'green 40)
                          (make-lego 7 'red 40))))
; Purpose:
; Takes a lego building and prduces an image of the building
; LegoBld -> Image

#;(define (lb->image LegoB)
  (cond
    [(empty? LegoB) (empty-scene 50 50)]
    [(lego? LegoB) (lego->image LegoB)]
    [(bigger?  LegoB)
     (overlay  (above/align "center" (beside/align "bottom" (lego->image (bigger-lego LegoB)) (lb->image (bigger-right LegoB)))
               (beside/align "bottom" (lego->image (bigger-lego LegoB)) (lb->image (bigger-left LegoB)))))]))
                                               
; Purpose:
; Takes a lego and produces an image of it
; Lego -> Image
(check-expect (lego->image lego1) (rectangle 2 10 'solid 'blue))
(check-expect (lego->image lego2) (rectangle 4 10 'solid 'green))

(define (lego->image a-lego)
  (rectangle (lego-width a-lego) 10 'solid (lego-color a-lego)))



