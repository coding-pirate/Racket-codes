;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname paint-by-numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Harsh Shah (20774723)
;; CS 135 Fall 2018
;; Assignment 06, Problem 2
;; ***************************************************
;;

;; A Grid is a (listof (listof (anyof 'B '-)))
;; requires: all sublists have the same length

(define fish-grid '((B B - - B B B B - - - B)
                    (- B B B - - - - B - - -)
                    (- B B B - - - - B B - -)
                    (- B - - B B B B B - - -)
                    (B B - - - - B - - - - -)))

;; Beginning of PART A.

;; Helper functions and constants for PART A.

(define first-column 0)

;; (cut-columns Grid) deletes the leftmost columns of the Grid
;; as per what is required in (column col Grid).

;; cut-columns: Grid -> Grid
(define (cut-first-column Grid)
  (cond [(empty? Grid) empty]
        [else (cons (rest (first Grid)) (cut-first-column (rest Grid)))]))

;; End of helper functions and constants for PART A.

;; (column col Grid) consumes a natural number col and a
;; Grid and produces the col'th column of the Grid.

;; column: Nat Grid -> (listof (anyof 'B '-))
;; Examples:
(check-expect (column 5 fish-grid) '(B - - B -))
(check-expect (column 0 fish-grid) '(B - - - B))
   
(define (column col Grid)
  (cond [(empty? Grid) empty]
        [(= col first-column) (cons (first (first Grid)) (column col (rest Grid)))]
        [(and (> col first-column)(< col (length (first Grid))))
         (column (sub1 col) (cut-first-column Grid))]
        [else empty]))

;;Tests:
(check-expect (column 2 fish-grid) '(- B B - - ))
(check-expect (column 32 fish-grid) empty)

;; End of PART A.

;; Beginning of PART B.

;; Helper functions for PART B:
;; (cells->tallies/acc sub-grid acc) uses an accumulator to produce a
;; list of tallies of adjacent black cells in a sub-grid.

;; cells->tallies/acc: (listof (anyof ’B ’-)) Nat -> (listof Nat)
(define (cells->tallies/acc sub-grid acc)
  (cond [(empty? sub-grid) (cond [(zero? acc) empty]
                                 [else (cons acc empty)])]
        [(zero? acc)
         (cond [(symbol=? 'B (first sub-grid))
                (cells->tallies/acc (rest sub-grid) (add1 acc))]
               [else (cells->tallies/acc (rest sub-grid) acc)])]
        [else (cond [(symbol=? 'B (first sub-grid))
                     (cells->tallies/acc (rest sub-grid) (add1 acc))]
                    [else (cons acc
                                (cells->tallies/acc (rest sub-grid)
                                                    first-column))])]))

;; End of helper functions for PART B.

;; (cells->tallies sub-grid) produces a list of tallies of
;; adjacent black cells where each tally is represented by
;; a natrual number.

;; cells->tallies: (listof (anyof ’B ’-)) -> (listof Nat)
;; Example
(check-expect (cells->tallies (second fish-grid))
              (list 3 1))
(check-expect (cells->tallies (fifth fish-grid))
              (list 2 1))

(define (cells->tallies sub-grid)
  (cells->tallies/acc sub-grid first-column))

;; Tests
(check-expect (cells->tallies (first fish-grid))
              (list 2 4 1))
(check-expect (cells->tallies (fifth fish-grid))
              (list 2 1))
(check-expect (cells->tallies (fourth fish-grid))
              (list 1 5))

;; End of PART B.

;; Beginning of PART C.

;; Helper functions for PART C:

;; (puzzle-labels/row Grid) generates the labels for the rows
;; of the Grid.

;; puzzle-labels/row: Grid -> (listof (listof Nat) (listof Nat))
(define (puzzle-labels/row Grid)
  (cond [(empty? Grid) empty]
        [else (cons (cells->tallies (first Grid))
                    (puzzle-labels/row (rest Grid)))]))

;; (puzzle-labels/col Grid n) generates the labels fot the columns
;; of the Grid.

;; puzzle-labels/row: Grid Nat -> (listof (listof (listof Any))
(define (puzzle-labels/col Grid n)
  (cond [(= n (length (first Grid))) empty]
        [else (cons (cells->tallies (column n Grid))
                    (puzzle-labels/col Grid (add1 n)))]))

;; End of helper functions for PART B.

;; (puzzle-labels grid) generates the row and column labels for
;; the inputted Grid.

;; puzzle-labels: Grid -> (listof (listof (listof Nat)))

(define (puzzle-labels grid)
  (list (puzzle-labels/row grid)
        (puzzle-labels/col grid first-column)))

;; Tests:
(check-expect (puzzle-labels fish-grid) (list (list (list 2 4 1)
                                                    (list 3 1)
                                                    (list 3 2)
                                                    (list 1 5)
                                                    (list 2 1))
                                              (list (list 1 1)
                                                    (list 5)
                                                    (list 2)
                                                    (list 2)
                                                    (list 1 1)
                                                    (list 1 1)
                                                    (list 1 2)
                                                    (list 1 1)
                                                    (list 3)
                                                    (list 1)
                                                    empty
                                                    (list 1))))

;; End of PART C.
