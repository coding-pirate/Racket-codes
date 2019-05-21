;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname stringtrees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Harsh Shah (20774723)
;; CS 135 Fall 2018
;; Assignment 07, Problem 3
;; ***************************************************
;;

;; A StringTree is a (list Str (listof StringTree))

(define celebs '("Punk" (("Tokido" ())
                         ("Marn" (("JWong" ())
                                  ("Ricki" ())
                                  ("KBrad" ())))
                         ("Smug" (("Fuudo" ())
                                  ("Mena" (("Caba" ())
                                           ("NuckleDu" ())))
                                  ("SnakeEyez" ()))))))

;; ======================PART A=======================

(define big-example-tree '("Funk"
                           (("Hokido" ())
                            ("Barn" (("Jong" ())
                                     ("Nicki" (("Braden" (("Henna" ())
                                                          ("Fuddu" ())
                                                          ("Caaba" ())
                                                          ("Smuggy" ())))
                                               ("FakeEyez" ()))))))))

(define small-example-tree '("Funk"
                             (("Barn"
                               (("Nicki"
                                 (("Braden" ()))))))))

;; ======================PART B=======================

(define (stringtree-template1 ST)
  (cond [(empty? (second ST))...]
        [(string? (first ST))
         (...(first ST)...(stringtree-template1 (second ST))...)]
        [else (...(stringtree-template1 (first ST))...
                  (stringtree-template1 (second ST))..)]))

(define (stringtree-template ST)
  (cond [(empty? (second ST))...]
        [else (...(first ST)...
                  (stringtree-template (second ST))...)]))

;; ======================PART C=======================

;; (list-recurser lst n) produces the nth element of the list
;; lst with the first element being 0.

;; list-recurser: List Nat -> Any
;; Examples:
(check-expect (list-recurser '(a b (1 2 3) "lol") 3) "lol")
(check-expect (list-recurser '(a b (1 2 3) "lol") 2) '(1 2 3)) 

(define (list-recurser lst n)
  (cond [(empty? lst) (error "path is invalid")]
        [(zero? n) (first lst)]
        [else (list-recurser (rest lst) (sub1 n))]))

;; Tests:
(check-error (list-recurser celebs 2) "path is invalid")
(check-expect (list-recurser (second celebs) 0)
              '("Tokido" ()))
(check-expect (list-recurser (second (second (second celebs))) 1)
              '("Ricki" ())) 

(define (string-insert name IP part-ST)
  (cond [(member? name part-ST) part-ST]
        [(and (empty? IP) (empty? (rest part-ST))) (error "path is invalid")]
        [(and (empty? IP) (cons? (rest part-ST))) (error "path is invalid")]
        [(and (>= (length IP) 1) (cons? (rest ST))) (cons (first ))]))

(define (root-checker? IP part-ST)
  (cond [(empty? IP) false]
        [(empty? (rest part-ST)) false]
        [(< (rest part-ST) (first IP)) true]
        [else (root-checker? (rest IP)
                             (list-recurser (rest part-ST) (first IP)))]))
