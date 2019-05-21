;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fib) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Harsh Shah (20774723)
;; CS 135 Fall 2018
;; Assignment 09, Problem 2
;; ***************************************************
;;

;; (fast-fib i) calculates the ith fibonacci number.

;; fast-fib: Nat -> Nat
;; Examples:
(check-expect (fast-fib 0) 0)
(check-expect (fast-fib 1) 1)
(check-expect (fast-fib 6) 8)
(check-expect (fast-fib 87) 679891637638612258)
 
(define (fast-fib i)
  (cond [(= i 0) 0]
        [(= i 1) 1]
        [(= i 2) 1]
        [(even? i) (* (fast-fib (/ i 2))
                      (- (* 2 (fast-fib (+ (/ i 2) 1)))
                         (fast-fib (/ i 2))))]
        [(odd? i) (+ (sqr (fast-fib (/ (- i 1) 2)))
                     (sqr (fast-fib (+ (/ (- i 1) 2) 1))))]))

;; Tests:
(check-expect (fast-fib 42) 267914296)
(check-expect (fast-fib 101) 573147844013817084101)
(check-expect (fast-fib 234) 3577855662560905981638959513147239988861837901112)
(check-expect (fast-fib 300)
              222232244629420445529739893461909967206666939096499764990979600)