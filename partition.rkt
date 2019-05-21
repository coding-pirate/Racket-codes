;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname partition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Harsh Shah (20774723)
;; CS 135 Fall 2018
;; Assignment 08, Problem 3
;; ***************************************************
;;

;; (partition pred? lst) produces a two element list where
;; the first element is a list of those items in lst that
;; satisfy the predicate pred? and the second element is a
;; items in lst that do not satisfy the predicate pred?.

;; partition: (Any -> Bool) (listof Any) -> (list (listof Any)
;;                                            (listof Any))
;; Examples:
(check-expect (partition even? '(1 2 3 4 5 6))
              '((2 4 6) (1 3 5)))
(check-expect (partition symbol? '(harsh 2 no))
              '((harsh no) (2)))

(define (partition pred? lst)
  (local
    [;; (filter-true/acc pred? lst acc) produces the elements
     ;; of list (lst) that satisfy the predicate (pred?) using
     ;; an accumulator
     ;; filter-true/acc: (Any -> Bool) (listof Any) Any -> (listof Any)
     (define (filter-true/acc pred? lst acc)
       (cond [(empty? lst) acc]
             [(pred? (first lst))
              (cons (first lst)
                    (filter-true/acc pred? (rest lst) acc))]
             [else (filter-true/acc pred? (rest lst) acc)]))
     
     ;; (filter-true pred? lst) wraps the accumulative function
     ;; filter-true/acc and sets the accumulator to empty
     ;; filter-true: (Any -> Bool) (listof Any) -> (listof Any)
     (define (filter-true pred? lst)
       (filter-true/acc pred? lst empty))
     
     ;; (filter-false/acc pred? lst acc) produces the elements
     ;; of list (lst) that do not satisfy the predicate 
     ;; (pred?) using an accumulator
     ;; filter-false/acc: (Any -> Bool) (listof Any) Any -> (listof Any)
     (define (filter-false/acc pred? lst acc)
       (cond [(empty? lst) acc]
             [(not (pred? (first lst)))
              (cons (first lst)
                    (filter-false/acc pred? (rest lst) acc))]
             [else (filter-false/acc pred? (rest lst) acc)]))
     
     ;; (filter-false pred? lst) wraps the accumulative function
     ;; filter-false/acc and sets the accumulator to empty
     ;; filter-false: (Any -> Bool) (listof Any) -> (listof Any)
     (define (filter-false pred? lst)
       (filter-false/acc pred? lst empty))]
    (list (filter-true pred? lst) (filter-false pred? lst))))

;; Tests:
(check-expect (partition positive? '(1 -1 2 -2 3 4))
              '((1 2 3 4) (-1 -2)))
(check-expect (partition string? '(31 31 "lol" "CS135" 531))
              '(("lol" "CS135") (31 31 531)))
(check-expect (partition odd? '())
              '(() ()))