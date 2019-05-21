;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cs135flix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; A Movie-fv is a (list Nat Nat Nat Nat Nat Nat Nat Nat)
;; A Rating is an Int
;; requires: Int is either -1 or 1

;; A Pref-v is a (list Int Int Int Int Int Int Int Int)


;; PART A

;; Helper functions for PART A:

;; (sum-list list1 list2) determines the sum of two vectors by
;; adding two lists of integers in a pairwise fashion.
;; sum-list: (listof Int) (listof Int) -> (listof Int)
;; Example:
(check-expect (sum-list (list 0 1 0 1 0 1 0 1)
                        (list 1 0 1 0 1 0 1 0))
              (list 1 1 1 1 1 1 1 1))

(define (sum-list list1 list2)
  (cond [(empty? list1) empty]
        [else (cons (+ (first list1) (first list2))
                    (sum-list (rest list1) (rest list2)))]))

;; (multiply list1 n) multiplies all elements of list1 by n.
;; multiply-list: (listof Int) Int -> (listof Int)
;; Example:
(check-expect (multiply-list (list 1 0 1 0 0 0 1 0) -1)
              (list -1 0 -1 0 0 0 -1 0))
              
(define (multiply-list list1 n)
  (cond [(empty? list1) empty]
        [else (cons (* n (first list1)) (multiply-list (rest list1) n))]))

;; End of helper functions for PART A!

;; (find-preference Rating Movie-fv) consumes the Rating and Movie-fv
;; lists (of same length) and produces a Pref-v which is an 8 element
;; list of that consists of the sum of the scores for each of the
;; eight genres.

;; find-preference: (listof Rating) (listof Movie-fv) -> Pref-v
;; requires: both lists are the same size
;;           both lists are non-empty
;; Examples:
(check-expect (find-preference (list 1)
                               (list (list 1 1 1 0 0 0 0 0)))
              (list 1 1 1 0 0 0 0 0))
(check-expect (find-preference (list 1 1 -1)
                               (list (list 1 1 0 0 0 0 0 0)
                                     (list 1 1 1 0 0 0 0 0)
                                     (list 0 1 1 1 0 0 0 0)))
              (list 2 1 0 -1 0 0 0 0))

(define (find-preference Rating Movie-fv)
  (cond [(empty? Rating) (list 0 0 0 0 0 0 0 0)]
        [else (sum-list (multiply-list (first Movie-fv) (first Rating))
                        (find-preference (rest Rating) (rest Movie-fv)))]))

;;Tests:
(check-expect (find-preference (list -1)
                               (list (list 1 1 1 0 0 0 0 0)))
              (list -1 -1 -1 0 0 0 0 0))
(check-expect (find-preference (list -1 1 1)
                               (list (list 1 0 0 1 0 0 0 0)
                                     (list 0 0 0 1 0 0 0 0)
                                     (list 0 0 0 0 0 0 1 0)))
              (list -1 0 0 0 0 0 1 0))
(check-expect (find-preference (list 1 1 1 1 1 1 1 1)
                               (list (list 0 -1 1 0 1 1 0 1)
                                     (list -1 -1 1 -1 1 0 1 1)
                                     (list 1 0 1 -1 -1 0 1 1)
                                     (list 0 1 1 0 1 1 1 0)
                                     (list 0 -1 1 -1 -1 1 1 1)
                                     (list 0 1 1 0 0 -1 1 1)
                                     (list 0 1 1 -1 -1 1 1 1)
                                     (list -1 -1 0 1 0 1 -1 0)))
              (list -1 -1 7 -3 0 4 5 6))


(define-struct movie (title genres))
;; A Movie is a (make-movie Str Movie-fv)

;; Helper functions for PART B:

;; (dot-product list1 list2) calculates the dot product of two
;; vectors given in the form of lists.
;; dot-product: (listof Int) (listof Int) -> Int
;; Requirement: both lists must be of same lengths
;; Examples:
(check-expect (dot-product (list 1 1 1 2 0 0 0 1)
                           (list 0 0 1 2 0 2 2 -1))
              4)

(define (dot-product list1 list2)
  (cond [(empty? list1) 0]
        [else (+ (* (first list1) (first list2))
                 (dot-product (rest list1) (rest list2)))]))

;; (insert list1 n) inserts n into the sorted list list1 so that
;; the resulting list is also sorted.
;; insert: (listof Int) Int -> (listof Int)
;; Examples:
(check-expect (insert 4 (list 1 2 5))
              (list 4 1 2 5))

(define (insert n list1)
  (cond [(empty? list1) (cons n empty)]
        [(>= n (first list1)) (cons n list1)]
        [else (cons (first list1) (insert n (rest list1)))]))

;; (sort list1) sorts list1 into non-increasing order.
;; sort: (listof Int) -> (listof Int)
;; Examples: 
(check-expect (sorter (list 2 3 1 5))
              (list 5 3 2 1))

(define (sorter list1)
  (cond [(empty? list1) empty]
        [else (insert (first list1) (sorter (rest list1)))]))

;; (movie-order Pref-v Movie-fv) calculates the dot product of the Pref-v
;; list and each of the movie-genres in the Movie-fv list. It then uses the
;; sorter helper function to determine the order of scores in the output
;; list.
;; movie-order: Pref-v (listof Movie-fv) -> (listof Int)
;; Example:
(check-expect (movie-order
               (list 0 1 0 1 1 0 2 0)
               (list (make-movie "Dark Knight Returns" (list 0 1 0 0 0 0 1 0))
                     (make-movie "Batman" (list 0 1 0 0 0 0 1 0))
                     (make-movie "Dark Knight" (list 1 0 0 1 1 0 1 0))))
              (list 4 3 3))

(define (movie-order Pref-v Movie-fv)
  (cond [(empty? Movie-fv) empty]
        [else (sorter (cons (dot-product Pref-v (movie-genres (first Movie-fv)))
                            (movie-order Pref-v (rest Movie-fv))))]))

;; End of helper functions for PART B!

;; (suggestions Pref-v Movie-fv) takes the dot product of the
;; Pref-v and the Movie-fv to get a score and then determines the
;; movie with the highest score.

;; suggestions: Pref-v (listof Movie) -> Str
;; requires: (listof Movie) is non-empty
;; Examples:
(check-expect (suggestions
               (list 2 1 0 -1 0 0 0 0)
               (list (make-movie "The Meg" (list 1 0 0 0 1 0 1 0))
                     (make-movie "Smallfoot" (list 0 1 1 0 0 0 0 0))
                     (make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0))))
              "The Meg")

(define (suggestions Pref-v Movie-fv)
  (cond [(= (first (movie-order Pref-v Movie-fv))
            (dot-product Pref-v (movie-genres (first Movie-fv))))
         (movie-title (first Movie-fv))]
        [else (suggestions Pref-v (rest Movie-fv))]))

;; Tests:
(check-expect (suggestions
               (list 0 1 0 1 1 0 2 0)
               (list (make-movie "Dark Knight Returns" (list 0 1 0 0 0 0 1 0))
                     (make-movie "Batman" (list 0 1 0 0 0 0 1 0))
                     (make-movie "Dark Knight" (list 1 0 0 1 1 0 1 0))))
              "Dark Knight")
(check-expect (suggestions
               (list -1 -1 7 -3 0 4 5 6)
               (list (make-movie "Halloween" (list 0 0 0 0 1 0 0 1))
                     (make-movie "Goosebumps 2" (list 0 0 1 0 1 0 0 0))
                     (make-movie "Venom" (list 1 0 0 0 0 0 1 0))
                     (make-movie "Star is Born" (list 0 0 0 1 0 1 0 0))))
              "Goosebumps 2")
