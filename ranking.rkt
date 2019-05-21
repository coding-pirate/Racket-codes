;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ranking) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Harsh Shah (20774723)
;; CS 135 Fall 2018
;; Assignment 08, Problem 4
;; ***************************************************
;;

(require "partition.rkt")

;; An employer ID, EmpId, is a Str (e.g. "Google")
;; A student ID, StdId, is a Str (e.g. "Anna")
;; An Id is (anyof EmpId StdId)
;; A preference, Pref, is a Nat (e.g. 2)
;; requires: a preference is >= 1

;; An EmpRanking is (list EmpId (listof (list StdId Pref)))
;; requires: The list of preferences is in non-decreasing order
;;           and non-empty.

;; A StdRanking is (list StdId (listof (list EmpId Pref)))
;; requires: The list of preferences is in non-decreasing order
;;           and non-empty.

;; A Ranking is (anyof EmpRanking StdRanking)

(define employers
  '(("Manulife" (("Anna" 1) ("Feisal" 2) ("Zihan" 2) ("Hannah" 2)))
    ("Google" (("Anna" 1) ("Feisal" 2) ("Rafelia" 2)))
    ("Apple" (("Rafelia" 1) ("Zihan" 2) ("Feisal" 3)))
    ))

(define employers-2
  '(("Slack" (("Ameen" 1) ("Yazan" 1) ("Harsh" 2)))
    ("AQ" (("Harsh" 1) ("Ameen" 2)))
    ("Bridgewater" (("Harsh" 1) ("Yazan" 2)))
    ("TB" (("Yazan 1") ("Harsh" 2)))
    ))

(define students
  '(("Anna" (("Google" 1) ("Manulife" 2)))
    ("Feisal" (("Google" 1) ("Apple" 1) ("Manulife" 1)))
    ("Rafelia" (("Apple" 1) ("Google" 2)))
    ("Zihan" (("Apple" 1) ("Manulife" 1)))
    ("Hannah" (("Manulife" 1)))
    ))

(define students-2
  '(("Yazan" (("AQ" 1) ("Slack" 2) ("TB" 2)))
    ("Harsh" (("Bridgewater" 1) ("TB" 1)))
    ("Ameen" (("TB" 1) ("AQ" 2) ("Slack" 3)))
    ))

;; A FlatRanking is a (list Str Str Num).
(define flat-employers
  '(("Manulife" "Anna" 1)
    ("Manulife" "Feisal" 2)
    ("Manulife" "Zihan" 2)
    ("Manulife" "Hannah" 2)
    ("Google" "Anna" 1)
    ("Google" "Feisal" 2)
    ("Google" "Rafelia" 2)
    ("Apple" "Rafelia" 1)
    ("Apple" "Zihan" 2)
    ("Apple" "Feisal" 3)
    ))

(define flat-students
  '(("Anna" "Google" 1)
    ("Anna" "Manulife" 2)
    ("Feisal" "Google" 1)
    ("Feisal" "Apple" 1)
    ("Feisal" "Manulife" 1)
    ("Rafelia" "Apple" 1)
    ("Rafelia" "Google" 2)
    ("Zihan" "Apple" 1)
    ("Zihan" "Manulife" 1)
    ("Hannah" "Manulife" 1)))

;; ======================PART A=======================

;; (expunge lofr rankingID rankedID) and produces the same list of
;; flat rankings but without the elemenets containing the rankingID
;; or rankedID.

;; expunge: (listof FlatRanking) Id Id -> (listof FlatRanking)
;; Examples:
(check-expect (expunge flat-employers "Apple" "")
              '(("Manulife" "Anna" 1)
                ("Manulife" "Feisal" 2)
                ("Manulife" "Zihan" 2)
                ("Manulife" "Hannah" 2)
                ("Google" "Anna" 1)
                ("Google" "Feisal" 2)
                ("Google" "Rafelia" 2)))              

(define (expunge lofr rankingID rankedID)
  (filter (lambda (x)
            (not (or (string=? rankingID (first x))
                     (string=? rankedID (second x)))))
          lofr))

;; Tests:
(check-expect (expunge flat-students "Feisal" "")
              '(("Anna" "Google" 1)
                ("Anna" "Manulife" 2)
                ("Rafelia" "Apple" 1)
                ("Rafelia" "Google" 2)
                ("Zihan" "Apple" 1)
                ("Zihan" "Manulife" 1)
                ("Hannah" "Manulife" 1)))
(check-expect (expunge flat-employers "Google" "Feisal")
              '(("Manulife" "Anna" 1)
                ("Manulife" "Zihan" 2)
                ("Manulife" "Hannah" 2)
                ("Apple" "Rafelia" 1)
                ("Apple" "Zihan" 2)))

;; ======================PART B=======================

;; (ranking-sort lofr) consumes a list of FlatRanking and produces
;; the same data however reordered so that the Ranking IDs are in
;; increasing alphabetical order.

;; ranking-sort: (listof FlatRanking) -> (listof FlatRanking)
;; Examples:
(check-expect (ranking-sort '(("A" "c" 1) ("A" "a" 78) ("B" "a" 5)))
              '(("A" "a" 78) ("A" "c" 1) ("B" "a" 5)))
(check-expect (ranking-sort '(("A" "b" 23) ("A" "a" 11)))
              '(("A" "a" 11) ("A" "b" 23)))

(define (ranking-sort lofr)
  (map (lambda (x) (list
                    (string (first x))
                    (string (second x))
                    (char->integer (third x))))
       (map (lambda (y) (string->list y))
            (quicksort
             (map (lambda (z)
                    (string
                     (first (string->list (first z)))
                     (first (string->list (second z)))
                     (first (string->list (string
                                           (integer->char (third z))))))) lofr)
             string<?))))

;; Tests:
(check-expect (ranking-sort '(("Y" "b" 13) ("Z" "c" 11) ("X" "a" 12)))
              '(("X" "a" 12) ("Y" "b" 13) ("Z" "c" 11)))
(check-expect (ranking-sort '(("G" "h" 11) ("G" "g" 12) ("G" "i" 13)))
              '(("G" "g" 12) ("G" "h" 11) ("G" "i" 13)))

;; ======================PART C=======================

;; (unfold lor) consumes a list of rankings and produces an
;; equivalent list of FlatRankings.

;; unfold: (listof Ranking) -> (listof FlatRanking)
;; Examples:
(check-expect (unfold employers) flat-employers)
(check-expect (unfold students) flat-students)

(define (unfold lor)
  (foldr append '()
         (map (lambda (x)
                (map (lambda (y) (cons (first x) y)) (second x))) lor)))

;; Tests:
(check-expect (unfold employers-2)
              '(("Slack" "Ameen" 1)
                ("Slack" "Yazan" 1)
                ("Slack" "Harsh" 2)
                ("AQ" "Harsh" 1)
                ("AQ" "Ameen" 2)
                ("Bridgewater" "Harsh" 1)
                ("Bridgewater" "Yazan" 2)
                ("TB" "Yazan 1")
                ("TB" "Harsh" 2)))
(check-expect (unfold students-2)
              '(("Yazan" "AQ" 1)
                ("Yazan" "Slack" 2)
                ("Yazan" "TB" 2)
                ("Harsh" "Bridgewater" 1)
                ("Harsh" "TB" 1)
                ("Ameen" "TB" 1)
                ("Ameen" "AQ" 2)
                ("Ameen" "Slack" 3)))

;; ======================PART D=======================

(define flat-employers-2 (unfold employers-2))
(define flat-students-2 (unfold students-2))

;; (find key lst) consumes a list and produces the first element
;; within the list that has the first element equal to the key.

;; find: Any (listof (listof Any)) -> (listof Any)
;; Examples:
(check-expect (find "Feisal" flat-students) '("Feisal" "Google" 1))
(check-expect (find "Apple" flat-employers) '("Apple" "Rafelia" 1)) 

(define (find key lst)
  (cond [(ormap (lambda (x)
                  (equal? key (first x))) lst)
         (first
          (foldr (lambda (y z)
                   (cond [(equal? key (first y)) (cons y z)]
                         [else z]))
                 '()
                 lst))]
        [else false]))

;; Tests:
(check-expect (find "Harsh" flat-students-2) '("Harsh" "Bridgewater" 1))
(check-expect (find "Bridgewater" flat-employers-2)
              '("Bridgewater" "Harsh" 1))
(check-expect (find "Hannah" flat-students-2) false)

;; ======================PART E=======================

;; (find-pref lor rankingID rankedID) consumes a list of Rankings,
;; a Ranking ID and a Ranked ID and produces the preference the
;; Ranking ID has given to the Ranked ID.

;; find-pref: (listof Ranking) Id Id -> Pref
;; Requires: The preference must exist within the list of
;;           rankings
;; Examples:
(check-expect (find-pref employers "Google" "Feisal") 2)
(check-expect (find-pref students "Rafelia" "Google") 2)

(define (find-pref lor rankingID rankedID)
  (second (first
           (filter
            (lambda (x)
              (string=? rankedID (first x)))
            (second (first
                     (filter
                      (lambda (y)
                        (string=? rankingID (first y))) lor)))))))

;; Tests:
(check-expect (find-pref employers-2 "Slack" "Harsh") 2)
(check-expect (find-pref students-2 "Harsh" "TB") 1)
(check-expect (find-pref students-2 "Ameen" "AQ") 2)