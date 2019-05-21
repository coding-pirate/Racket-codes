;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname match) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Harsh Shah (20774723)
;; CS 135 Fall 2018
;; Assignment 08, Problem 5
;; ***************************************************
;;

(require "partition.rkt")
(require "a08helpers.rkt")
(require "ranking.rkt")

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


;; (match employers students) matches students to their
;; respective employers using the preferences students
;; give to employers and the preferences employers give
;; to students.

;; match: (listof EmpRanking) (listof StdRanking) ->
;;        (listof (list EmpId StdId))
;; Examples:
