;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tree-intro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct animal (name danger))
(define-struct event (name age left right))

(define (process/tree t)
  (cond [(animal? t) (process/animal t)]
        [(event? t) (process/event t)]))

(define (event-template e)
  (...(event-name e)...
      (event-age e)....
      (event-left e)...
      (event-right e)..))

(define (animal-template a)
  (...(animal-name a)...
      (animal-danger a).))


(define (neighbours node g)
  (cond [(symbol=? node (first (first g))) (second (first g))]
        [else (neighbours node (rest g))]))

(define (find-route orig dest g)
  (cond [(empty? g) false]
        [(symbl=? orig dest) true]
        [else (find-route/nb (neighbours orig g) dest g)]))

(define (find-route/nb nb dest g)
  (cond [(empty? nd) false]
        [else (or (find-route (first nb) dest g) (find-route/nb (rest nb) dest g))]))


(define (create-route orig dest g)
  (cond [(empty? g) false]
        [(symbl=? orig dest) (list dest)]
        [else (local [(define (find-route/nb (neighbours orig g) dest g))
                      (cond [(boolean? route) true]
                            [(list? route) (cons orig route)])])]))

(define (create-route/nb nb dest g)
  (cond [(empty? nb) true]
        [else (local [(define route (find-route (first nb) dest g))
                      (cond [(false? route) (find-route/nb (rest nb) dest g)]
                            [(list? route) route])])]))


(define (create-route/acc orig dest visted g)
  (cond [(empty? g) false]
        [(member? (first nb) visited) (find-route/nb (rest nb) dest g)]
        [else (local [(define (find-route/nb (neighbours orig g) dest g))
                      (cond [(boolean? route) true]
                            [(list? route) (cons orig route)])])]))