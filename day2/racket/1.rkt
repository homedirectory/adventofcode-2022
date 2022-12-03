#lang racket/base

(require racket/string)
(require "../helpers.rkt")

(define FILENAME "input")

; rock
;   rock : draw (0)
;   paper: lose (-1)
;   scissors: win (1)
; paper
;   rock : win (1)
;   paper: draw (0)
;   scissors: lose (-1)
; scissors
;   rock : lose (-1)
;   paper: win (1)
;   scissors: draw (0)

(define (rock? s) (eq? 'rock s))
(define (paper? s) (eq? 'paper s))
(define (scissors? s) (eq? 'scissors s))

(define (parse-them s)
  (case s
    ['A 'rock]
    ['B 'paper]
    ['C 'scissors]))

(define (parse-you s)
  (case s
    ['X 'rock]
    ['Y 'paper]
    ['Z 'scissors]))

(define (score s)
  (case s
    ['rock 1]
    ['paper 2]
    ['scissors 3]))

(define (round you them)
  (cond [(eq? you them) 3] ; draw
        [(or (and (rock? you) (scissors? them))
             (and (paper? you) (rock? them))
             (and (scissors? you) (paper? them)))
         6] ; win
        [(or (and (rock? you) (paper? them))
             (and (paper? you) (scissors? them))
             (and (scissors? you) (rock? them)))
         0] ; lose
        [else (error "round: " you them)]))

(define (round-score them you)
  (+ (score you) (round you them)))

; returns a list of symbols '(them you)  
(define (parse-line line)
  (let ([strings (string-split line " ")]
        [f (lambda (parser x) (parser (string->symbol x)))]) 
    (list (f parse-them (car strings)) (f parse-you (cadr strings)))))

; recursively
;(define (run line)
;  (if (eof-object? line)
;    0
;    (+ (apply round-score (parse-line line)) (run (read-line in)))))

;(run (read-line in))

; iteratively
(define (iter total line)
  (if (eof-object? line)
    total
    (begin (displayln line)
      (let ([parsed (parse-line line)])
        (displayln parsed)
        (iter (+ total (apply round-score parsed)) (read-line in))))))

(define in (open-input-file FILENAME #:mode 'text))

(iter 0 (read-line in))
