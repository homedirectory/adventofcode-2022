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

(define (loser s)
  (case s
    ['rock 'scissors]
    ['paper 'rock]
    ['scissors 'paper]))

(define (winner s)
  (case s
    ['rock 'paper]
    ['paper 'scissors]
    ['scissors 'rock]))

(define (parse-them s)
  (case s
    ['A 'rock]
    ['B 'paper]
    ['C 'scissors]))

; them - parsed symbol
; x - unparsed
(define (parse-you them s)
  (case s
    ; X - lose
    ['X (loser them)]
    ; Y - draw
    ['Y them]
    ; Z - win
    ('Z (winner them))))

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

(define (parse-line line)
  (let ([symbols (map string->symbol (string-split line " "))])
    (let ([them (parse-them (car symbols))])
      (list them 
            (parse-you them (cadr symbols))))))

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
