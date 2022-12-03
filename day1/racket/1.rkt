#lang racket/base

(require aoc2022)

(define FILENAME "input")

(define in (open-input-file FILENAME #:mode 'text))

(define (iter curr-max calories line)
  (if (eof-object? line)
    curr-max
    (if (empty-string? line)
      (let ([new-max (max curr-max (sum calories))])
        (iter new-max '() (read-line in)))
      (iter curr-max (cons (string->number line) calories) (read-line in)))))

(iter 0 '() (read-line in))
