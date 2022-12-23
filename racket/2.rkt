#lang racket/base

(require racket/list)
(require "../helpers.rkt")

(define FILENAME "input")

(define in (open-input-file FILENAME #:mode 'text))

(define (iter totals elf-calories line)
  (if (eof-object? line)
    (sum (take (sort totals >) 3))
    (if (empty-string? line)
      (iter (cons elf-calories totals) 0 (read-line in))
      (iter totals (+ elf-calories (string->number line)) (read-line in)))))

(iter '() 0 (read-line in))
