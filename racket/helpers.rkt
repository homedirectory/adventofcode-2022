#lang racket/base

(require racket/function racket/string)

(provide (all-defined-out))

;------------------------------------------------------------

(define (sum lst) (foldl + 0 lst))
(define empty-string? (negate non-empty-string?))
