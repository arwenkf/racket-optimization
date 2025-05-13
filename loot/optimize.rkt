#lang racket
(provide optimize)
(require "ast.rkt"
         "parse.rkt"
         "finterp.rkt")

(module+ test (require rackunit))