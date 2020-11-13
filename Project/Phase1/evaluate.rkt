#lang racket

(require racket/include)

(require "environment.rkt")
(require "lexer_parser.rkt")
(require "list-utils.rkt")
(require "arith-ops.rkt")
(require "interpreter.rkt")


(define (evaluator filename)
  (let ((code (file->string filename)))
    (define my-lexer (lex-this simple-lexer (open-input-string code)))
    (let ((parser-res (simple-parser my-lexer))) (value-of parser-res (empty-env)))
    
    ))


(evaluator "a.txt")