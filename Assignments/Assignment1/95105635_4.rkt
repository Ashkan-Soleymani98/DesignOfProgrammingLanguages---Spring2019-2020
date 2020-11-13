#lang racket
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (recursive-read input-port)
  (let ((input-command (read-syntax 13 input-port)))
    (cond
      [(eof-object? input-command) `()]
      [else (cons input-command (recursive-read input-port))]
      )
    )
  )

(define (recursive-run commands)
  (cond
    [(null? commands) `()]
    [else (cons (eval (car commands) ns) (recursive-run (cdr commands)))]
    )
  )

(define (recursive-write results output-port)
  (cond
    [(null? results) `()]
    [else
     (write (car results) output-port)
     (newline output-port)
     (recursive-write (cdr results) output-port)
     ]
    )
  )

(define (main input-file-name output-file-name)
  (let
      ((input-port (open-input-file input-file-name))
      (output-port  (open-output-file output-file-name)))
    (recursive-write (recursive-run (recursive-read input-port)) output-port)
    (close-output-port output-port)
    (close-input-port input-port)
    )
  )

(main "input.txt" "output.txt")
    