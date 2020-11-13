#lang racket


(require racket/include)
(require "environment.rkt")
(require "lexer_parser.rkt")
(require "list-utils.rkt")
(require "arith-ops.rkt")


(define value-of
  (lambda (tree env)
    (let ((action (car tree)))
      (displayln "--------")
      (displayln tree)
      (displayln env)
      (cond
        [(equal? action 'unitcommand)
         (let ((result (value-of (cadr tree) env)))
           (if (equal? (caddr result) 'END)
               (begin
                 ;(printf "Evaluation Result is ~a!\n" (car result))
                 ;(printf "Final Environment is ~a!\n" (cadr result))
                 (list (car result) (cadr result) 'END))
               result)
          )
         ]

        [(equal? action 'multicommands)
         (let ((result (value-of (cadr tree) env)))
           (if (equal? (caddr result) 'END)
               (begin
                 ;(printf "Evaluation Result is ~a!\n" (car result))
                 ;(printf "Final Environment is ~a!\n" (cadr result))
                 (list (car result) (cadr result) 'END))
               (value-of (caddr tree) (cadr result)))
          )
         ]

        [(equal? action 'whilecom) (value-of (cadr tree) env)]
        [(equal? action 'ifcom) (value-of (cadr tree) env)]
        [(equal? action 'assigncom) (value-of (cadr tree) env)]
        [(equal? action 'returncom) (value-of (cadr tree) env)]

        [(equal? action 'while)
         (let ((exp-result (value-of (cadr tree) env)))
           (if (and (car exp-result) (not (eqv? (caddr exp-result) 'END)))
               (let* ((new-env (cadr exp-result))
                      (command-result (value-of (caddr tree) new-env)))
                 (displayln "########&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&")
                 (displayln (cadr tree))
                 (displayln command-result)
                 (displayln "########&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&")
                 (if (not (eqv? (caddr command-result) 'END))
                     (let* ((condition-new-env (cadr command-result)))
                       (value-of tree condition-new-env))
                      command-result)
                 )
                 exp-result
             )
           )
         ]

        [(equal? action 'if)
         (let* ((exp-result (value-of (cadr tree) env))
                (new-env (cadr exp-result)))
           (if (car exp-result)
               (value-of (caddr tree) new-env)
               (value-of (cadddr tree) new-env))
           )
         ]

        
        [(equal? action 'seteq)
         (let* ((result (value-of (caddr tree) env)))
           (list (car result) (extend-env (cadr tree) (car result) (cadr result)) 'NOTEND)
           )
         ]

        [(equal? action 'return)
         (let ((result (value-of (cadr tree) env)))
           (list (car result) (cadr result) 'END)
           )
         ]

        [(equal? action 'aexp) (value-of (cadr tree) env)]
        [(equal? action 'greater?)
         (let* ((valu (value-of (cadr tree) env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left))
           )
           (list (greater right-operand left-operand) second-new-env 'NOTEND)
           )
        ]
        [(equal? action 'less?)
         (let* ((valu (value-of (cadr tree) env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left))
           )
           (list (smaller right-operand left-operand) second-new-env 'NOTEND)
           )
        ]
        [(equal? action 'eqs?)
         (let* ((valu (value-of (cadr tree) env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left))
           )
           (list (equality right-operand left-operand) second-new-env 'NOTEND)
           )
        ]
        [(equal? action 'noteqs?)
         (let* ((valu (value-of (cadr tree) env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left))
           )
           (list (inequality right-operand left-operand) second-new-env 'NOTEND)
           )
        ]

        [(equal? action 'bexp) (value-of (cadr tree) env)]

        [(equal? action 'subtract)
         (let* ((valu (value-of (cadr tree) env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left))
           )
           (list (arith-co right-operand "-" left-operand) second-new-env 'NOTEND)
           )
        ]

        [(equal? action 'add)
         (let* ((valu (value-of (cadr tree) env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left))
           )
           (list (arith-co right-operand "+" left-operand) second-new-env 'NOTEND)
           )
        ]

        [(equal? action 'cexp) (value-of (cadr tree) env)]
        [(equal? action 'mult)
         (let* ((valu (value-of (cadr tree) env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left))
           )
           (list (arith-co right-operand "*" left-operand) second-new-env 'NOTEND)
           )
        ]
        [(equal? action 'div)
         (let* ((valu (value-of (cadr tree) env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left))
           )
           (list (arith-co right-operand "/" left-operand) second-new-env 'NOTEND)
           )
        ]

        [(equal? action 'minus)
         (let* ((valu (value-of (cadr tree) env))
           (right-operand (car valu))
           (new-env (cadr valu))
           )
           (list (reverse right-operand) new-env 'NOTEND)
           )
        ]
        [(equal? action 'expinpar) (value-of (cadr tree) env)]
        [(equal? action 'number) (list (cadr tree) env 'NOTEND)]
        [(equal? action 'null) (list '() env 'NOTEND)]
        [(equal? action 'bool) (list (cadr tree) env 'NOTEND)]
        [(equal? action 'string) (list (cadr tree) env 'NOTEND)]
        [(equal? action 'var) (list (apply-env env (cadr tree)) env 'NOTEND)]
        [(equal? action 'list) (value-of (cadr tree) env)]
        [(equal? action 'cflist)
         (let* ((result-indices (value-of (caddr tree) env))
                (indices (car result-indices))
                (new-env (cadr result-indices))
                (input-list (apply-env env (cadr tree))))
           (list (get-list-item input-list indices) new-env 'NOTEND)
           )
         ]      

        [(equal? action 'listvalues) (value-of (cadr tree) env)]
        [(equal? action 'emptylist) (list '() env 'NOTEND)]

        [(equal? action 'lval)
         (let ((result (value-of (cadr tree) env)))
           (list (list (car result)) (cadr result) 'NOTEND)
           )
         ]
        [(equal? action 'lvals)
         (let* ((result-val (value-of (cadr tree) env))
                (result-vals (value-of (caddr tree) (cadr result-val))))
           (list (cons (car result-val) (car result-vals)) (cadr result-vals) 'NOTEND)
           )
         ]

        [(equal? action 'listmem)
         (let ((result (value-of (cadr tree) env)))
           (list (list (car result)) (cadr result) 'NOTEND)
           )
         ]
        [(equal? action 'listmems)
         (let* ((result-val (value-of (cadr tree) env))
                (result-vals (value-of (caddr tree) (cadr result-val))))
           (list (cons (car result-val) (car result-vals)) (cadr result-vals) 'NOTEND)
           )
         ]
        )
      )
    )
  )
                

(define my-lexer (lex-this simple-lexer (open-input-string "a = [true, false, 2 > 1]; if a == 1 then return true else b = -a endif; while true do if a[0] == a[1] then return a else a = a + true endif end; return 2")))
;(define my-lexer (lex-this simple-lexer (open-input-string "a = 2; return a")))
;(let ((parser-res (simple-parser my-lexer))) (displayln parser-res))
(let ((parser-res (simple-parser my-lexer))) (value-of parser-res (empty-env)))

          
(provide (all-defined-out))

        
                      
        

      