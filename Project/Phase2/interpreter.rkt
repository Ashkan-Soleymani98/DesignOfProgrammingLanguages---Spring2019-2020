#lang racket

(require racket/include)
(require "environment.rkt")
(require "lexer_parser.rkt")
(require "list-utils.rkt")
(require "arith-ops.rkt")
(require "libfuncs.rkt")

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

;Eval Thunks for Lib
(define eval-thunks
  (lambda (thunks)
    (if (null? thunks)
        '()
        (let ((first (car thunks)))
          (if (is-thunk? first)
               (let* ((body (cadr first))
                      (saved-env (caddr first))
                      (saved-proc-env (cadddr first)))
                 (cons (car (value-of body saved-env saved-proc-env)) (eval-thunks (cdr thunks))))
               (cons first (eval-thunks (cdr thunks))))
           ))
    )
  )
        
    
;Apply Procedure
(define apply-procedure
  (lambda (proc1 vals proc-env)
    (cond
      [(eqv? (car proc1) 'procedure)
        (let* ((p-name (cadr proc1))
             (b-vars (caddr proc1))
             (p-body (cadddr proc1))
             (temp (cddddr proc1))
             (p-env (car temp)))
          (value-of p-body (extend-env-multi p-name b-vars vals p-env) proc-env))]
      [(eqv? (car proc1) 'lib-procedure)
       (let* ((p-name (cadr proc1))
              (vars-length (caddr proc1))
              (evaled-vals (eval-thunks vals)))
         (if (equal? vars-length (length vals))
             (list (apply (eval (string->symbol p-name) ns) evaled-vals) (empty-env) proc-env 'NOTEND) 
             (report-function-arguments-size-missmatch p-name))
         )]
      [else (report-is-not-callable proc1)])
    )
  )

(define (evalcode code)
   (define my-lexer (lex-this simple-lexer (open-input-string code)))
  (displayln code)
    (let ((parser-res (simple-parser my-lexer))) (car (value-of parser-res (empty-env) (initial-proc-env))))
  )


        
          
(define value-of
  (lambda (tree env proc-env)
    (displayln "--------")
    (displayln "--------")
    (displayln tree)
    ;(displayln "--")
    ;(displayln env)
    ;(displayln "--")
    ;(displayln proc-env)
    ;(displayln "--")
    (displayln "--------")
    (let ((action (car tree)))
      (cond
        [(equal? action 'unitcommand)
         (let ((result (value-of (cadr tree) env proc-env)))
           (if (equal? (cadddr result) 'END)
               (begin
                 ;(printf "Evaluation Result is ~a!\n" (car result))
                 ;(printf "Final Environment is ~a!\n" (cadr result))
                 (list (car result) (cadr result) (caddr result) 'END))
               result)
          )
         ]

        [(equal? action 'multicommands)
         (let ((result (value-of (cadr tree) env proc-env)))
           (if (equal? (cadddr result) 'END)
               (begin
                 ;(printf "Evaluation Result is ~a!\n" (car result))
                 ;(printf "Final Environment is ~a!\n" (cadr result))
                 (list (car result) (cadr result) (caddr result) 'END))
               (value-of (caddr tree) (cadr result) (caddr result)))
          )
         ]

        [(equal? action 'whilecom) (value-of (cadr tree) env proc-env)]
        [(equal? action 'ifcom) (value-of (cadr tree) env proc-env)]
        [(equal? action 'assigncom) (value-of (cadr tree) env proc-env)]
        [(equal? action 'returncom) (value-of (cadr tree) env proc-env)]

        [(equal? action 'while)
         (let ((exp-result (value-of (cadr tree) env proc-env)))
           (if (and (car exp-result) (not (eqv? (cadddr exp-result) 'END)))
               (let* ((new-env (cadr exp-result))
                      (new-proc-env (caddr exp-result))
                      (command-result (value-of (caddr tree) new-env new-proc-env)))
                 (if (not (eqv? (cadddr command-result) 'END))
                     (let* ((condition-new-env (cadr command-result))
                            (condition-new-proc-nev (caddr command-result)))
                       (value-of tree condition-new-env condition-new-env))
                      exp-result)
                 )
                 exp-result
             )
           )
         ]

        [(equal? action 'if)
         (let* ((exp-result (value-of (cadr tree) env proc-env))
                (new-env (cadr exp-result))
                (new-proc-env (caddr exp-result)))
           (if (car exp-result)
               (value-of (caddr tree) new-env new-proc-env)
               (value-of (cadddr tree) new-env new-proc-env))
           )
         ]
           
        [(equal? action 'function)
         (let* ((result (value-of (cadr tree) env proc-env))
                (p-vars (car result))
                (p-body (caddr tree))
                (p-env (cadr result)))
           (list (list p-vars p-body p-env) p-env (caddr result) 'NOTEND)
           )
         ]

        [(equal? action 'call-func)
         (let* ((proc (apply-env proc-env (cadr tree)))
                ;(proc-result (value-of (list 'var (cadr tree)) env))
                ;(proc (car proc-result))
                (args-result (value-of (caddr tree) env proc-env))
                (args (car args-result)))
           (apply-procedure proc args (caddr args-result))
           )
         ]

        [(equal? action 'setqfunc)
         (let* ((func-result (value-of (caddr tree) env proc-env))
                (func-list (car func-result))
                (new-env (cadr func-result))
                (p-name (cadr tree))
                (b-vars (car func-list))
                (p-body (cadr func-list))
                (p-env (caddr func-list))
                (proc (procedure p-name b-vars p-body p-env)))
           (list proc (cadr func-result) (extend-env-rec p-name b-vars p-body p-env proc-env) 'NOTEND)
           )
         ]

        [(equal? action 'setqcall)
         (let* ((a-thunk (thunk (caddr tree) env proc-env)))
           (list a-thunk (extend-env (cadr tree) a-thunk env) proc-env 'NOTEND)
           )
         ]
        
        ;[(equal? action 'setqcall)
         ;(let* ((result (value-of (caddr tree) env proc-env)))
           ;(list (car result) (extend-env (cadr tree) (car result) env) proc-env 'NOTEND)
           ;)
         ;]

        [(equal? action 'seteq)
         (let* ((a-thunk (thunk (caddr tree) env proc-env)))
           (list a-thunk (extend-env (cadr tree) a-thunk env) proc-env 'NOTEND)
           )
         ]
           
        
        ;[(equal? action 'seteq)
         ;(let* ((result (value-of (caddr tree) env proc-env)))
           ;(list (car result) (extend-env (cadr tree) (car result) (cadr result)) (caddr result) 'NOTEND)
           ;)
         ;]

         

        [(equal? action 'return)
         (let ((result (value-of (cadr tree) env proc-env)))
           (list (car result) (cadr result) (caddr result) 'END)
           )
         ]

        [(equal? action 'aexp) (value-of (cadr tree) env proc-env)]
        [(equal? action 'greater?)
         (let* ((valu (value-of (cadr tree) env proc-env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env proc-env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (greater right-operand left-operand) second-new-env (caddr val-left) 'NOTEND)
           )
        ]
        [(equal? action 'less?)
         (let* ((valu (value-of (cadr tree) env proc-env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env proc-env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (smaller right-operand left-operand) second-new-env (caddr val-left) 'NOTEND)
           )
        ]
        [(equal? action 'eqs?)
         (let* ((valu (value-of (cadr tree) env proc-env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env proc-env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (equality right-operand left-operand) second-new-env (caddr val-left) 'NOTEND)
           )
        ]
        [(equal? action 'noteqs?)
         (let* ((valu (value-of (cadr tree) env proc-env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env proc-env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (inequality right-operand left-operand) second-new-env (caddr val-left) 'NOTEND)
           )
        ]

        [(equal? action 'bexp) (value-of (cadr tree) env proc-env)]

        [(equal? action 'subtract)
         (let* ((valu (value-of (cadr tree) env proc-env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env proc-env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (arith-co right-operand "-" left-operand) second-new-env (caddr val-left) 'NOTEND)
           )
        ]

        [(equal? action 'add)
         (let* ((valu (value-of (cadr tree) env proc-env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env proc-env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (arith-co right-operand "+" left-operand) second-new-env (caddr val-left) 'NOTEND)
           )
        ]

        [(equal? action 'cexp) (value-of (cadr tree) env proc-env)]
        [(equal? action 'mult)
         (cond
           ((equal? (car (value-of (cadr tree) env proc-env)) 0) (list 0 env proc-env 'NOTEND))
           ((equal? (car (value-of (cadr tree) env proc-env)) #f) (list #f proc-env 'NOTEND))
           (else (list (arith-co (car (value-of (cadr tree) env proc-env)) "*" (car (value-of (caddr tree) env proc-env))) (cadr (value-of (caddr tree) env proc-env)) (caddr (value-of (caddr tree) env proc-env)) 'NOTEND)))
        ]
        [(equal? action 'div)
         (let* ((valu (value-of (cadr tree) env proc-env))
           (right-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env proc-env))
           (left-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (arith-co right-operand "/" left-operand) second-new-env (caddr val-left) 'NOTEND)
           )
        ]

        [(equal? action 'minus)
         (let* ((valu (value-of (cadr tree) env proc-env))
           (right-operand (car valu))
           (new-env (cadr valu)))
           (list (converse right-operand) new-env (caddr valu) 'NOTEND)
           )
        ]
        [(equal? action 'expinpar) (value-of (cadr tree) env proc-env)]
        [(equal? action 'number) (list (cadr tree) env proc-env 'NOTEND)]
        [(equal? action 'null) (list '() env proc-env 'NOTEND)]
        [(equal? action 'bool) (list (cadr tree) env proc-env 'NOTEND)]
        [(equal? action 'string) (list (cadr tree) env proc-env 'NOTEND)]
        ;[(equal? action 'var) (list (apply-env env (cadr tree)) env proc-env 'NOTEND)]
        [(equal? action 'var)
         (let ((temp (apply-env env (cadr tree))))
           (if (is-thunk? temp)
               (let* ((body (cadr temp))
                      (saved-env (caddr temp))
                      (saved-proc-env (cadddr temp)))
                 (list (car (value-of body saved-env saved-proc-env)) env proc-env 'NOTEND))
               (list temp env proc-env 'NOTEND))
           )
         ]
        
        [(equal? action 'list) (value-of (cadr tree) env proc-env)]
        [(equal? action 'cflist)
         (let* ((result-indices (value-of (caddr tree) env proc-env))
                (indices (car result-indices))
                (new-env (cadr result-indices))
                (new-proc-env (caddr result-indices)))
           (let* ((temp (apply-env env (cadr tree)))
                  (input-list (if (is-thunk? temp)
                                  (let* ((body (cadr temp))
                                         (saved-env (caddr temp))
                                         (saved-proc-env (cadddr temp)))
                                    (car (value-of body saved-env saved-proc-env)))
                                  temp)))
             (list (get-list-item input-list indices) new-env new-proc-env 'NOTEND)
             )
           )
         ]    
        [(equal? action 'cflist)
         (let* ((result-indices (value-of (caddr tree) env proc-env))
                (indices (car result-indices))
                (new-env (cadr result-indices))
                (new-proc-env (caddr result-indices))
                (input-list (apply-env env (cadr tree))))
           (list (get-list-item input-list indices) new-env new-proc-env 'NOTEND)
           )
         ]      

        [(equal? action 'listvalues) (value-of (cadr tree) env proc-env)]
        [(equal? action 'emptylist) (list '() env proc-env 'NOTEND)]

        [(equal? action 'lval)
         (let ((result (value-of (cadr tree) env proc-env)))
           (list (list (car result)) (cadr result) (caddr result) 'NOTEND)
           )
         ]
        [(equal? action 'lvals)
         (let* ((result-val (value-of (cadr tree) env proc-env))
                (result-vals (value-of (caddr tree) (cadr result-val) (caddr result-val))))
           (list (cons (car result-val) (car result-vals)) (cadr result-vals) (caddr result-vals) 'NOTEND)
           )
         ]

        [(equal? action 'listmem)
         (let ((result (value-of (cadr tree) env proc-env)))
           (list (list (car result)) (cadr result) (caddr result) 'NOTEND)
           )
         ]
        [(equal? action 'listmems)
         (let* ((result-val (value-of (cadr tree) env proc-env))
                (result-vals (value-of (caddr tree) (cadr result-val) (caddr result-val))))
           (list (cons (car result-val) (car result-vals)) (cadr result-vals) (caddr result-vals) 'NOTEND)
           )
         ]

        [(equal? action 'funcvar)
         (list (list (cadr tree)) env proc-env 'NOTEND)]
        [(equal? action 'funcvars)
         (let* ((var (cadr tree))
                (result-vars (value-of (caddr tree) env proc-env)))
           (list (cons var (car result-vars)) (cadr result-vars) (caddr result-vars) 'NOTEND)
           )
         ]

        [(equal? action 'arg)
         (let* ((a-thunk (thunk (cadr tree) env proc-env)))
           (list (list a-thunk) env proc-env 'NOTEND)
           )
         ]
        
        ;[(equal? action 'arg)
         ;(let ((result (value-of (cadr tree) env proc-env)))
           ;(list (list (car result)) (cadr result) (caddr result) 'NOTEND)
           ;)
         ;]

        [(equal? action 'args)
         (let* ((a-thunk (thunk (cadr tree) env proc-env))
                (result-args (value-of (caddr tree) env proc-env)))
           (list (cons a-thunk (car result-args)) (cadr result-args) (caddr result-args) 'NOTEND)
           )
         ]
        
        ;[(equal? action 'args)
         ;(let* ((result-arg (value-of (cadr tree) env proc-env))
          ;      (result-args (value-of (caddr tree) (cadr result-arg) (caddr result-arg))))
           ;(list (cons (car result-arg) (car result-args)) (cadr result-args) (caddr result-args) 'NOTEND)
           ;)
         ;]

         
        )
      )
    )
  )
                
(define initial-proc-env
  (lambda ()
    (extend-env-lib "pow" 2
                    (extend-env-lib "makelist" 2
                                    (extend-env-lib "reverse" 1
                                                    (extend-env-lib "reverseall" 1
                                                                    (extend-env-lib "set" 3
                                                                                    (extend-env-lib "merge" 2
                                                                                                    (extend-env-lib "mergesort" 1
                                                                                                                    (extend-env-lib "evalcode" 1
                                                                                                                                    (empty-env)))))))))
    )
  )

;(define my-lexer (lex-this simple-lexer (open-input-string "a = 2; f = func(b) {return a}; a = 3; b = f(2); return b"))) ; #1st test
;(define my-lexer (lex-this simple-lexer (open-input-string "f = func(b) {return a}; a = 3; b = f(2); return b"))) ; #2nd test
;(define my-lexer (lex-this simple-lexer (open-input-string "f = func(b) {a = 3; return a}; b = f(2); return a"))) ; #3rd test
;(define my-lexer (lex-this simple-lexer (open-input-string "f = func(b) {a = b; return a; while true do a = a + 1 end }; b = f(3); return b"))) ; #4th test
;(define my-lexer (lex-this simple-lexer (open-input-string "listmaker = func(a, b){if a == 0 then return [] else a = listmaker(a - 1, b); return a + [b] endif}; b = listmaker(3, 5); return b"))) ; #5th test
;(define my-lexer (lex-this simple-lexer (open-input-string "f = func(b) {return 8}; a = f(2/0); b = 2 / 0; return a"))) ; #6th test
;(define my-lexer (lex-this simple-lexer (open-input-string "a = 2; f = func(b, c) {return c}; a = 3; b = f(2/0, 11); return b"))) ; #7th test
;(define my-lexer (lex-this simple-lexer (open-input-string "f = pow(2, 10); return f"))) ; #8th test 
;(define my-lexer (lex-this simple-lexer (open-input-string "f = mergesort([4,5,2,7,8,1,11,22,13,7,9]); return f"))) ; #9th test
;(define my-lexer (lex-this simple-lexer (open-input-string "f = evalcode('a = 2; return a');return f"))) ; #10th test
;(define my-lexer (lex-this simple-lexer (open-input-string "f = makelist(10, 2); return f"))) ; #11th test
;(define my-lexer (lex-this simple-lexer (open-input-string "f = reverse([1, [2, 3], [4, [5, 6]]]); return f"))) ; #12th test
;(define my-lexer (lex-this simple-lexer (open-input-string "f = reverseall([1, [2, 3], [4, [5, 6]]]); return f"))) ; #13th test
;(define my-lexer (lex-this simple-lexer (open-input-string "b = 8;a = [2 - 5.56, b, 55, 47];if -a[-b+10] == -55 then a = 5 else a = 6 endif; return a"))) ;#14th test **
;(define my-lexer (lex-this simple-lexer (open-input-string "c=[];b = [[1], [3, 2, [1, [1, 2, 3]]], 7, [[[11]],22,19]];a = b[1];return a == [3, 2, [1, [1, 2, 3]]]"))) ; #15th test **
;(define my-lexer (lex-this simple-lexer (open-input-string "c=[1, 2, 3, 5, 7]; b = 3 - -4;return b/c+[55]"))) ; #16th test
;(define my-lexer (lex-this simple-lexer (open-input-string "a = 10;c=0;while a > 5 do a = a - 1;c = c + a end;return c"))) ; #17th test
;(define my-lexer (lex-this simple-lexer (open-input-string "return (2 / 0) * 0"))) ; #18th test
;(define my-lexer (lex-this simple-lexer (open-input-string "return 0 * (2 / 0)"))) ; #19th test
;(define my-lexer (lex-this simple-lexer (open-input-string "return (1 == 2) * [1/0, 2, 21]"))) ; #20th test
;(define my-lexer (lex-this simple-lexer (open-input-string "a = evalcode('return (1 == 2) * [1/0, 2, 21]'); return a"))) ; #21st test
;(define my-lexer (lex-this simple-lexer (open-input-string "a = evalcode('b = evalcode('return 1'); return b == 2'); return a"))) ; #22nd test
;(define my-lexer (lex-this simple-lexer (open-input-string "dividebytwo = func(num){i = 0; while num > 0 do num = num - 2; i = i + 1 end; return i}; geq = func(a, b){if a > b then return a > b else return a == b endif}; bsearch = func(list, num, l, r){gq = geq(r, l) ;if gq then d = dividebytwo(r - l); mid = l + d; if list[mid] == num then return mid else if list[mid] > num then r = mid - 1; f = bsearch(list, num, l, r); return f else l = mid + 1; f = bsearch(list, num, l, r); return f endif endif else return -1 endif}; f = bsearch([1,2,3, 5, 6, 8], 6, 0, 5); return f")))

;(define my-lexer (lex-this simple-lexer (open-input-string "a = [null, null, null] == null; return a")))
;(define my-lexer (lex-this simple-lexer (open-input-string "a = 2; return a")))
;(let ((parser-res (simple-parser my-lexer))) (displayln parser-res))
(let ((parser-res (simple-parser my-lexer))) (car (value-of parser-res (empty-env) (initial-proc-env))))
          
(provide (all-defined-out))
      
