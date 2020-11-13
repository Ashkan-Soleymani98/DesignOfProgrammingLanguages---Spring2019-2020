#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(require (except-in eopl #%module-begin))

(define simple-lexer
  (lexer
       ["+" (token-plus)]
       ["-" (token-minus)]
       ["*" (token-mult)]
       ["/" (token-div)]
       ["==" (token-eq)]
       ["!=" (token-neq)]
       ["=" (token-seteq)]
       [">" (token-gt)]
       ["<" (token-lt)]
       [";" (token-semicolon)]
       ["," (token-comma)]
       ["[" (token-lbrack)]
       ["]" (token-rbrack)]
       ["(" (token-lpar)]
       [")" (token-rpar)]
       ["return" (token-ret)]
       ["while" (token-whilet)]
       ["do" (token-dot)]
       ["end" (token-ewhile)]
       ["if" (token-ift)]
       ["then" (token-thent)]
       ["else" (token-elset)]
       ["null" (token-NULL null)]
       ["endif" (token-eif)]
       [(:or "true" "false") (token-BOOL (if (equal? lexeme "true") #t #f))]
       [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z))) (token-VAR lexeme)]
       [(:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUM (string->number lexeme))]
       [(:: "'" (complement (:: any-string "'" any-string)) "'") (token-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
       [whitespace (simple-lexer input-port)]
       [(eof) (token-eof)]
       [(:+ (:or "@" "!" "#" "%" "^" "&" "~" "{" "}")) (eopl:error (string-append lexeme " is unknown!"))]
  
       )
  )

(define-tokens a (NUM STRING BOOL VAR NULL))
(define-empty-tokens b (eof plus minus mult div seteq eq neq gt lt semicolon comma lbrack rbrack lpar rpar ret whilet dot ewhile ift thent elset eif))

(define (error-thrower name value)
  (if (equal? value #f)
      (let ((sym (cond
                   [(equal? name 'eof) "end of file"]
                   [(equal? name 'plus) "+"]
                   [(equal? name 'minus) "-"]
                   [(equal? name 'div) "/"]
                   [(equal? name 'mult) "*"]
                   [(equal? name 'seteq) "="]
                   [(equal? name 'eq) "=="]
                   [(equal? name 'neq) "!="]
                   [(equal? name 'gt) ">"]
                   [(equal? name 'lt) "<"]
                   [(equal? name 'lpar) "("]
                   [(equal? name 'rpar) ")"]
                   [(equal? name 'lbrack) "["]
                   [(equal? name 'rbrack) "]"]
                   [(equal? name 'semicolon) ";"]
                   [(equal? name 'colon) ","]
                   [(equal? name 'semicolon) ";"]
                   [(equal? name 'ret) "return"]
                   [(equal? name 'whilet) "while"]
                   [(equal? name 'dot) "do"]
                   [(equal? name 'ewhile) "end"]
                   [(equal? name 'ift) "if"]
                   [(equal? name 'thent) "then"]
                   [(equal? name 'elset) "else"]
                   [(equal? name 'eif) "endif"]
                   )))
                 
                  

        (eopl:error (string-append "syntax error near " sym)))
      (cond
        [(number? value) (eopl:error (string-append "syntax error near " (number->string value)))]
        [(equal? name 'VAR) (eopl:error (string-append value " is not defined as a syntax token"))]
        [(equal? name 'NULL) (eopl:error "syntax error near null")]
        [(string? value) (eopl:error (string-append "syntax error near " value))]
        [(boolean? value) (if value (eopl:error (string-append "syntax error near " "true")) (eopl:error (string-append "syntax error near " "false")))]
        )
  ))
  

(define simple-parser
           (parser
            (start command)
            (end eof)
            (debug "debug.txt")
            (error (lambda (tok-ok? tok-name tok-value)
                     (displayln tok-ok?)
                     (displayln tok-name)
                     (displayln tok-value)
                     (displayln (error-thrower tok-name tok-value))

                     ))
            (tokens a b)
            (grammar
             
           [command ((unitcom) (list 'unitcommand $1)) ((unitcom semicolon command) (list 'multicommands $1 $3))]

           [unitcom
            ((whilecom) (list 'whilecom $1))
            ((ifcom) (list 'ifcom $1))
            ((assign) (list 'assigncom $1))
            ((return) (list 'returncom $1))
            ]
           
           [whilecom ((whilet exp dot command ewhile) (list 'while $2 $4))]
           
           [ifcom ((ift exp thent command elset command eif) (list 'if $2 $4 $6))]
           
           [assign ((VAR seteq exp) (list 'seteq $1 $3))]
           
           [return ((ret exp) (list 'return $2))]
           
           [exp
            ((aexp) (list 'aexp $1))
            ((aexp gt aexp) (list 'greater? $1 $3))
            ((aexp lt aexp) (list 'less? $1 $3))
            ((aexp eq aexp) (list 'eqs? $1 $3))
            ((aexp neq aexp) (list 'noteqs? $1 $3))
            ]
           
           [aexp
            ((bexp) (list 'bexp $1))
            ((bexp minus aexp) (list 'subtract $1 $3))
            ((bexp plus aexp) (list 'add $1 $3))
            ]
           
           [bexp
            ((cexp) (list 'cexp $1))
            ((cexp mult bexp) (list 'mult $1 $3))
            ((cexp div bexp) (list 'div $1 $3))
            ]
      
            [cexp
            ((minus cexp) (list 'minus $2))
            ((lpar exp rpar) (list 'expinpar $2))
            ((NUM) (list 'number $1))
            ((NULL) (list 'null $1))
            ((BOOL) (list 'bool $1))
            ((STRING) (list 'string $1))
            ((VAR) (list 'var $1))
            ((list) (list 'list $1))
            ((VAR listmem) (list 'cflist $1 $2))
            ]
            
            [list
             ((lbrack listValues rbrack) (list 'listvalues $2))
             ((lbrack rbrack) (list 'emptylist))
             ]
            
            [listValues
             ((exp) (list 'lval $1))
             ((exp comma listValues) (list 'lvals $1 $3))
             ]
            
            [listmem
             ((lbrack exp rbrack) (list 'listmem $2))
             ((lbrack exp rbrack listmem) (list 'listmems $2 $4))
             ]
           
             )))


(define lex-this (lambda (lexer input) (lambda () (lexer input))))
;(define my-lexer (lex-this simple-lexer (open-input-string "a = 2; b = 3")))
;(let ((parser-res (simple-parser my-lexer))) parser-res)

(provide (all-defined-out))






