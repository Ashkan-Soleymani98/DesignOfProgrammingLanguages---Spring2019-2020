#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)


(define simple-math-lexer
  (lexer
       ("+" (token-plus))
       ("-" (token-minus))
       ("*" (token-mult))
       ("/" (token-div))
       ("==" (token-eq))
       ("=" (token-seteq))
       ("!=" (token-neq))
       (">" (token-gt))
       ("<" (token-lt))
       (";" (token-semicolon))
       ("," (token-comma))
       ("[" (token-lbrack))
       ("]" (token-rbrack))
       ("(" (token-lpar))
       (")" (token-rpar))
       ("return" (token-ret))
       ("while" (token-whilet))
       ("do" (token-dot))
       ("end" (token-ewhile))
       ("if" (token-ift))
       ("then" (token-thent))
       ("else" (token-elset))
       ("null" (token-NULL))
       ("endif" (token-eif))
       ((:or "true" "false") (token-BOOL (if (equal? lexeme "true") #t #f)))
       ((:+ (:or (char-range #\a #\z) (char-range #\A #\Z))) (token-VAR lexeme))
       ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUM (string->number lexeme)))
       ((:: "'" (complement (:: any-string "'" any-string)) "'") (token-STRING lexeme))
       (whitespace (simple-math-lexer input-port))
       ((eof) (token-eof))
       )
  )

(define-tokens a (NUM STRING BOOL NULL VAR))
(define-empty-tokens b (eof plus minus mult div seteq eq neq gt lt semicolon comma lbrack rbrack lpar rpar ret whilet dot ewhile ift thent elset eif))

(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "' hello ' == ' s '")))

(define (print-lex  l)
  (cond
    [(equal? l 'eof) (print l)]
    [else (print l)
          (display "\n")
          (print-lex (my-lexer))]))


(print-lex (my-lexer))