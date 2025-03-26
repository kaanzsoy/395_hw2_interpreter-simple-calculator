;; required modules
(use-modules (ice-9 rdelim))

;; token structure
(define (make-token type value)
  (cons type value))

(define (token-type token)
  (car token))

(define (token-value token)
  (cdr token))

;; converts input to a list of tokens
(define (tokenize input)
  (let loop ((chars (string->list input))
             (tokens '())
             (num-str ""))
    (cond
      ((null? chars)
       (if (string-null? num-str)
           (reverse tokens)
           (reverse (cons (make-token 'number (string->number num-str)) tokens))))
      
      ((char-whitespace? (car chars))
       (if (string-null? num-str)
           (loop (cdr chars) tokens "")
           (loop (cdr chars) 
                 (cons (make-token 'number (string->number num-str)) tokens)
                 "")))
      
      ((char-numeric? (car chars))
       (loop (cdr chars) tokens (string-append num-str (string (car chars)))))
      
      (else
        (let ((token
               (case (car chars)
                 ((#\+) (make-token 'plus '+))
                 ((#\-) (make-token 'minus '-))
                 ((#\*) (make-token 'multiply '*))
                 ((#\/) (make-token 'divide '/))
                 ((#\^) (make-token 'power '^))
                 ((#\() (make-token 'lparen 'lparen))
                 ((#\)) (make-token 'rparen 'rparen))
                 (else (error "Invalid character")))))
          (if (string-null? num-str)
              (loop (cdr chars) (cons token tokens) "")
              (loop (cdr chars)
                    (cons token (cons (make-token 'number (string->number num-str)) tokens))
                    "")))))))

;; convert tokens to nested expression structure
(define (parse tokens)
  (let ((current 0))
    
    (define (peek)
      (if (< current (length tokens))
          (list-ref tokens current)
          #f))
    
    (define (advance)
      (set! current (+ current 1)))
    
;; addition and subtraction
    (define (expression)
      (let ((left (term)))
        (let loop ((result left))
          (let ((token (peek)))
            (if (and token (memq (token-type token) '(plus minus)))
                (begin
                  (advance)
                  (loop (list (token-type token) result (term))))
                result)))))
    
;; multipl, divide, pow
    (define (term)
      (let ((left (factor)))
        (let loop ((result left))
          (let ((token (peek)))
            (if (and token (memq (token-type token) '(multiply divide power)))
                (begin
                  (advance)
                  (loop (list (token-type token) result (factor))))
                result)))))

;; numbers, paranthesized things  
    (define (factor)
      (let ((token (peek)))
        (if (not token)
            (error "Unexpected end of input")
            (begin
              (advance)
              (case (token-type token)
                ((number) (token-value token))
                ((lparen)
                 (let ((expr (expression)))
                   (if (and (peek)
                            (eq? (token-type (peek)) 'rparen))
                       (begin
                         (advance)
                         expr)
                       (error "Expected closing parenthesis"))))
                (else (error "Unexpected token")))))))
    
    (expression)))

(define (evaluate expr)
  (cond
    ((number? expr) expr)
    ((list? expr)
     (let ((op (car expr))
           (left (evaluate (cadr expr)))
           (right (evaluate (caddr expr))))
       (case op
         ((plus) (+ left right))
         ((minus) (- left right))
         ((multiply) (* left right))
         ((divide) 
          (if (zero? right)
              (error "Division by zero")
              (/ left right)))
         ((power) (expt left right))
         (else (error "Unknown operator")))))
    (else (error "Invalid expression"))))

;; REPL
(define (repl)
  (display "Input:\n")
  (let ((input (read-line)))
    (if (or (eof-object? input) (string=? input ""))
        (display "Exiting calculator!\n")
        (begin
          (display "Output: ")
          (catch #t
            (lambda ()
              (let* ((tokens (tokenize input))
                     (expr (parse tokens))
                     (result (evaluate expr)))
                (display result)
                (newline)))
            (lambda (key . args)
              (display "Error: ")
              (display (if (null? args) key (car args)))
              (newline)))
          (repl)))))

;; start program
(repl)
