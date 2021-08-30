#lang br/quicklang

;; ===== READER CODE =================================================================================

;; Define func read-syntax with input params `path` and `port`
;; NOTE: Path is not used for the project
(define (read-syntax path port)
  
  ;; Define anonymous func
  ;; - Reads `port` to list of strings `port->lines`
  ;; - Stores list of strings to `src-lines`
  (define src-lines (port->lines port))

  ;; Converts strings into datums
  ;; - format-datums creates a function list of strings and converts via format string
  ;;  (~a marks placement for argument string substitution)
  (define src-datums (format-datums '~a src-lines))

  ;; Inserts new forms into a return as the syntax object
  ;; - Moves values into a nested handle statement
  ;; - Uses a quasiquote (`) operator for string interpolation
  ;; - Uses an unquote-splicing (,@) operator for splicing in merged list
  (define module-datum `(module funstacker-mod "funstacker.rkt"
                          (handle-args ,@src-datums)))

  ;; Converts datum to syntax object and returns
  ;; - #f is placeholder context argument
  (datum->syntax #f module-datum))

;; Makes the read-syntax func publicly available
(provide read-syntax)

;; ===== EXPANDER CODE ===============================================================================

;; Define expander macro stacker-module-begin
;; - Takes in the pattern variable HANDLE-EXPR
(define-macro (stacker-module-begin HANDLE-EXPR ...)

  ;; Defines return syntax object
  ;; - #' captures the current lexical context (available variables)
  #'(#%module-begin
     ;; Handle all expressions on the stack
     HANDLE-EXPR ...
     ;; Display the first value of the stack, after all handling
     (display (first stack))))

;; Makes the macro syntax publicly available outside
(provide (rename-out [stacker-module-begin #%module-begin]))

;; Creates an empty list stack
(define stack empty)

;; Defines a pop func to remove the top argument
(define (pop-stack!)

  ;; Gets the first of the stack, assigning to arg
  (define arg (first stack))

  ;; Set the stack equal to the rest of the arguments
  (set! stack (rest stack))

  ;; Returns arg
  arg)

;; Defines a push func to add a new argument
(define (push-stack! arg)

  ;; Sets the stack to a construction of the arg and the stack
  (set! stack (cons arg stack)))

;; Defines our handler function
(define (handle [arg #f])

  ;; Introduce a conditional expression with two branches
  (cond
    
    ;; Branch 1 - If the argument is a number, push it to the stack
    [(number? arg) (push-stack! arg)]

    ;; Branch 2 - If the argument is addition or multiplication, calculate
    [(or (equal? + arg) (equal? - arg) (equal? * arg) (equal? / arg))
     
     ;; Pop the stack twice and calculate the expression in proper order
     (define popRHS (pop-stack!))
     (define popLHS (pop-stack!))
     (define op-result (arg popLHS popRHS))
     
     ;; Push this value back to the stack
     (push-stack! op-result)]))

;; Makes the handler function publicly available
(provide handle)

;; Provide bindings for + and * operations from br/quicklang
(provide + - * /)