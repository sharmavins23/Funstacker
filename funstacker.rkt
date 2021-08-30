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
(define-macro (funstacker-module-begin HANDLE-ARGS-EXPR)

  ;; Defines return syntax object
  ;; - #' captures the current lexical context (available variables)
  #'(#%module-begin
     ;; Display the argument, after all nested handling
     (display (first HANDLE-ARGS-EXPR))))

;; Makes the macro syntax publicly available outside
(provide (rename-out [funstacker-module-begin #%module-begin]))

;; Defines a handler function that emulates stack functionality
;; - Defines `args` as a rest (optional all-enclosing) argument
(define (handle-args . args)

  ;; Iterate over the list of values
  (for/fold ([stack-acc empty])

            ;; Iterate over the arguments
            ([arg (in-list args)]
             #:unless (void? arg))

    ;; Handles argument between operators or numbers
    (cond

      ;; If a number, put on stack
      [(number? arg) (cons arg stack-acc)]

      ;; Otherwise, handle it by evaluating the expression
      [(or (equal? + arg) (equal? - arg) (equal? * arg) (equal? / arg))
       (define op-result
         ;; Ordering for specific division/subtraction functionality
         (arg (second stack-acc) (first stack-acc)))

       ;; Explicitly drop two items from the stack, then add it again
       (cons op-result (drop stack-acc 2))])))

;; Makes the handler function publicly available
(provide handle-args)

;; Provide bindings for + and * operations from br/quicklang
(provide + - * /)