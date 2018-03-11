#lang racket
(require (for-syntax syntax/parse syntax/stx))
 (require racket/unsafe/ops)
(provide (rename-out [app #%app][datum #%datum]
                     [add +][sub -][div /][mul *]
                     [strapd string-append]
                     [tnot not][lam lambda]))
(provide print-type: int string bool -> define-type)
(define int 'type-int)
(define string 'type-string)
(define bool 'type-bool)
(define ->internal (lambda l (error "ooh,you can't call me at runtime")))
(define-syntax ->
  (syntax-rules ()
    [(_ a ...) (#%app ->internal a ...)]))
(define-syntax (define-type stx)
  (syntax-case stx ()
    [(_ tname tvar) #'(define-syntax tname (make-val-transformer #'tvar))]
    ))
(define-for-syntax (type-app stx errorinf)
  (syntax-case stx ()
    [((-> a b) c) (type=e? #'a #'c) #'b]
    [((-> a b c ...) d e ...) (type=e? #'a #'d)
                              (type-app #'((-> b c ...) e ...) errorinf)]
    [others (raise-syntax-error 'type-system-error
                                "error when apply the arguements to function"
                                errorinf)]))
(define-for-syntax (type=e? x y) (type=? (local-expand x (syntax-local-context) '())
                                         (local-expand y (syntax-local-context) '())))
(define-for-syntax (type=? x y)
  (or (and (identifier? x) (identifier? y) (free-identifier=? x y))
      (and (stx-null? x) (stx-null? y))
      (and (type=? (stx-car x) (stx-car y))
           (type=? (stx-cdr x) (stx-cdr y)))))
(define-syntax (app stx)
  (syntax-case stx ()
    [(_ func . args) (let ([types #`(#,@(map
                                   (lambda (x)
                                     (let ([t (syntax-property (local-expand x
                                                                (syntax-local-context)
                                                                '())
 'type)])
                                       (if t t (raise-syntax-error 'type-system-error
                                                                   "missing type:"
                                                                   x))))
                                      (syntax->list #'(func . args))))])
                       
                       (syntax-property #'(#%app func . args) 'type
                                        (type-app types #'(#%app func . args))))]))
(define-syntax (datum stx)
  (syntax-parse stx
    [(d . c:integer) (syntax-property #'(#%datum . c)
                                  'type
                                  #'int)]
    [(d . c:boolean) (syntax-property #'(#%datum . c)
                                   'type
                                   #'bool)]
    [(d . c:str) (syntax-property #'(#%datum . c)
                                  'type
                                  #'string)]
    [(d . any) (raise-syntax-error 'type-system-error "Don't Support this type." #'any)]
     ))
(define-syntax (lam stx)
  (syntax-case stx ()
    [(_ ([var : types] ...) body)
      (let* ([body (local-expand #'(lambda (var ...)
                        (let-syntax ([var (make-val-transformer (syntax-property #'var 'type #'types))] ...)
                          body))
                    (syntax-local-context) '())]
            [bodyt (syntax-case body ()
                     [(_ args (letvalues () (letvalues2 () e)))
                      (syntax-property #'e 'type)]
                     [(_ (lam args (letvalues () (letvalues2 () e))))
                      (syntax-property #'e 'type)]
                     )]
            )
        (if bodyt
            (syntax-property body 'type (gen-type #'(types ...) bodyt))
            (raise-syntax-error 'type-system-error
                          "the body of lambda doesn;t have a type"
                          #'(lambda ([var : types] ...) body)))
        )]))
(define-syntax (print-type: stx)
  (syntax-case stx ()
    [(_ e) #`(begin
               (display '#,(syntax-property (local-expand #'e (syntax-local-context) '()) 'type))
               (newline))]))
(define-for-syntax (gen-type pa f)
  (syntax-case pa ()
    [(a ...) #`(-> a ... #,f)]
    ))
              
(define-for-syntax (make-val-transformer val)
  (lambda (stx)
    (syntax-case stx ()
      [_ (identifier? stx) val]
      [(_ . args) #`(app #,val . args)])))
(define-syntax add (make-val-transformer (syntax-property #'(lambda (x y) (unsafe-fx+ x y)) 'type #'(-> int int int))))
(define-syntax sub (make-val-transformer (syntax-property #'(lambda (x y) (unsafe-fx- x y)) 'type #'(-> int int int))))
(define-syntax mul (make-val-transformer (syntax-property #'(lambda (x y) (unsafe-fx* x y)) 'type #'(-> int int int))))
(define-syntax div (make-val-transformer (syntax-property #'(lambda (x y) (unsafe-fx/ x y)) 'type #'(-> int int int))))
(define-syntax strapd (make-val-transformer (syntax-property #'(lambda (x y) (string-append x y)) 'type #'(-> string string string))))
(define-syntax tnot (make-val-transformer (syntax-property #'not 'type #'(-> bool bool))))