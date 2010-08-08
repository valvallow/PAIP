;; PAIP excersise 3.1
;; let*Ž®‚Æ“™‰¿‚Èƒ‰ƒ€ƒ_Ž®‚ðŽ¦‚¹


(define-syntax let*->lambda
  (syntax-rules ()
    ((_ () body ...)
     (let ()
       body ...))
    ((_ ((var val)) body ...)
     (let ((var val))
       body ...))
    ((_ ((var1 val1)(var2 val2) ...) body ...)
     (let*->lambda ((var1 val1))
                   (let*->lambda ((var2 val2) ...)
                                 body ...)))))

(macroexpand '(let*->lambda ((a 10)(b a)(c (+ a b)))
                            (print a b c)))
;; (#<identifier user#let> ((a 10))
;;               (#<identifier user#let*->lambda> ((b a) (c (+ a b)))
;;                             (print a b c)))


(define-syntax let->lambda
  (syntax-rules ()
    ((_ () body ...)
     ((lambda ()
        body ...)))
    ((_ ((var val)) body ...)
     ((lambda (var)
        body ...) val))
    ((_ ((var1 val1)(var2 val2) ...) body ...)
     (let->lambda ((var1 val1))
                  (let->lambda ((var2 val2) ...)
                               body ...)))))

(macroexpand '(let->lambda ((a 10))
                           (print a)))
;; ((#<identifier user#lambda> (a)
;;                (print a)) 10)


(define-syntax let*->lambda
  (syntax-rules ()
    ((_ () body ...)
     (let->lambda ()
                  body ...))
    ((_ ((var val)) body ...)
     (let->lambda ((var val))
                  body ...))
    ((_ ((var1 val1)(var2 val2) ...) body ...)
     (let*->lambda ((var1 val1))
                   (let*->lambda ((var2 val2) ...)
                                 body ...)))))

(macroexpand '(let*->lambda ((a 10)(b a)(c (+ a b)))
                            (print a b c)))

;; ((#<identifier user#lambda> (a)
;;                (#<identifier user#let*->lambda> ((b a) (c (+ a b)))
;;                              (print a b c))) 10)
