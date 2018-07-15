(import (rnrs base (6)))
(import (rnrs exceptions (6)))
(import (rnrs syntax-case (6)))
(import (rnrs mutable-pairs (6)))

;;; constants

(define the-false-value (cons "false" "boolean"))
(define env.init '())
(define env.global env.init)

;;; utilities

(define (lookup id env)
  (if (pair? env)
      (if (eq? id (car (car env)))
          (cdr (car env))
          (lookup id (cdr env)))
      (raise "No such binding (lookup)")))

(define (update! id env value)
  (if (pair? env)
      (if (eq? (car (car env)) id)
          (begin (set-cdr! (car env) value)
                 value)
          (update! id (cdr env) value))
      (raise "No such binding (update!)")))

(define (extend env variables values)
  (cond ((pair? variables)
         (if (pair? values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (raise "Too less values")))
        ((null? variables)
          (if (null? values)
              env
              (raise "Too much values")))
         ((symbol? variables)
          (cons (cons variables values) env))))

;; evaluate progn
(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env))
          (evaluate (car exps) env))
      '()))

(define (make-function variables body env)
  (lambda (values)
    (eprogn body (extend env.init variables values))))

;; evaluate argument list
(define (evlis exps env)
  (if (pair? exps)
      (cons (evaluate (car exps) env)
            (evlis (cdr exps) env))
      '()))

(define (invoke fn args)
  (if (procedure? fn)
      (fn args)
      (riase "Not a function")))

;;; global environment

(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (begin (set! env.global (cons (cons 'name 'void) env.global))
            'name))
    ((definitial name value)
     (begin (set! env.global (cons (cons 'name value) env.global))
            'name))))

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name 
       (lambda (values)
         (if (= arity (length values))
             (apply value values)
             (raise "Incorrect arity")))))))

(definitial t #t)
(definitial f the-false-value)
(definitial nil '())

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive eq? eq? 2)
(defprimitive < < 2)

;;; evaluator

(define (evaluate exp env)
  (if (not (pair? exp))
      (cond ((symbol? exp) (lookup exp env))
            ((or (number? exp) (string? exp) (char? exp)
                 (boolean? exp) (vector? exp))
             exp)
            (else (raise "cannot evaluate")))
      (case (car exp)
        ((quote) (car (cdr exp)))
        ((if) (if (evaluate (car (cdr exp)) env)
                  (evaluate (car (cdr (cdr exp))) env)
                  (evaluate (car (cdr (cdr (cdr exp)))) env)))
        ((begin) (eprogn (cdr exp) env))
        ((set!) (update! (car (cdr exp)) env (evaluate (car (cdr (cdr exp))) env)))
        ((lambda) (make-function (car (cdr exp)) (cdr (cdr exp)) env))
        (else (invoke (evaluate (car exp) env)
                      (evlis (cdr exp) env))))))

;;; repl

(define (chap1-toplevel)
  (begin (display (evaluate (read) env.global))
         (display "\n"))
  (chap1-toplevel))
