(import (rnrs syntax-case (6)))

;;; constants

(define the-false-value (cons "false" "boolean"))

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

;; evaluate progn
(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env))
          (evaluate (car exps) env))
      '()))

(define (make-function args body env) ())

;; evaluate argument list
(define (evlis exps env)
  (if (pair? exps)
      (cons (evaluate (car exps) env)
            (evlis (cdr exps) env))
      '()))

(define (invoke fn args) ())

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
        ((begin) (eprogn (cdr e) env))
        ((set!) (update! (car (cdr exp)) env (evaluate (car (cdr (cdr exp))) env)))
        ((lambda) (make-function (car (cdr exp)) (cdr (cdr exp)) env))
        (else (invoke (evaluate (car exp) env)
                      (evlis (cdr exp) env))))))
