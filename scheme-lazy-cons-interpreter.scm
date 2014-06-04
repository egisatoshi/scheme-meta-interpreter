#!/usr/bin/gosh
;;;
;;; Scheme Interpreter 2008/12/23(Tue)
;;;

(use util.match)
(use srfi-1)

;;
;; Environment
;;
(define find-from-frame
  (lambda (key frame)
    (let ((pair (assq key frame)))
      (if (eq? pair #f)
          'not-found
          (cadr pair)))))

(define find-from-env
  (lambda (key env)
    (if (null? env)
        key
        (let ((top-frame (car env))
              (restenv (cdr env)))
          (let ((val (find-from-frame key top-frame)))
            (if (eq? val 'not-found)
                (find-from-env key restenv)
                val))))))

(define concat-to-env
  (lambda (frame env)
    (cons frame env)))

(define define-value!
  (lambda (var val env)
    (let ((top-frame (car env)))
      (let ((new-top-frame (cons `(,var ,val) top-frame)))
        (set-car! env new-top-frame)))))

(define set-value!
  (lambda (var val env)
    'not-implemented-yet))

;;
;; Utilitiy
;;
(define base-frame
  (list `(+ (primop ,+))
        `(- (primop ,-))
        `(* (primop ,*))
        `(/ (primop ,/))
        `(modulo (primop ,modulo))
        `(not (primop ,not))
        `(= (primop ,=))
  ))

(define init-env
  (list base-frame))

;;
;; Evaluator
;;
(define Eval
  (lambda (exp env)
    (match exp
           ((? not-pair? term)
            (if (number? term)
                term
                (find-from-env term env)))
           (('set! var vexp)
            (let ((val (Eval vexp env)))
              (set-value! var val env)
              'ok))
           (('define var vexp)
            (let ((val (Eval vexp env)))
              (define-value! var val env)
              'ok))
           (('if c te fe)
            (if (Eval c env)
                (Eval te env)
                (Eval fe env)))
           (('lambda args body)
            `(proc ,args ,body ,env))
           (('delay dexp)
            `(lazy ,dexp ,env))
           (('force delayed)
            (let ((new-delayed (Eval delayed env)))
              (match new-delayed
                     (('lazy dexp denv)
                      (Eval dexp denv))
                     (else (print "argument of force must be lazy")))))
           (('cons ae de) `(pair ,ae ,de ,env))
           (('car pair)
            (let ((new-pair (Eval pair env)))
              (match new-pair
                     (('pair ae _ pair-env)
                      (Eval ae pair-env))
                     (else (print "args of car must be pair")))))
           (('cdr pair)
            (let ((new-pair (Eval pair env)))
              (match new-pair
                     (('pair _ de pair-env)
                      (Eval de pair-env))
                     (else (print "args of cdr must be pair")))))
           ((op . args)
            (let ((new-op (Eval op env))
                  (new-args (map (lambda (e) (Eval e env)) args)))
              (match new-op
                     (('primop primop)
                      (apply primop new-args))
                     (('proc func-args func-body func-env)
                      (let ((new-env (concat-to-env (zip func-args new-args) func-env)))
                        (Eval func-body new-env)))
                     (else (display "invalid function: ")
                           (print new-op))))))))

;; Environments are Circler List, so we must not display them.
(define output
  (lambda (ret)
    (match ret
           (('proc . _) (print "<procedure>"))
           (('lazy . _) (print "<lazy>"))
           (('pair . _) (print "<pair>"))
           (else (print ret)))))
    
(define interpret
  (lambda ()
    (let ((env init-env))
      (let loop ()
        (display "> ") (flush)
        (let ((exp (read)))
          (if (eof-object? exp)
              (begin (newline) (print "byebye"))
              (let ((ret (Eval exp env)))
                (output ret)
                (loop))))))))

(define main
  (lambda (args)
    (interpret)))

;; test

;(define make-integer-list-from
;  (lambda (n)
;    (cons n (make-integer-list-from (+ n 1)))))
;
;(define integers (make-integer-list-from 0))
