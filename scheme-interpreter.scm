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
        `(cons (primop ,cons))
        `(car (primop ,car))
        `(cdr (primop ,cdr)))
  )

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
           (('begin . E)
            (let loop ((E E))
              (if (null? (cdr E))
                  (Eval (car E) env)
                  (begin
                    (Eval (car E) env)
                    (loop (cdr E))))))
           (('if c te fe)
            (if (Eval c env)
                (Eval te env)
                (Eval fe env)))
           (('quote e) e)
           (('lambda args . body)
            `(proc ,args ,(cons 'begin body) ,env))
           (('letrec B . C)
            (let* ((new-env (concat-to-env '() ;; dummy
                                           env))
                   (new-frame (map (match-lambda
                                    ((f ('lambda A . D))
                                     `(,f ,(Eval `(lambda ,A . ,D) new-env))))
                                   B)))
              (set-car! new-env new-frame)
              (Eval (cons 'begin C) new-env)))
           (('letrec2 B . C)
            (let* ((new-B (map (match-lambda
                                ((f ('lambda A . D))
                                 `(define ,f (lambda ,A . ,D))))
                               B))
                   (new-C (append new-B C)))
              (print new-C)
              (Eval (cons 'begin new-C) env)))
           (('delay dexp)
            `(lazy ,dexp ,env))
           (('force delayed)
            (let ((new-delayed (Eval delayed env)))
              (match new-delayed
                     (('lazy dexp denv)
                      (Eval dexp denv))
                     (else (print "argument of force must be lazy")))))
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

;; Environments are Circler List, So we must not display them.
(define output
  (lambda (ret)
    (match ret
           (('proc . _) (print "<procedure>"))
           (('lazy . _) (print "<lazy>"))
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
