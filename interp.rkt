#lang eopl

;; interpreter for the LET language. 


(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp

      ;; implement const-exp here
     (const-exp (num) (num-val num))

      ;; implement var-exp here
      (var-exp (var) (apply-env env var))

      ;; implement comp-exp here
      (comp-exp (exp1 op-exp exp3) (let ((val1 (value-of exp1 env))
                                   (val2 (value-of exp3 env))
                                   (op1 (value-of op-exp env)))
                                     (let ((num1 (expval->num val1))
                                           (num2 (expval->num val2 ))
                                           (oper (expval->string op1)))
                                       (bool-val
                                        (cond ((equal? oper "'greater'") (if (> num1 num2) #t #f))
                                           ((equal? oper "'equal'") (if (= num1 num2) #t #f))
                                           ((equal? oper "'less'") (if (< num1 num2) #t #f))
                                       )))))
                                     

                                   
                                   
      
      ;; implement op-exp here
      (op-exp (exp1 op-exp exp2)
              (let ((v1 (value-of exp1 env))
                    (o1 (value-of op-exp env))
                    (v2 (value-of exp2 env)))
                (let ((num1 (expval->num v1))
                      (operation1 (expval->string o1))
                      (num2 (expval->num v2)))
                  (num-val
                   (cond ((equal? operation1 "'add'") (+ num1 num2))
                         ((equal? operation1 "'mult'") (* num1 num2))
                         ((equal? operation1 "'div'") (/ num1 num2))
                         ((equal? operation1 "'sub'") (- num1 num2)))))))
                         
                


      
      ;; if-exp
      (if-exp (cond1 exp1 else-exp)
              (let ((val1 (value-of cond1 env)))      
                (if (expval->bool val1)
                    (value-of exp1 env)
                    (value-of else-exp env))))

      ;; implement my-cond-exp here
     (my-cond-exp (cond1 exp1 conds exps else-exp)
                   (if (null? exps)
                                   (if (expval->bool (value-of cond1 env)) (value-of exp1 env) (value-of else-exp env))
                                   (let ((val1 (value-of cond1 env)))
                                     (if (expval->bool val1)
                                         (value-of (my-cond-exp (car conds) (car exps) (cdr conds) (cdr exps) exp1) env)
                                         (value-of (my-cond-exp (car conds) (car exps) (cdr conds) (cdr exps) else-exp) env)))))

      
      ;; implement str-exp here
      (str-exp (str) (str-val str))

      ;; implement bool-exp here
      (bool-exp (bool) (bool-val (if (equal? bool "#true") #t #f)))
      
      ;; implement zero-exp here
      (zero?-exp (exp1) (bool-val (if (eqv? 0 (expval->num (value-of exp1 env))) #true #false)))

      ;; implement let-exp here
      (let-exp (var exp1 body)
               (let ((v1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var v1 env))))
      ;; implementation of calcNxEdge-exp
      (calcNxEdge-exp (exp1 exp2 exp3)
              (let ((v1 (value-of exp1 env))
                    (v2 (value-of exp2 env))
                    (v3 (value-of exp3 env)))
                (let ((a (expval->num v1))
                      (b (expval->num v2))
                      (pi 3.141592653589793238462643383279502884197169399375105820974944592307816406286)
                      (deg (expval->num v3)))
                  (num-val (sqrt (- (+ (* b b) (* a a)) (* 2 (* b (cos (/ (* deg pi) 180))) ) )))))))))


;(trace value-of)