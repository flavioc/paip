
(load "pattern-match")

(defun variable-p (exp)
  "Variables are the symbols M through Z."
  ;; put x,y,z first to find them a little faster
  (member exp '(x y z m n o p q r s t u v w)))

(defun binary-expr-p (x)
    (and (expr-p x) (= (length (expr-args x)) 2)))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
    (mapcar #'prefix->infix
            (if (binary-expr-p exp)
              (list (expr-lhs exp) (expr-op exp) (expr-rhs exp))
              exp))))

;; Define x+ and y+ as a sequence:
(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))

(defun not-numberp (x) (not (numberp x)))

;; Define n and m as numbers; s as a non-number:
(pat-match-abbrev 'n '(?is n numberp))
(pat-match-abbrev 'm '(?is m numberp))
(pat-match-abbrev 's '(?is s not-numberp))

(defun simp-rule (rule)
  "Transform a rule into proper format."
  (let ((expr (infix->prefix rule)))
    (mkexp (expand-pat-match-abbrev (expr-lhs expr))
           (expr-op expr) (expr-rhs expr))))

(defun rule-pattern (rule) (first rule))
(defun rule-response (rule) (second rule))

(defparameter *infix->prefix-rules*
  (mapcar #'expand-pat-match-abbrev
          '(((x+ = y+) (= x y))
            ((- x+)    (- x))
            ((+ x+)    (+ x))
            ((x+ + y+) (+ x y))
            ((x+ - y+) (- x y))
            ((d y+ / d x) (d y x))        ;*** New rule
            ((Int y+ d x) (int y x))      ;*** New rule
            ((x+ * y+) (* x y))
            ((x+ / y+) (/ x y))
            ((x+ ^ y+) (^ x y)))))

(defun infix->prefix (exp)
  "Translate an infix expression into prefix notation."
  ;; Note we cannot do implicit multiplication in this system
  (cond ((atom exp) exp)
        ((= (length exp) 1) (infix->prefix (first exp)))
        ((rule-based-translator exp *infix->prefix-rules*
                                :rule-if #'rule-pattern :rule-then #'rule-response
                                :action
                                #'(lambda (bindings response)
                                    (sublis (mapcar
                                              #'(lambda (pair)
                                                  (cons (first pair)
                                                        (infix->prefix (rest pair))))
                                              bindings)
                                            response))))
        ((symbolp (first exp))
         (list (first exp) (infix->prefix (rest exp))))
        (t (error "Illegal exp"))))

(defun expr-variable-p (expr)
  "Variables are the symbols M through Z."
  ;; put x,y,z first to find them a little faster
  (member expr '(x y z m n o p q r s t u v w)))


(defstruct (rule (:type list)) pattern response)
(defstruct (expr (:type list)
                 (:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun expr-p (x) (consp x))
(defun expr-args (x) (rest x))

(defparameter *simplification-rules* nil)

(setf *simplification-rules* (mapcar #'simp-rule '(
                                                   (x + 0  = x)
                                                   (0 + x  = x)
                                                   (x + x  = 2 * x)
                                                   (x - 0  = x)
                                                   (0 - x  = - x)
                                                   (x - x  = 0)
                                                   (- - x  = x)
                                                   (x * 1  = x)
                                                   (1 * x  = x)
                                                   (x * 0  = 0)
                                                   (0 * x  = 0)
                                                   (x * x  = x ^ 2)
                                                   (x / 0  = undefined)
                                                   (0 / x  = 0)
                                                   (x / 1  = x)
                                                   (x / x  = 1)
                                                   (0 ^ 0  = undefined)
                                                   (x ^ 0  = 1)
                                                   (0 ^ x  = 0)
                                                   (1 ^ x  = 1)
                                                   (x ^ 1  = x)
                                                   (x ^ -1 = 1 / x)
                                                   (x * (y / x) = y)
                                                   ((y / x) * x = y)
                                                   ((y * x) / x = y)
                                                   ((x * y) / x = y)
                                                   (x + - x = 0)
                                                   ((- x) + x = 0)
                                                   (x + y - x = y)
                                                   )))


(defun ^ (x y) "Exponentiation" (expt x y))

(defun simplifier ()
  "Read a mathematical expression, simplify it, and print the result."
  (loop
    (print 'simplifier>)
    (force-output t)
    (print (simp (read)))))

(defun simp (inf) (prefix->infix (simplify (infix->prefix inf))))

(defun simplify (exp) 
  "Simplify an expression by first simplifying its components."
  (if (atom exp) exp
    (simplify-expr (mapcar #'simplify exp))))

(defun simplify-expr (exp)
  "Simplify using a rule, or by doing arithmetic."
  (cond ((rule-based-translator exp *simplification-rules*
                                :rule-if #'expr-lhs :rule-then #'expr-rhs
                                :action #'(lambda (bindings response)
                                            (simplify (sublis bindings response)))))
        ((evaluable exp) (eval exp))
        (t exp)))

(defun evaluable (exp)
  "Is this an arithmetic expression that can be evaluated?"
  (and (every #'numberp (expr-args exp))
       (or (member (expr-op exp) '(+ - * /))
           (and (eq (expr-op exp) '^)
                (integerp (second (expr-args exp)))))))

(setf *simplification-rules* 
      (append *simplification-rules* (mapcar #'simp-rule
                                             '((s * n = n * s)
                                               (n * (m * x) = (n * m) * x)
                                               (x * (n * y) = n * (x * y))
                                               ((n * x) * y = n * (x * y))
                                               (n + s = s + n)
                                               ((x + m) + n = x + n + m)
                                               (x + (y + n) = (x + y) + n)
                                               ((x + n) + y = (x + y) + n)))))

