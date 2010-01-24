(load "pattern-match")

(defstruct (rule (:type list)) pattern response)

(defstruct (expr (:type list)
                (:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun expr-p (x) (consp x))
(defun expr-args (x) (rest x))

(pat-match-abbrev '?x* '(?* ?x))
(pat-match-abbrev '?y* '(?* ?y))

(defparameter *student-rules* (mapcar #'expand-pat-match-abbrev
                                      '(((?x* |.|)                  ?x)
                                        ((?x* |.| ?y*)          (?x ?y))
                                        ((if ?x* |,| then ?y*)  (?x ?y))
                                        ((if ?x* then ?y*)      (?x ?y))
                                        ((if ?x* |,| ?y*)       (?x ?y))
                                        ((?x* |,| and ?y*)      (?x ?y))
                                        ((find ?x* and ?y*)     ((= to-find-1 ?x) (= to-find-2 ?y)))
                                        ((find ?x*)             (= to-find ?x))
                                        ((?x* equals ?y*)       (= ?x ?y))
                                        ((?x* same as ?y*)      (= ?x ?y))
                                        ((?x* = ?y*)            (= ?x ?y))
                                        ((?x* is equal to ?y*)  (= ?x ?y))
                                        ((?x* is ?y*)           (= ?x ?y))
                                        ((?x* - ?y*)            (- ?x ?y))
                                        ((?x* minus ?y*)        (- ?x ?y))
                                        ((difference between ?x* and ?y*)  (- ?y ?x))
                                        ((difference ?x* and ?y*)          (- ?y ?x))
                                        ((?x* + ?y*)            (+ ?x ?y))
                                        ((?x* plus ?y*)         (+ ?x ?y))
                                        ((sum ?x* and ?y*)      (+ ?x ?y))
                                        ((product ?x* and ?y*)  (* ?x ?y))
                                        ((?x* * ?y*)            (* ?x ?y))
                                        ((?x* times ?y*)        (* ?x ?y))
                                        ((?x* / ?y*)            (/ ?x ?y))
                                        ((?x* per ?y*)          (/ ?x ?y))
                                        ((?x* divided by ?y*)   (/ ?x ?y))
                                        ((half ?x*)             (/ ?x 2))
                                        ((one half ?x*)         (/ ?x 2))
                                        ((twice ?x*)            (* 2 ?x))
                                        ((square ?x*)           (* ?x ?x))
                                        ((?x* % less than ?y*)  (* ?y (/ (- 100 ?x) 100)))
                                        ((?x* % more than ?y*)  (* ?y (/ (+ 100 ?x) 100)))
                                        ((?x* % ?y*)            (* (/ ?x 100) ?y)))))

(defun student (words)
  "Solve certain Algebra Word Problems."
  (solve-equations
    (create-list-of-equations
      (translate-to-expression (remove-if #'noise-word-p words)))))

(defun translate-to-expression (words)
  "Translate an English phrase into an equation or expression."
  (or (rule-based-translator
        words *student-rules*
        :rule-if #'rule-pattern :rule-then 'rule-response
        :action #'(lambda (bindings response)
                    (sublis (mapcar #'translate-pair bindings)
                            response)))
      (make-variable words)))

(defun translate-pair (pair)
  "Translate the value part of the pair into an equation or expression."
  (cons (binding-var pair)
        (translate-to-expression (binding-val pair))))

(defun create-list-of-equations (expr)
  "Separate out equations embedded in nested parens."
  (cond ((null expr) nil)
        ((atom (first expr)) (list expr))
        (t (append (create-list-of-equations (first expr))
                   (create-list-of-equations (rest expr))))))

(defun make-variable (words)
  "Create a variable name based on the given list of words"
  ;; The list of words will already have noise words removed
  (first words))

(defun noise-word-p (word)
  "Is this a low-content word that can be safely ignored?"
  (member word '(a an the this number of $)))

(defun solve-equations (equations)
  "Print the equations and their solution"
  (print-equations "The equations to be solved are:" equations)
  (print-equations "The solution is: " (solve equations nil)))

(defun solve (equations known)
  "Solve a system of equations by constraint propagation."
  ;; Try to solve for one equation, and substitute its value into
  ;; the others. If that doesn't work, return what is known
  (or (some #'(lambda (equation)
                (let ((x (one-unknown equation)))
                  (when x
                    (let ((answer (solve-arithmetic
                                    (isolate equation x))))
                      (solve (subst (expr-rhs answer) (expr-lhs answer)
                                    (remove equation equations))
                             (cons answer known))))))
            equations)
      (let ((eqs (find-linear-pair equations)))
        (when eqs
          (let* ((eq1 (first eqs))
                 (eq2 (second eqs))
                 (vars (third eqs))
                 (var1 (first vars))
                 (var2 (second vars))
                 (isolated-eq1 (isolate eq1 var1))
                 (isolated-eq2 (isolate eq2 var2))
                 (new-eq2 (mkexp var2 '= (subst (expr-rhs isolated-eq1) var1 (expr-rhs isolated-eq2))))
                 (new-eq1 (mkexp var1 '= (subst (expr-rhs isolated-eq2) var2 (expr-rhs isolated-eq1))))
                 (new-equations1 (remove eq1 (remove eq2 equations)))
                 (new-equations (cons new-eq1 (cons new-eq2 new-equations1))))
            (warn "~a" new-equations)
            (solve new-equations known))))
      known))

(defun find-linear-pair (equations)
  (dolist (eq1 equations)
    (let ((vars (find-vars eq1)))
      (when (= (length vars) 2)
        (let ((eq2 (find-other-pair vars (remove eq1 equations))))
          (when eq2 
            (return-from find-linear-pair (list eq1 eq2 vars))))))))

(defun find-other-pair (vars rest-equations)
  (dolist (eq2 rest-equations)
    (let ((vars2 (find-vars eq2)))
      (when (null (set-difference vars vars2))
        (return-from find-other-pair eq2)))))

(defun find-vars (equation)
  (cond
    ((symbolp equation) (list equation))
    ((numberp equation) '())
    ((expr-p equation)
     (union
       (find-vars (expr-lhs equation))
       (find-vars (expr-rhs equation))))))


(defun isolate (e x)
  "Isolate the lone x in e on the left-hand side of e."
  ;; This assumes there is exactly one x in e,
  ;; and that e is an equation.
  (cond ((eq (expr-lhs e) x)
         ;; Case I: X = A -> X = n
         e)
        ((in-exp x (expr-rhs e))
         ;; Case II: A = f(X) -> f(X) = A
         (isolate (mkexp (expr-rhs e) '= (expr-lhs e)) x))
        ((in-exp x (expr-lhs (expr-lhs e)))
         ;; Case III: f(X)*A = B -> f(X) = B/A
         (isolate (mkexp (expr-lhs (expr-lhs e)) '=
                         (mkexp (expr-rhs e)
                                (inverse-op (expr-op (expr-lhs e)))
                                (expr-rhs (expr-lhs e)))) x))
        ((commutative-p (expr-op (expr-lhs e)))
         ;; Case IV: A * f(X) = B -> f(X) = B/A
         (isolate (mkexp (expr-rhs (expr-lhs e)) '=
                         (mkexp (expr-rhs e)
                                (inverse-op (expr-op (expr-lhs e)))
                                (expr-lhs (expr-lhs e)))) x))
        (t ;; Case V: A/f(X) = B -> f(X) = A/B
          (isolate (mkexp (expr-rhs (expr-lhs e)) '=
                          (mkexp (expr-lhs (expr-lhs e))
                                 (expr-op (expr-lhs e))
                                 (expr-rhs e))) x))))

(defun print-equations (header equations)
  "Print a list of equations."
  (let ((norm (mapcar #'prefix->infix equations)))
    (princ header)
    (terpri)
    (dolist (el norm)
      (princ " ")
      (dolist (el2 el)
        (princ " ")
        (princ el2))
      (terpri))
    (terpri)))

(defconstant operators-and-inverses
             '((+ -) (- +) (* /) (/ *) (= =)))

(defun inverse-op (op)
  (second (assoc op operators-and-inverses)))

(defun unknown-p (expr)
  (symbolp expr))

(defun in-exp (x expr)
  "True if x appears anywhere in expr"
  (or (eq x expr)
      (and (expr-p expr)
           (or (in-exp x (expr-lhs expr))
               (in-exp x (expr-rhs expr))))))

(defun no-unknown (expr)
  "Returns true if there are no unknowns in expr."
  (cond ((unknown-p expr) nil)
        ((atom expr) t)
        ((no-unknown (expr-lhs expr)) (no-unknown (expr-rhs expr)))
        (t nil)))

(defun one-unknown (expr)
  "Returns the single unknown in expr, if there is exactly one."
  (cond ((unknown-p expr) expr)
        ((atom expr) nil)
        ((no-unknown (expr-lhs expr)) (one-unknown (expr-rhs expr)))
        ((no-unknown (expr-rhs expr)) (one-unknown (expr-lhs expr)))
        (t nil)))

(defun commutative-p (op)
  "Is operator commutative?"
  (member op '(+ * =)))

(defun solve-arithmetic (equation)
  "Do the arithmetic for the right-hand side."
  ;; This assumes that the right-hand side is in the right form.
  (mkexp (expr-lhs equation) '= (eval (expr-rhs equation))))

(defun binary-expr-p (x)
  (and (expr-p x) (= (length (expr-args x)) 2)))

(defun prefix->infix (expr)
  "Translate prefix to infix expressions."
  (if (atom expr) expr
    (mapcar #'prefix->infix
            (if (binary-expr-p expr)
              (list (expr-lhs expr) (expr-op expr) (expr-rhs expr))
              expr))))

(defparameter *prob1*
  '(If the number of customers Tom gets is twice the square of 20 % of the number of advertisements he runs |,|
       and the number of advertisements is 45 |,|
       then what is the number customers Tom gets ?))

(defparameter *prob2*
  '(Fran's age divided by Robin's height is one half Kelly's IQ |.|
           Kelly's IQ minus 80 is Robin's height |.|
           If Robin is 4 feet tall |,| how old is Fran ?))

(defparameter *eq1*
  '(= 100 (+ OVERHEAD (* RUNNING 40))))
(defparameter *eq2*
  '(= OVERHEAD (* 10 RUNNING)))
(defparameter *eqs* (list *eq1* *eq2*))
