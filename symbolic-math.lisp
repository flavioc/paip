
(load "pattern-match")
(load "starts")

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

(setf *simplification-rules* 
      (append *simplification-rules* (mapcar #'simp-rule '(
                                                           (log 1         = 0)
                                                           (log 0         = undefined)
                                                           (log e         = 1)
                                                           (sin 0         = 0)
                                                           (sin pi        = 0)
                                                           (cos 0         = 1)
                                                           (cos pi        = -1)
                                                           (sin(pi / 2)   = 1)
                                                           (cos(pi / 2)   = 0)
                                                           (log (e ^ x)   = x)
                                                           (e ^ (log x)   = x)
                                                           ((x ^ y) * (x ^ z) = x ^ (y + z))
                                                           ((x ^ y) / (x ^ z) = x ^ (y - z))
                                                           (log x + log y = log(x * y))
                                                           (log x - log y = log(x / y))
                                                           ((sin x) ^ 2 + (cos x) ^ 2 = 1)
                                                           ))))


(setf *simplification-rules* 
      (append *simplification-rules* (mapcar #'simp-rule '(
                                                           (d x / d x       = 1)
                                                           (d (u + v) / d x = (d u / d x) + (d v / d x))
                                                           (d (u - v) / d x = (d u / d x) - (d v / d x))
                                                           (d (- u) / d x   = - (d u / d x))
                                                           (d (u * v) / d x = u * (d v / d x) + v * (d u / d x))
                                                           (d (u / v) / d x = (v * (d u / d x) - u * (d v / d x)) 
                                                              / v ^ 2) ; [This corrects an error in the first printing]
                                                           (d (u ^ n) / d x = n * u ^ (n - 1) * (d u / d x))
                                                           (d (u ^ v) / d x = v * u ^ (v - 1) * (d u / d x)
                                                              + u ^ v * (log u) * (d v / d x))
                                                           (d (log u) / d x = (d u / d x) / u)
                                                           (d (sin u) / d x = (cos u) * (d u / d x))
                                                           (d (cos u) / d x = - (sin u) * (d u / d x))
                                                           (d (e ^ u) / d x = (e ^ u) * (d u / d x))
                                                           (d u / d x       = 0)))))

(defun factorize (exp)
  "Return a list of the factors of exp^n,
  where each factor is of the form (^ y n)."
  (let ((factors nil)
        (constant 1))
    (labels
      ((fac (x n)
         (cond
           ((numberp x)
            (setf constant (* constant (expt x n))))
           ((starts-with x '*)
            (fac (expr-lhs x) n)
            (fac (expr-rhs x) n))
           ((starts-with x '/)
            (fac (expr-lhs x) n)
            (fac (expr-rhs x) (- n)))
           ((and (starts-with x '-) (length=1 (expr-args x)))
            (setf constant (- constant))
            (fac (expr-lhs x) n))
           ((and (starts-with x '^) (numberp (expr-rhs x)))
            (fac (expr-lhs x) (* n (expr-rhs x))))
           (t (let ((factor (find x factors :key #'expr-lhs
                                  :test #'equal)))
                (if factor
                    (incf (expr-rhs factor) n)
                    (push `(^ ,x ,n) factors)))))))
      ;; Body of factorize:
      (fac exp 1)
      (case constant
        (0 '((^ 0 1)))
        (1 factors)
        (t `((^ ,constant 1) .,factors))))))


(defun simp-fn (op) (get op 'simp-fn))
(defun set-simp-fn (op fn) (setf (get op 'simp-fn) fn))

(defun simplify-expr (expr)
  "Simplify using a rule, or by doing arithmetic,
  or by using the simp function supplied for this operator."
  (cond ((simplify-by-fn expr))
        ((rule-based-translator expr *simplification-rules*
                                :rule-if #'expr-lhs :rule-then #'expr-rhs
                                :action #'(lambda (bindings response)
                                            (simplify (sublis bindings response)))))
        ((evaluable expr) (eval expr))
        (t expr)))

(defun simplify-by-fn (expr)
  "If there is a simplification fn for this expr,
  and if applying it gives a non-null result,
  then simplify the result and return that."
  (let* ((fn (simp-fn (expr-op expr)))
         (result (if fn (funcall fn expr))))
    (if (null result)
      nil
      (simplify result))))


(defun unfactorize (factors)
  "Convert a list of factors back into prefix form."
  (cond ((null factors) 1)
        ((length=1 factors) (first factors))
        (t `(* ,(first factors) ,(unfactorize (rest factors))))))

(defun divide-factors (numer denom)
  "Divide a list of factors by another, producing a third."
  (let ((result (mapcar #'copy-list numer)))
    (dolist (d denom)
      (let ((factor (find (expr-lhs d) result :key #'expr-lhs
                          :test #'equal)))
        (if factor
          (decf (expr-rhs factor) (expr-rhs d))
          (push `(^ ,(expr-lhs d) ,(- (expr-rhs d))) result))))
    (delete 0 result :key #'expr-rhs)))

(defun length=1 (ls)
  (and (consp ls)
       (null (rest ls))))

(defun free-of (expr var)
  "True if expression has no occurrence of var."
  (not (find-anywhere var expr)))

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree? If so, return it."
  (cond ((eql item tree) tree)
        ((atom tree) nil)
        ((find-anywhere item (first tree)))
        ((find-anywhere item (rest tree)))))

(defun integrate (exp x)
  ;; First try some trivial cases
  (cond
    ((free-of exp x) `(* ,exp x))          ; Int c dx = c*x
    ((starts-with exp '+)                  ; Int f + g  = 
     `(+ ,(integrate (expr-lhs exp) x)      ;   Int f + Int g
         ,(integrate (expr-rhs exp) x)))
    ((starts-with exp '-)              
     (ecase (length (expr-args exp))        
       (1 (integrate (expr-lhs exp) x))     ; Int - f = - Int f
       (2 `(- ,(integrate (expr-lhs exp) x) ; Int f - g  =
              ,(integrate (expr-rhs exp) x)))))  ; Int f - Int g
    ;; Now move the constant factors to the left of the integral
    ((multiple-value-bind (const-factors x-factors)
         (partition-if #'(lambda (factor) (free-of factor x))
                       (factorize exp))
       (identity ;simplify
         `(* ,(unfactorize const-factors)
             ;; And try to integrate:
             ,(cond ((null x-factors) x)
                    ((some #'(lambda (factor)
                               (deriv-divides factor x-factors x))
                           x-factors))
                    ;; <other methods here>
                    (t `(int? ,(unfactorize x-factors) ,x)))))))))

(defun partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

(defun deriv-divides (factor factors x)
  (assert (starts-with factor '^))
  (let* ((u (expr-lhs factor))              ; factor = u^n
         (n (expr-rhs factor))
         (k (divide-factors 
              factors (factorize `(* ,factor ,(deriv u x))))))
    (cond ((free-of k x)
           ;; Int k*u^n*du/dx dx = k*Int u^n du
           ;;                    = k*u^(n+1)/(n+1) for n/=1
           ;;                    = k*log(u) for n=1
           (if (= n -1)
               `(* ,(unfactorize k) (log ,u))
               `(/ (* ,(unfactorize k) (^ ,u ,(+ n 1)))
                   ,(+ n 1))))
          ((and (= n 1) (in-integral-table? u))
           ;; Int y'*f(y) dx = Int f(y) dy
           (let ((k2 (divide-factors
                       factors
                       (factorize `(* ,u ,(deriv (expr-lhs u) x))))))
             (if (free-of k2 x)
                 `(* ,(integrate-from-table (expr-op u) (expr-lhs u))
                     ,(unfactorize k2))))))))

(defun deriv (y x) (simplify `(d ,y ,x)))

(defun integration-table (rules)
  (dolist (i-rule rules)
    ;; changed infix->prefix to simp-rule - norvig Jun 11 1996
    (let ((rule (simp-rule i-rule)))
      (setf (get (expr-op (expr-lhs (expr-lhs rule))) 'int)
            rule))))

(defun in-integral-table? (exp)
  (and (expr-p exp) (get (expr-op exp) 'int)))

(defun integrate-from-table (op arg)
  (let ((rule (get op 'int)))
    (subst arg (expr-lhs (expr-lhs (expr-lhs rule))) (expr-rhs rule))))

(set-simp-fn 'Int #'(lambda (exp)
		      (unfactorize
		       (factorize
			(integrate (expr-lhs exp) (expr-rhs exp))))))

(integration-table
  '((Int log(x) d x = x * log(x) - x)
    (Int exp(x) d x = exp(x))
    (Int sin(x) d x = - cos(x))
    (Int cos(x) d x = sin(x))
    (Int tan(x) d x = - log(cos(x)))
    (Int sinh(x) d x = cosh(x))
    (Int cosh(x) d x = sinh(x))
    (Int tanh(x) d x = log(cosh(x)))
    ))

