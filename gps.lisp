
(defvar *state* nil "The current state: a list of conditions")
(defvar *ops* nil "A list of available operators")

(defstruct op "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))
  
(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op)))
     
(defun GPS (*state* goals *ops*)
  "General Problem Solver: archieve all goals using *ops*"
  (if (archieve-all goals) 'solved))

(defun archieve-all (goals)
  "Try to archieve each goal, then make sure they still hold"
  (and (every #'archieve goals) (subsetp goals *state*)))
  
(defun archieve (goal)
  "A goal is archieved if it already holds
  or if there is an appropriate op for it that is applicable"
  (or (member goal *state*)
    (some #'apply-op
        (find-all goal *ops* :test #'appropriate-p))))

(defun apply-op (op)
  "Print a message and update *state* if op is applicable"
  (when (archieve-all (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))
    
(defun find-all (item sequence &rest keyword-args
                &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords. Doesn't alter sequence."
  (if test-not
    (apply #'remove item sequence
      :test-not (complement test-not) keyword-args)
    (apply #'remove item sequence
      :test (complement test) keyword-args)))

      (defparameter *school-ops*
        (list
          (make-op :action 'drive-son-to-school
            :preconds '(son-at-home car-works)
            :add-list '(son-at-school)
            :del-list '(son-at-home))
          (make-op :action 'shop-installs-battery
              :preconds '(car-needs-battery shop-knows-problem shop-has-money)
              :add-list '(car-works))
              (make-op :action 'tell-shop-problem
                :preconds '(in-communication-with-shop)
                :add-list '(shop-knows-problem))
                (make-op :action 'telephone-shop
                  :preconds '(know-phone-number)
                  :add-list '(in-communication-with-shop))
                  (make-op :action 'look-up-number
                    :preconds '(have-phone-book)
                    :add-list '(know-phone-number))
                    (make-op :action 'give-shop-money
                      :preconds '(have-money)
                      :add-list '(shop-has-money)
                      :del-list '(have-money))))