
(load "gps2")
(load "search")
(load "blocks")

(use (make-block-ops '(a b c)))

(defun search-gps (start goal &optional (beam-width 10))
  "Search for a sequence of operators leading to a goal."
  (find-all-if
    #'action-p
    (beam-search
      (cons '(start) start)
      #'(lambda (state) (subsetp goal state :test #'equal))
      #'gps-successors
      #'(lambda (state)
          (+ (count-if #'action-p state)
             (count-if #'(lambda (con)
                           (not (member-equal con state)))
                       goal)))
      beam-width)))

(defun gps-successors (state)
  "Return a list of states reachable from this one using ops."
  (mapcar
    #'(lambda (op)
        (append
          (remove-if #'(lambda (x)
                         (member-equal x (op-del-list op)))
                     state)
          (op-add-list op)))
    (applicable-ops state)))

(defun applicable-ops (state)
  "Return a list of all ops that are applicable now."
  (find-all-if
    #'(lambda (op)
        (subsetp (op-preconds op) state :test #'equal))
    *ops*))

(setf start '((c on a) (a on table) (b on table) (space on c) (space on b) (space on table)))
