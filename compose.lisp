(defun my-compose (&rest fn-list)
  (reduce
    #'(lambda (f all)
        (if (null all)
          #'(lambda (&rest args) (apply f args))
          #'(lambda (&rest args) (funcall f (apply all args)))))
    fn-list
    :from-end t
    :initial-value nil))
