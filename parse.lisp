

(defmacro defgrammar (name &body body)
  `(defun ,name ()
     (list ',name
      ,@(loop for form in body collect
             (make-parser form)))))

(defmethod make-parser ((literal string))
  `(match ,literal))

(defmethod make-parser ((form list))
  (case (car form)
    ('or `(one-of ,@(cdr form)))
    ('one-or-more `(atleast-one ,@(cdr form)))))

(defmacro atleast-one (&rest items)
  `(and ,(make-parser (car items))
        

(defmacro one-of (&rest options)
  (if (null options) nil
      `(or ,(make-parser (car options))
           (one-of ,@(cdr options)))))

(defgrammar digit
  (or "0" "1"))

(defgrammar number
  (one-or-more '#digit))
