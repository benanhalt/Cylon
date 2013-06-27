;;;; cylon.lisp

(in-package #:cylon)

(defun eval-module (ast globals)
  (destructuring-bind (module-tag stmts) ast
    (assert (eq module-tag :module))
    (dolist (stmt stmts)
      (eval-stmt stmt globals globals))))

(defun eval-stmt (ast globals locals)
  (case (car ast)
    (:expr (eval-expr (cadr ast) globals locals))
    (:assign (handle-assign ast globals locals))
    (t (error "bad statement"))))

(defun handle-assign (ast globals locals)
  (destructuring-bind (tag targets expr) ast
    (assert (eq tag :assign))
    (let ((value (eval-expr expr globals locals)))
      (if (= 1 (length targets))
          (set-binding (car targets) locals value)))))

(defun set-binding (ast env value)
  (destructuring-bind (tag name ctx) ast
    (assert (eq tag :name))
    (setf (gethash (stringname env) value)))

(defun eval-expr (ast globals locals)
  (case (car ast)
    (:call (eval-call ast globals locals))
    (:name (eval-name ast globals locals))
    (:str (eval-str ast))
    (:num (eval-num ast))
    (t (error "bad expr"))))

(defun eval-call (ast globals locals)
  (destructuring-bind (call-tag func args keywords starargs kwargs) ast
    (assert (eq call-tag :call))
    (let* ((func-val (eval-expr func globals locals))
           (arg-vals (mapcar (lambda (arg) (eval-expr arg globals locals)) 
                             args)))
      func-val)))

(defun eval-name (ast globals locals)
  (destructuring-bind (tag name ctx) ast
    (assert (eq tag :name))
    (print name)))

(defun eval-str (ast)
  (destructuring-bind (tag value) ast
    (assert (eq tag :str))
    value))

(defun eval-num (ast)
  (destructuring-bind (tag value) ast
    (assert (eq tag :num))
    value))
