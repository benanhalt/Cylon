(in-package cylon)

(defparameter *tab-size* 8)

(defvar *look*)
(defvar *column*)
(defvar *indent-stack*)

(defun init ()
  (setf *column* 0)
  (setf *indent-stack* '(0))
  (setf *look* (read-char *standard-input* nil :eof)))

(defun getchar ()
  (assert (not (eq *look* :eof)))
  (setf *column*
        (+ *column*
           (if (eq #\tab *look*) *tab-size* 1)))
  (prog1
      *look*
    (setf *look* (read-char *standard-input* nil :eof))))

(defun emit (&rest items)
  (format t " ~{~a~}" items))

(defun emitln (&rest items)
  (apply #'emit items)
  (format t "~&"))

(defun expected (expected)
  (error (concatenate 'string "Expected: '" expected "'")))

(defun whitespace-p ()
  (find *look* '(#\space #\tab)))

(defun name-p ()
  (alpha-char-p *look*))

(defun in-name-p ()
  (alphanumericp *look*))

(defun number-p ()
  (digit-char-p *look*))

(defun newline-p ()
  (eq #\newline *look*))

(defun eof-p ()
  (eq :eof *look*))

(defun string-p ()
  (find *look* '(#\" #\')))

(defun indent-p ()
  (> *column* (car *indent-stack*)))

(defun dedent-p ()
  (< *column* (car *indent-stack*)))

(defun whitespace ()
  (loop while (whitespace-p) do (getchar))
  :whitespace)

(defun newline ()
  (if (not (newline-p)) (expected "NEWLINE"))
  (getchar)
  (emit :newline)
  (setf *column* 0)
  (whitespace)
  (cond
    ((newline-p) (newline))
    ((indent-p) (indent))
    ((dedent-p) (dedent))))

(defun indent ()
  (emit :indent)
  (push *column* *indent-stack*))

(defun dedent ()
  (loop while (< *column* (car *indent-stack*)) do
       (pop *indent-stack*)
       (emit :dedent))
  (assert (eq *column* (car *indent-stack*))))

(defun name ()
  (emit
   (coerce
    (loop while (in-name-p) collecting (getchar))
    'string)))

(defun get-number ()
  (emit
   (coerce
    (loop while (number-p) collecting (getchar))
    'string)))

(defun token ()
  (cond
    ((name-p) (name))
    ((number-p) (get-number))
    ;; ((string-p) (string))
    (t (expected "token")))
  (whitespace))

(defun line ()
  (loop while (not (newline-p)) do
       (token))
  (if (not (eof-p)) (newline)))

(defun lines ()
  (loop while (not (eof-p)) do (line)))
