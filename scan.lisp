(in-package cylon)

(defparameter *tab-size* 8)

(defvar *look*)
(defvar *look-ptr*)
(defvar *column*)
(defvar *indent-stack*)

(defun readchar ()
  (read-char *standard-input* nil :eof))

(defun init ()
  (setf *column* 0)
  (setf *indent-stack* '(0))
  (setf *look* (vector (readchar) (readchar) (readchar)))
  (setf *look-ptr* 0))

(defun look (&optional (n 0))
  (elt *look* (mod (+ n *look-ptr*) (length *look*))))

(defun getchar ()
  (setf *column*
        (+ *column*
           (if (eq #\tab *look*) *tab-size* 1)))
  (prog1
      (if (eq :eof (look)) (error "EOF") (look))
    (setf (elt *look* *look-ptr*) (readchar))
    (setf *look-ptr* (mod (1+ *look-ptr*) (length *look*)))))

(defun emit (&rest items)
  (format t " ~{~a~}" items))

(defun emitln (&rest items)
  (apply #'emit items)
  (format t "~&"))

(defun expected (expected)
  (error (concatenate 'string "Expected: '" expected "'")))

(defun whitespace-p ()
  (or
   (find (look) '(#\space #\tab))
   (and (eq (look 0) #\\)
        (eq (look 1) #\newline))))

(defun name-p ()
  (alpha-char-p (look)))

(defun in-name-p ()
  (alphanumericp (look)))

(defun number-p ()
  (digit-char-p (look)))

(defun newline-p ()
  (eq #\newline (look)))

(defun eof-p ()
  (eq :eof (look)))

(defun string-p ()
  (find (look) '(#\" #\')))

(defun in-string-p (quote-char)
  (not (eq (look) quote-char)))

(defun indent-p ()
  (> *column* (car *indent-stack*)))

(defun dedent-p ()
  (< *column* (car *indent-stack*)))

(defun whitespace ()
  (loop while (whitespace-p) do
       (if (eq (getchar) #\\)
           (assert (eq (getchar) #\newline)))))

(defun newline ()
  (if (not (newline-p)) (expected "NEWLINE"))
  (getchar)
  (emitln :newline)
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

(defun get-string ()
  (let ((quote-char (getchar)))
    (emit
     :string
     (coerce
      (loop while (in-string-p quote-char) collecting
           (if (eq (look) #\\) (unescape-char) (getchar)))
      'string))
    (getchar)))

(defun unescape-char ()
  (getchar)
  (let ((escaped (getchar)))
    (case escaped
      (#\n #\newline)
      (#\t #\tab)
      (otherwise escaped))))

(defun token ()
  (cond
    ((name-p) (name))
    ((number-p) (get-number))
    ((string-p) (get-string))
    (t (expected "token")))
  (whitespace))

(defun line ()
  (loop while (not (newline-p)) do
       (token))
  (if (not (eof-p)) (newline)))

(defun lines ()
  (loop while (not (eof-p)) do (line)))
