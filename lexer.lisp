(in-package cylon.lexer)

(defparameter *tab-size* 8)
(defparameter *eof* #\null)

(defvar *look*)
(defvar *look-ptr*)
(defvar *column*)
(defvar *indent-stack*)

(define-condition eof-error (error) ())
(define-condition parsing-error (error) ())

(defun readchar ()
  (read-char nil nil *eof*))

(defun init ()
  (setf *column* 0)
  (setf *indent-stack* '(0))
  (setf *look* (make-array 3 :element-type 'character))
  (dotimes (i (length *look*)) (setf (elt *look* i) (readchar)))
  (setf *look-ptr* 0))

(defun look (&optional (n 0))
  (assert (< n (length *look*)))
  (elt *look* (mod (+ n *look-ptr*) (length *look*))))

(defun getchar ()
  (incf *column* (if (eql #\tab *look*) *tab-size* 1))
  (prog1
      (if (eql *eof* (look)) (error 'eof-error) (look))
    (setf (elt *look* *look-ptr*) (readchar))
    (setf *look-ptr* (mod (1+ *look-ptr*) (length *look*)))))

(defun match (expected)
  (if (eql expected (look))
      (getchar)
      (expected expected)))

(defun expected (expected)
  (error 'parsing-error (concatenate 'string "Expected: '" expected "'")))

(defun comment-p ()
  (eql (look) #\#))

(defun whitespace-p ()
  (or
   (find (look) '(#\space #\tab))
   (and (eql (look 0) #\\)
        (eql (look 1) #\newline))))

(defun name-p ()
  (alpha-char-p (look)))

(defun in-name-p ()
  (alphanumericp (look)))

(defun number-p ()
  (digit-char-p (look)))

(defun newline-p ()
  (eql #\newline (look)))

(defun eof-p ()
  (eql *eof* (look)))

(defun string-p ()
  (find (look) '(#\" #\')))

(defun in-string-p (quote-char)
  (not (eql (look) quote-char)))

(defun indent-p ()
  (> *column* (car *indent-stack*)))

(defun dedent-p ()
  (< *column* (car *indent-stack*)))

(defun whitespace ()
  (loop while (whitespace-p) do
       (if (eql (getchar) #\\)
           (assert (eql (getchar) #\newline)))))

(defun do-comment ()
  (unless (comment-p) (expected "COMMENT"))
  (loop until (or (newline-p) (eof-p))
       do (getchar)))

(defun do-newline ()
  (loop while (newline-p) do
       (match #\newline)
       (setf *column* 0)
       (whitespace)
       (if (comment-p) (do-comment)))
  (cons :newline
        (cond
          ((indent-p) (indent))
          ((dedent-p) (dedent)))))

(defun indent ()
  (push *column* *indent-stack*)
  (list :indent))

(defun dedent ()
  (loop while (< *column* (car *indent-stack*))
     do (pop *indent-stack*)
     collecting :dedent into dedents
     finally
       (unless (eql *column* (car *indent-stack*))
         (error 'parsing-error "bad indentation"))
       (return dedents)))

(defun get-name ()
  (unless (name-p) (expected "NAME"))
  (list
   :name
   (loop while (in-name-p)
      collecting (getchar) into chars
      finally (return (coerce chars 'string)))))

(defun get-number ()
  (unless (number-p) (expected "NUMBER"))
  (list
   :number
   (loop while (number-p)
      collecting (getchar) into chars
      finally (return (coerce chars 'string)))))

(defun get-string ()
  (unless (string-p) (expected "STRING"))
  (let ((quote-char (getchar)))
    (list
     :string
     (loop while (in-string-p quote-char)
        collecting
          (if (eql (look) #\\) (unescape-char) (getchar))
        into chars finally
          (getchar)
          (return (coerce chars 'string))))))

(defun unescape-char ()
  (getchar)
  (let ((escaped (getchar)))
    (case escaped
      (#\n #\newline)
      (#\t #\tab)
      (otherwise escaped))))

(defun get-token ()
  (prog1
      (cond
        ((name-p) (get-name))
        ((number-p) (get-number))
        ((string-p) (get-string))
        (t (expected "TOKEN")))
    (whitespace)))

(defun get-line ()
  (loop until (or (comment-p) (newline-p) (eof-p))
     collecting (get-token) into tokens
     finally
       (if (comment-p) (do-comment))
       (return
         (cond
           ((newline-p) (nconc tokens (do-newline)))
           ((eof-p) (nconc tokens (list :newline)))
           (t (expected "NEWLINE or EOF"))))))

(defun get-lines ()
  (loop until (eof-p)
     nconc (get-line)))

