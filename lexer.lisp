(in-package cylon.lexer)

(defparameter *tab-size* 8)
(defparameter *eof* #\null)

(define-condition eof-error (error) ())
(define-condition parsing-error (error) ())

(defclass lexer ()
  ((stream
    :initarg :stream
    :initform nil
    :reader stream-in)
   (look-ahead
    :initform (make-array 3 :element-type 'character)
    :accessor look-ahead)
   (look-ptr
    :initform 0
    :accessor look-ptr)
   (column
    :initform 0
    :accessor column)
   (indent-stack
    :initform '(0)
    :accessor indent-stack)))

(defgeneric readchar (lexer)
  (:documentation "Reads one character from the input stream.")
  (:method ((lexer lexer)) (read-char (stream-in lexer) nil *eof*)))

(defgeneric init (lexer)
  (:method ((lexer lexer))
    (with-slots (look-ahead) lexer
      (dotimes (i (length look-ahead))
        (setf (elt look-ahead i) (readchar lexer))))))

(defgeneric look (lexer &optional n)
  (:method ((lexer lexer) &optional (n 0))
    (with-slots (look-ahead look-ptr) lexer
      (assert (and (<= 0 n) (< n (length look-ahead))))
      (elt look-ahead (mod (+ n look-ptr) (length look-ahead))))))

(defgeneric getchar (lexer)
  (:method ((lexer lexer))
    (with-slots (column look-ptr look-ahead) lexer
      (let ((look (look lexer)))
        (incf column (if (eql #\tab (look lexer)) *tab-size* 1))
        (prog1
            (if (eql *eof* look) (error 'eof-error) look)
          (setf (elt look-ahead look-ptr) (readchar lexer))
          (setf look-ptr (mod (1+ look-ptr) (length look-ahead))))))))

(defgeneric match (lexer expected)
  (:method ((lexer lexer) expected)
    (if (eql expected (look lexer))
        (getchar lexer)
        (expected expected))))

(defun expected (expected)
  (error 'parsing-error (concatenate 'string "Expected: '" expected "'")))

(defgeneric comment-p (lexer)
  (:method ((lexer lexer))
    (eql (look lexer) #\#)))

(defgeneric whitespace-p (lexer)
  (:method ((lexer lexer))
    (or
     (find (look lexer) '(#\space #\tab))
     (and (eql (look lexer 0) #\\)
          (eql (look lexer 1) #\newline)))))

(defgeneric name-p (lexer)
  (:method ((lexer lexer))
    (alpha-char-p (look lexer))))

(defgeneric in-name-p (lexer)
  (:method ((lexer lexer))
    (alphanumericp (look lexer))))

(defgeneric number-p (lexer)
  (:method ((lexer lexer))
    (digit-char-p (look lexer))))

(defgeneric newline-p (lexer)
  (:method ((lexer lexer))
    (eql #\newline (look lexer))))

(defgeneric eof-p (lexer)
  (:method ((lexer lexer))
    (eql *eof* (look lexer))))

(defgeneric string-p (lexer)
  (:method ((lexer lexer))
    (find (look lexer) '(#\" #\'))))

(defgeneric in-string-p (lexer quote-char)
  (:method ((lexer lexer) quote-char)
    (not (eql (look lexer) quote-char))))

(defgeneric indent-p (lexer)
  (:method ((lexer lexer))
    (> (column lexer) (car (indent-stack lexer)))))

(defgeneric dedent-p (lexer)
  (:method ((lexer lexer))
    (< (column lexer) (car (indent-stack lexer)))))

(defgeneric whitespace (lexer)
  (:method ((lexer lexer))
    (loop while (whitespace-p lexer) do
         (if (eql (getchar lexer) #\\)
             (assert (eql (getchar lexer) #\newline))))))

(defgeneric do-comment (lexer)
  (:method ((lexer lexer))
    (unless (comment-p lexer) (expected "COMMENT"))
    (loop until (or (newline-p lexer) (eof-p lexer))
       do (getchar lexer))))

(defgeneric do-newline (lexer)
  (:method ((lexer lexer))
    (loop while (newline-p lexer) do
         (match lexer #\newline)
         (setf (column lexer) 0)
         (whitespace lexer)
         (if (comment-p lexer) (do-comment lexer)))
    (cons :newline
          (cond
            ((indent-p lexer) (indent lexer))
            ((dedent-p lexer) (dedent lexer))))))

(defgeneric indent (lexer)
  (:method ((lexer lexer))
    (push (column lexer) (indent-stack lexer))
    (list :indent)))

(defgeneric dedent (lexer)
  (:method ((lexer lexer))
    (with-slots (column indent-stack) lexer
      (loop while (< column (car indent-stack))
         do (pop indent-stack)
         collecting :dedent into dedents
         finally
           (unless (eql column (car indent-stack))
             (error 'parsing-error "bad indentation"))
           (return dedents)))))

(defgeneric get-name (lexer)
  (:method ((lexer lexer))
    (unless (name-p lexer) (expected "NAME"))
    (list
     :name
     (loop while (in-name-p lexer)
        collecting (getchar lexer) into chars
        finally (return (coerce chars 'string))))))

(defgeneric get-number (lexer)
  (:method ((lexer lexer))
    (unless (number-p lexer) (expected "NUMBER"))
    (list
     :number
     (loop while (number-p lexer)
        collecting (getchar lexer) into chars
        finally (return (coerce chars 'string))))))

(defgeneric get-string (lexer)
  (:method ((lexer lexer))
    (unless (string-p lexer) (expected "STRING"))
    (let ((quote-char (getchar lexer)))
      (list
       :string
       (loop while (in-string-p lexer quote-char)
          collecting
            (if (eql (look lexer) #\\)
                (unescape-char lexer)
                (getchar lexer))
          into chars finally
            (getchar lexer)
            (return (coerce chars 'string)))))))

(defgeneric unescape-char (lexer)
  (:method ((lexer lexer))
    (getchar lexer)
    (let ((escaped (getchar lexer)))
      (case escaped
        (#\n #\newline)
        (#\t #\tab)
        (otherwise escaped)))))

(defgeneric get-atom (lexer)
  (:method ((lexer lexer))
    (prog1
        (cond
          ((name-p lexer) (get-name lexer))
          ((number-p lexer) (get-number lexer))
          ((string-p lexer) (get-string lexer))
          (t (expected "TOKEN")))
      (whitespace lexer))))

(defgeneric get-line (lexer)
  (:method ((lexer lexer))
    (loop until (or (comment-p lexer) (newline-p lexer) (eof-p lexer))
       collecting (get-atom lexer) into atoms
       finally
         (if (comment-p lexer) (do-comment lexer))
         (return
           (cond
             ((newline-p lexer) (nconc atoms (do-newline lexer)))
             ((eof-p lexer) (nconc atoms (list :newline)))
             (t (expected "NEWLINE or EOF")))))))

(defgeneric get-lines (lexer)
  (:method ((lexer lexer))
    (loop until (eof-p lexer)
       nconc (get-line lexer))))

