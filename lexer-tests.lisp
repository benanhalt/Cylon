(in-package cylon.lexer)

(defmacro stdin-str (str &body body)
  `(with-input-from-string (in ,str)
     (let ((lexer (make-instance 'lexer :stream in)))
       (init lexer)
       ,@body)))

(define-test test-init
  (stdin-str "123456"
    (assert-equal "123" (look-ahead lexer))
    (assert-equal 0 (look-ptr lexer))
    (assert-equal 0 (column lexer))
    (assert-equal '(0) (indent-stack lexer))))

(define-test test-init-eof
  (stdin-str ""
    (dotimes (i (length (look-ahead lexer)))
      (assert-equal *eof* (look lexer i)))))

(define-test test-getchar
  (stdin-str "123456"
    (dotimes (i 6)
      (assert-equal (+ i (char-code #\1)) (char-code (getchar lexer)))
      (assert-equal (1+ i) (column lexer))
      (assert-equal (mod (1+ i) (length (look-ahead lexer))) (look-ptr lexer)))
    (assert-error 'eof-error (getchar lexer))))

(define-test test-get-name
  (stdin-str "foo"
    (assert-equal '(:name "foo") (get-name lexer)))

  (stdin-str "foo "
    (assert-equal '(:name "foo") (get-name lexer))))

(define-test test-get-string
  (stdin-str "\"foo bar\""
    (assert-equal '(:string "foo bar") (get-string lexer)))

  (stdin-str "'foo\\'bar'"
    (assert-equal '(:string "foo'bar") (get-string lexer)))

  (stdin-str "'foo\\tbar'"
    (assert-equal '(:string "foo	bar") (get-string lexer)))

  (stdin-str "'foo\\nbar'"
    (assert-equal '(:string "foo
bar") (get-string lexer))))

(define-test test-whitespace
  (stdin-str "   foo"
    (whitespace lexer)
    (assert-equal '(:name "foo") (get-name lexer)))

  (stdin-str "   \\
    foo"
    (whitespace lexer)
    (assert-equal '(:name "foo") (get-name lexer)))

  (stdin-str "	foo"
    (whitespace lexer)
    (assert-equal '(:name "foo") (get-name lexer))))

(define-test test-get-line
  (stdin-str "foo foo 123 'bar'"
    (assert-equal '((:name "foo")
                    (:name "foo")
                    (:number "123")
                    (:string "bar")
                    :newline)
                  (get-line lexer)))

  (stdin-str "foo
"
    (assert-equal '((:name "foo") :newline)
                  (get-line lexer))))

(define-test test-get-lines
  (stdin-str
"foo
    bar
"
    (assert-equal '((:name "foo") :newline
                    :indent (:name "bar") :newline
                    :dedent)
                  (get-lines lexer)))

  (stdin-str
"foo
    bar
baz"

    (assert-equal '((:name "foo") :newline
                    :indent (:name "bar") :newline
                    :dedent (:name "baz") :newline)
                  (get-lines lexer)))

  (stdin-str
"foo
    bar
  baz
"
    (assert-error 'parsing-error (get-lines lexer)))

  (stdin-str
"
foo

    bar
  
         baz
 
bing
"
    (assert-equal '(:newline
                    (:name "foo") :newline
                    :indent (:name "bar") :newline
                    :indent (:name "baz") :newline
                    :dedent :dedent (:name "bing") :newline)
                  (get-lines lexer)))

  (stdin-str
"foo
bar
"
    (assert-equal '((:name "foo") :newline
                    (:name "bar") :newline)
                  (get-lines lexer)))

  (stdin-str
"foo # whatever
   # comment
bar
"
    (assert-equal '((:name "foo") :newline
                    (:name "bar") :newline)
                  (get-lines lexer))))
