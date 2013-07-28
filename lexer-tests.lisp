(in-package cylon.lexer)

(defmacro stdin-str (str &body body)
  `(with-input-from-string (*standard-input* ,str)
     (init)
     ,@body))

(define-test test-init
  (stdin-str "123456"
    (assert-equal "123" *look*)
    (assert-equal 0 *look-ptr*)
    (assert-equal 0 *column*)
    (assert-equal '(0) *indent-stack*)))

(define-test test-init-eof
  (stdin-str ""
    (dotimes (i (length *look*))
      (assert-equal *eof* (look i)))))

(define-test test-getchar
  (stdin-str "123456"
    (dotimes (i 6)
      (assert-equal (+ i (char-code #\1)) (char-code (getchar)))
      (assert-equal (1+ i) *column*)
      (assert-equal (mod (1+ i) (length *look*)) *look-ptr*))
    (assert-error 'eof-error (getchar))))

(define-test get-name
  (stdin-str "foo"
    (assert-equal '(:name "foo") (get-name)))

  (stdin-str "foo "
    (assert-equal '(:name "foo") (get-name))))

(define-test test-get-string
  (stdin-str "\"foo bar\""
    (assert-equal '(:string "foo bar") (get-string)))

  (stdin-str "'foo\\'bar'"
    (assert-equal '(:string "foo'bar") (get-string)))

  (stdin-str "'foo\\tbar'"
    (assert-equal '(:string "foo	bar") (get-string)))

  (stdin-str "'foo\\nbar'"
    (assert-equal '(:string "foo
bar") (get-string))))

(define-test test-whitespace
  (stdin-str "   foo"
    (whitespace)
    (assert-equal '(:name "foo") (get-name)))

  (stdin-str "   \\
    foo"
    (whitespace)
    (assert-equal '(:name "foo") (get-name)))

  (stdin-str "	foo"
    (whitespace)
    (assert-equal '(:name "foo") (get-name))))

(define-test test-get-line
  (stdin-str "foo foo 123 'bar'"
    (assert-equal '((:name "foo")
                    (:name "foo")
                    (:number "123")
                    (:string "bar")
                    :newline)
                  (get-line)))

  (stdin-str "foo
"
    (assert-equal '((:name "foo") :newline)
                  (get-line))))

(define-test test-get-lines
  (stdin-str
"foo
    bar
"
    (assert-equal '((:name "foo") :newline
                    :indent (:name "bar") :newline
                    :dedent)
                  (get-lines)))

  (stdin-str
"foo
    bar
baz"

    (assert-equal '((:name "foo") :newline
                    :indent (:name "bar") :newline
                    :dedent (:name "baz") :newline)
                  (get-lines)))

  (stdin-str
"foo
    bar
  baz
"
    (assert-error 'parsing-error (get-lines)))

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
                  (get-lines)))

  (stdin-str
"foo
bar
"
    (assert-equal '((:name "foo") :newline
                    (:name "bar") :newline)
                  (get-lines)))

  (stdin-str
"foo # whatever
   # comment
bar
"
    (assert-equal '((:name "foo") :newline
                    (:name "bar") :newline)
                  (get-lines))))
