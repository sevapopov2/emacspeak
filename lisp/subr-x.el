;;; subr-x.el --- extra Lisp functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: convenience
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Less commonly used functions that complement basic APIs, often implemented in
;; C code (like hash-tables and strings), and are not eligible for inclusion
;; in subr.el.

;; Do not document these functions in the lispref.
;; http://lists.gnu.org/archive/html/emacs-devel/2014-01/msg01006.html

;;; Code:

(require 'pcase)

(defmacro internal--thread-argument (first? &rest forms)
  "Internal implementation for `thread-first' and `thread-last'.
When Argument FIRST? is non-nil argument is threaded first, else
last.  FORMS are the expressions to be threaded."
  (pcase forms
    (`(,x (,f . ,args) . ,rest)
     `(internal--thread-argument
       ,first? ,(if first? `(,f ,x ,@args) `(,f ,@args ,x)) ,@rest))
    (`(,x ,f . ,rest) `(internal--thread-argument ,first? (,f ,x) ,@rest))
    (_ (car forms))))

(defmacro thread-first (&rest forms)
  "Thread FORMS elements as the first argument of their successor.
Example:
    (thread-first
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ (- (/ (+ 5 20) 25)) 40)
Note how the single `-' got converted into a list before
threading."
  (cl-declare (indent 1)
           (debug (form &rest [&or symbolp (sexp &rest form)])))
  `(internal--thread-argument t ,@forms))

(defmacro thread-last (&rest forms)
  "Thread FORMS elements as the last argument of their successor.
Example:
    (thread-last
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ 40 (- (/ 25 (+ 20 5))))
Note how the single `-' got converted into a list before
threading."
  (cl-declare (indent 1) (debug thread-first))
  `(internal--thread-argument nil ,@forms))

(defsubst internal--listify (elt)
  "Wrap ELT in a list if it is not one."
  (if (not (listp elt))
      (list elt)
    elt))

(defsubst internal--check-binding (binding)
  "Check BINDING is properly formed."
  (when (> (length binding) 2)
    (signal
     'error
     (cons "`let' bindings can have only one value-form" binding)))
  binding)

(defsubst internal--build-binding-value-form (binding prev-var)
  "Build the conditional value form for BINDING using PREV-VAR."
  `(,(car binding) (and ,prev-var ,(cadr binding))))

(defun internal--build-binding (binding prev-var)
  "Check and build a single BINDING with PREV-VAR."
  (thread-first
   binding
   internal--listify
   internal--check-binding
   (internal--build-binding-value-form prev-var)))

(defun internal--build-bindings (bindings)
  "Check and build conditional value forms for BINDINGS."
  (let ((prev-var t))
    (mapcar (lambda (binding)
              (let ((binding (internal--build-binding binding prev-var)))
                (setq prev-var (car binding))
                binding))
            bindings)))

(defmacro if-let (bindings then &rest else)
  "Process BINDINGS and if all values are non-nil eval THEN, else ELSE.
Argument BINDINGS is a list of tuples whose car is a symbol to be
bound and (optionally) used in THEN, and its cadr is a sexp to be
evalled to set symbol's value.  In the special case you only want
to bind a single value, BINDINGS can just be a plain tuple."
  (cl-declare (indent 2)
           (debug ([&or (&rest (symbolp form)) (symbolp form)] form body)))
  (when (and (<= (length bindings) 2)
             (not (listp (car bindings))))
    ;; Adjust the single binding case
    (setq bindings (list bindings)))
  `(let* ,(internal--build-bindings bindings)
     (if ,(car (internal--listify (car (last bindings))))
         ,then
       ,@else)))

(defmacro when-let (bindings &rest body)
  "Process BINDINGS and if all values are non-nil eval BODY.
Argument BINDINGS is a list of tuples whose car is a symbol to be
bound and (optionally) used in BODY, and its cadr is a sexp to be
evalled to set symbol's value.  In the special case you only want
to bind a single value, BINDINGS can just be a plain tuple."
  (cl-declare (indent 1) (debug if-let))
  (list 'if-let bindings (macroexp-progn body)))

(defsubst hash-table-empty-p (hash-table)
  "Check whether HASH-TABLE is empty (has 0 elements)."
  (zerop (hash-table-count hash-table)))

(defsubst hash-table-keys (hash-table)
  "Return a list of keys in HASH-TABLE."
  (let ((keys '()))
    (maphash (lambda (k _v) (push k keys)) hash-table)
    keys))

(defsubst hash-table-values (hash-table)
  "Return a list of values in HASH-TABLE."
  (let ((values '()))
    (maphash (lambda (_k v) (push v values)) hash-table)
    values))

(defsubst string-empty-p (string)
  "Check whether STRING is empty."
  (string= string ""))

(defsubst string-join (strings &optional separator)
  "Join all STRINGS using SEPARATOR."
  (mapconcat 'identity strings separator))

(define-obsolete-function-alias 'string-reverse 'reverse "25.1")

(defsubst string-trim-left (string)
  "Remove leading whitespace from STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

(defsubst string-trim-right (string)
  "Remove trailing whitespace from STRING."
  (if (string-match "[ \t\n\r]+\\'" string)
      (replace-match "" t t string)
    string))

(defsubst string-trim (string)
  "Remove leading and trailing whitespace from STRING."
  (string-trim-left (string-trim-right string)))

(defsubst string-blank-p (string)
  "Check whether STRING is either empty or only whitespace."
  (string-match-p "\\`[ \t\n\r]*\\'" string))

(defun string> (s1 s2)
  "Return t if first arg string is greater than second in lexicographic order.
Case is significant.
Symbols are also allowed; their print names are used instead."
  (string-lessp s2 s1))

(defun let-alist--deep-dot-search (data)
  "Return alist of symbols inside DATA that start with a `.'.
Perform a deep search and return an alist where each car is the
symbol, and each cdr is the same symbol without the `.'."
  (cond
   ((symbolp data)
    (let ((name (symbol-name data)))
      (when (string-match "\\`\\." name)
        ;; Return the cons cell inside a list, so it can be appended
        ;; with other results in the clause below.
        (list (cons data (intern (replace-match "" nil nil name)))))))
   ((not (consp data)) nil)
   ((eq (car data) 'let-alist)
    ;; For nested ‘let-alist’ forms, ignore symbols appearing in the
    ;; inner body because they don’t refer to the alist currently
    ;; being processed.  See Bug#24641.
    (let-alist--deep-dot-search (cadr data)))
   (t (append (let-alist--deep-dot-search (car data))
              (let-alist--deep-dot-search (cdr data))))))

(defun let-alist--access-sexp (symbol variable)
  "Return a sexp used to access SYMBOL inside VARIABLE."
  (let* ((clean (let-alist--remove-dot symbol))
         (name (symbol-name clean)))
    (if (string-match "\\`\\." name)
        clean
      (let-alist--list-to-sexp
       (mapcar #'intern (nreverse (split-string name "\\.")))
       variable))))

(defun let-alist--list-to-sexp (list var)
  "Turn symbols LIST into recursive calls to `cdr' `assq' on VAR."
  `(cdr (assq ',(car list)
              ,(if (cdr list) (let-alist--list-to-sexp (cdr list) var)
                 var))))

(defun let-alist--remove-dot (symbol)
  "Return SYMBOL, sans an initial dot."
  (let ((name (symbol-name symbol)))
    (if (string-match "\\`\\." name)
        (intern (replace-match "" nil nil name))
      symbol)))


;;; The actual macro.
;;;###autoload
(defmacro let-alist (alist &rest body)
  "Let-bind dotted symbols to their cdrs in ALIST and execute BODY.
Dotted symbol is any symbol starting with a `.'.  Only those present
in BODY are let-bound and this search is done at compile time.

For instance, the following code

  (let-alist alist
    (if (and .title .body)
        .body
      .site
      .site.contents))

essentially expands to

  (let ((.title (cdr (assq \\='title alist)))
        (.body  (cdr (assq \\='body alist)))
        (.site  (cdr (assq \\='site alist)))
        (.site.contents (cdr (assq \\='contents (cdr (assq \\='site alist))))))
    (if (and .title .body)
        .body
      .site
      .site.contents))

If you nest `let-alist' invocations, the inner one can't access
the variables of the outer one. You can, however, access alists
inside the original alist by using dots inside the symbol, as
displayed in the example above."
  (cl-declare (indent 1) (debug t))
  (let ((var (make-symbol "alist")))
    `(let ((,var ,alist))
       (let ,(mapcar (lambda (x) `(,(car x) ,(let-alist--access-sexp (car x) var)))
                     (delete-dups (let-alist--deep-dot-search body)))
         ,@body))))

(provide 'subr-x)

;;; subr-x.el ends here
