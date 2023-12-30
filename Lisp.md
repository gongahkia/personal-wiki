> FUA: 
> * continue from 4. Equality
> * add the difference between variable and symbol

# `Lisp`

The programmable programming language.

## Quickstart

```lisp
;;; ----------- QUICKSTART ----------
    ; everything in Common Lisp is comprised of atoms or s-expressions
    ; functional expression-based programming language, so all expressions evaluate to a value

;; ATOM
    ; symbols or numbers
        ; symbol => sequence of characters with significance
        ; number => integer, floating-point

;; S-EXPRESSION
    ; atom or list
        ; list => collection of s-expressions enclosed in () parentheses, that contain atoms or other lists
```

## Comments

```lisp
;;; ----------- COMMENT ----------

;; SINGLE-LINE COMMENTS
    ; 4 semicolons for file-level comments
    ; 3 semicolons for section-level descriptions
    ; 2 semicolons for definitions within code
    ; 1 semicolon for definitions outside code

;; MULTI-LINE COMMENTS
    ;; delimited by #| and |#
```

## Printing

```lisp
;;; ---------- PRINTING ----------
    ; format => prints formatted text to stdout, t is shorthand for the stdout stream
    ; print => prints text to stdout as a string literal with "" double quotation marks
    ; princ => prints text to stdout without double quotation marks

(format t "Hello, World!") ; prints "Hello, World!" to the stdout
(print "Hello") ; prints "Hello" to the stdout
(princ "Hello") ; prints Hello to the stdout
```

## Variable 

```lisp
;;; ---------- VARIABLE ----------
    ; variable names are ** (earmuff) asterisk delimited
        ; defvar => creates a global variable whose value does not change upon re-evaluation
        ; defparameter => creates a global variable whose value does change upon re-evaluation
    ; let => creates a local binding within the given () scope, and returns the last value specified in the let form

(defparameter *a_var* 5) ; assigns the global variable *a_var* the number value of 5
*a_var* ; evaluates to 5

(let ((me "dance with you")) me) ; evaluates to "dance with you"
```

## Types

```lisp
;;; ---------- TYPE -----------

;; PRIMITIVE DATATYPE
    ; symbol => uppercased automatically upon evaluation
        ; intern => intern operator creates a symbol from a string
    ; number => integer, binary, octal, hexadecimal, single, double, ratio, complex numbers
        ; quote / ' => quote operator creates literal data without evaluating it (it has the ' shorthand)
    ; boolean => any non-nil value is t (true) including expressions, nil (false) and () empty list evaluates to nil
    ; char => char literals declared with #\
    ; string => fixed-length char array, declared with "" double quotation marks

'foo ; evaluates to FOO, symbol literal
(intern "abc") ; create the symbol |abc| from a string
(intern "EFG") ; create the symbol EFG from a string

9999 ; number integer
#b111 ; number binary
#o111 ; number octal
#x111 ; number hexadecimal
3.14159s0 ; number single
3.14159d0 ; number double
1/2 ; number ratio
#C(1 2) ; complex number

(quote (+ 1 2)) ; quote creates literal data, and does not evaluate this function (+ 1 2)
'(+ 1 2) ; ' is the shorthand for quote, and it does the same thing, creating a literal data

t ; evaluates to t (true)
nil ; evaluates to nil (false)
() ; evaluates to nil (false)

#\A ; char literal 

"Hello, world!" ; string
```

## Operators

```lisp
;;; ---------- OPERATOR -----------

;; ARITHMETIC OPERATOR 
    ; + => addition
    ; - => subtraction
    ; * => multiplication
    ; / => division
    ; expt => exponentiation
    ; mod => modulo
    ; #C => creates a complex number

(+ 1 1) ; evaluates to 2
(- 8 1) ; evaluates to 7
(* 10 2) ; evaluates to 20
(expt 2 3) ; evaluates to 8
(mod 5 2) ; evaluates to 1
(/ 35 5) ; evaluates to 7
(/ 1 3) ; evaluates to 1/3
(+ #C(1 2) #C(6 -4)) ; evaluates to #C(7 -2)
```

## Control structures

```lisp

```

## Data structures

```lisp
;;; ---------- DATA STRUCTURE -----------

;; STRUCT
    ; defstruct => creates a struct
    ; defparameter => used to assign struct field values

(defstruct dog name breed age) ; defining dog struct
(defparameter *rover* 
    (make-dog : name "rover"
              : breed "collie"
              : age 5)) ; assigning struct field values

*rover* ; evaluates to #S(DOG :NAME "rover" :BREED "collie" :AGE 5)
(dog-name *rover*) ; evaluates to "rover"

;; PAIR
    ; cons => creates a pair
    ; car => returns the head of a pair
    ; cdr => returns the tail of a pair

(cons 'SUBJECT 'VERB) ; evaluates to '(SUBJECT . VERB)
(car (cons 'SUBJECT 'VERB)) ; evaluates to SUBJECT
(cdr (cons 'SUBJECT 'VERB)) ; evaluates to VERB

;; LIST
    ; every list is a linked-list comprised of cons pairs, the final element ending in a nil or '() to mark the end of the list
    ; list => convenient constructor to create a list 
    ; append => join two lists
    ; concatenate => join two lists when used alongside 'list
    ; mapcar => equivalent of .map
    ; remove-if-not => equivalent of .reduce
    ; every => checks if every list element fulfills a predicate
    ; some => checks if at least one list element fulfills a predicate
    ; butlast => returns a list but removes the last element

(cons 1 (cons 2 (cons 3 nil))) ; evaluates to '(1 2 3)
(list 1 2 3) ; evaluates to '(1 2 3)

(cons 4 '(1 2 3)) ; this syntax therefore is the equivalent of inserting an element at the front of a list, and evaluates to '(4 1 2 3)
(append '(1 2) '(3 4)) ; evaluates to '(1 2 3 4)
(concatenate 'list (1 2) '(3 4)) ; evaluates to '(1 2 3 4)
(mapcar #'1+ '(1 2 3)) ; evaluates to '(2 3 4)
(mapcar #'+ '(1 2 3) '(10 20 30)) ; evaluates to '(11 22 33)
(remove-if-not #'evenp '(1 2 3 4)) ; evaluates to '(2 4)
(every #'evenp '(1 2 3 4)) ; evaluates to () which is nil
(some #'oddp '(1 2 3 4)) ; evaluates to t
(butlast '(subject verb object)); evaluates to (SUBJECT VERB)

;; VECTOR
    ; vector literals are fixed-length arrays
    ; #() => declares a vector literal
    ; concatenate => joins two vectors

#(1 2 3) ; evaluates to #(1 2 3)
(concatenate 'vector #(1 2 3) #(4 5 6)) ; evaluates to #(1 2 3 4 5 6)

;; SET
    ; sets are just lists that you call set functions on
    ; operates the same as you'd expect in other languages like Python
    ; set-difference => returns the difference between two lists
    ; intersection => returns the similar elements between two lists
    ; union => returns two lists and removes duplicates
    ; adjoin => adds a specified element to the front of the list if element not already present and returns that list

(set-difference '(1 2 3 4) '(4 5 6 7)) ; evaluates to (3 2 1)
(intersection '(1 2 3 4) '(4 5 6 7)) ; evaluates to 4
(union '(1 2 3 4) '(4 5 6 7)) ; evaluates to (3 2 1 4 5 6 7)
(adjoin 4 '(1 2 3 4)) ; evaluates to (1 2 3 4)

;; HASH TABLE
    ; equivalent of dictionary 
    ; make-hash-table => creates a hash table, used alongside defparameter
    ; setf => sets a hash value
    ; gethash => retrieves a value at a specified hash and returns both the stored value and a boolean (t, nil) depending on whether a value can be found, used for assignment and simple retrieval

(defparameter *m* (make-hash-table)) ; creates a global hash table *m*
(setf (gethash 'a *m*) 1) ; sets a number value of 1 to the hash 'a
(gethash 'a *m*) ; retrieves the value stored at the hash 'a in the global hash table *m*, evaluating to 1, T
```

## Functions

```lisp
;;; ---------- FUNCTION -----------
    ; functions always return the evaluated value of the last expression
    ; function call syntax where f is function name and x y z are arguments is (f x y z)
    ; defun => creates a function, () is a list accepting arguments to the function
    ; &optional => specifies optional arguments to a function

;; FUNCTION CREATION

(defun hello-world () "Hello World") ; creates the function hello-world that evaluates to the string "Hello World"
(hello-world) ; evaluates to "Hello World"

(defun hello (name) (format nil "Hello, ~A" name)) ; creates the function hello that accepts the argument name
(hello "Steve") ; evaluates to "Hello, Steve"

;; OPTIONAL ARGUMENTS

(defun hello (name &optional from)
  (if from
      (format t "Hello, ~A, from ~A" name from)
      (format t "Hello, ~A" name))) ; optional function arguments (from) default to nil
(hello "Jim" "Alpacas") ; evaluates to "Hello, Jim, from Alpacas"

(defun hello (name &optional (from "The world"))
   (format nil "Hello, ~A, from ~A" name from)) ; default values (from's default value is the string "The World") can also be specified 
(hello "Steve") ; default value invoked, evaluates to "Hello, Steve, from The world"
(hello "Steve" "the alpacas") ; default value not invoked, evaluates to "Hello, Steve, from the alpacas"

;; ANONYMOUS FUNCTIONS
    ; lambda => creates an anonymous function
    ; funcall => calls an anonymous function with specified known arguments
    ; apply => calls an anonymous function when arguments are not known

(lambda () "Hello World") ; creation of anonymous function evaluates to #<FUNCTION (LAMBDA ()) {1004E7818B}>
(funcall (lambda () "Hello World")) ; => evaluates to "Hello World"
(apply (lambda () "Hello World") nil) ; evaluates to "Hello World"

```

## More on

* format
* array
* adjustable vector
* values
* &key
* [practical common lisp](https://gigamonkeys.com/book/)
* [lisp recipes](http://weitz.de/cl-recipes/)

