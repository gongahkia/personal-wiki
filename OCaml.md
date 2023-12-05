> continue making notes from here https://learnxinyminutes.com/docs/ocaml/ on unary minus, google what unary minus is

# `OCaml`

A strictly evaluated, functional language with imperative features.

## Comments

```ocaml
(* This is a comment *)
```

## Variables and functions

* double semicolon language *(to separate expressions, though not always necessary)*
* `let` for function and variable declaration
* [currying](https://www.codingame.com/playgrounds/6196/explaining-currying-to-myself) and [type signatures](https://stackoverflow.com/questions/6005176/ocaml-explicit-type-signatures) like haskell
* optional type declaration *(OCaml compiler automatically infers types)*

```ocaml
(*** FUNCTIONAL LAND ***)

(* everything in OCaml is an expression and every expression (* variables, functions *) evaluates to a value *)

(* OCaml lacks "procedures" and every function must evaluate to a value, so functions that don't do that and are called for their side effects (* like print_endline *) return a value of "unit" type *)

(*** QUICKSTART ***)

(*** EXPRESSIONS ***)

(* Expressions can be separated by a double semicolon, though in production source code often omits the double semicolon for stylistic purposes. *)

(*** VARIABLES ***)

let x = 10;;  (* this is a variable *)

(* definitons can be chained together with "let ... in" constructs *)

let x = 10 in 
let y = 20 in
x + y ;; (* this expression evaluates to 30 *)

(*** FUNCTIONS ***)

let func_int(x:int):int = x + 1;; (* this is a function *)

(* invoking functions usually doesn't require brackets UNLESS the function argument is an expression *)

let func_int_10 = func_int 10;;
let func_inc_idk = func_int (11-1);;

(* every function must take at least one argument *)

let print_hlo_wrld() = print_endline "hello world";; (* when creating void functions, we need to specify the bracket to denote the empty function *)
print_hlo_wrld();; (* when calling void functions, we must specify the empty bracket *)

(* calling a function with an insufficient number of arguments creates a new function *)
let make_inc x y = x + y;; (* the type signature for make_inc is int -> int -> int *)
let inc_2 = make_inc 2;; (* this creates a new function inc_2 of type signature int -> int *)
inc_2 3;; (* this will evaluate to 5 *)

(* when there are multiple expressions in the function body, the last expression becomes the return value and all other expressions are of "unit" type *)

let print_and_return x = 
    print_endline (string_of_int x);
    x
;;

(*** RECURSIVE FUNCTIONS ***)

(* recursive functions must be marked with a rec*)

let rec factorial n = 
    if n = 0 then 1
    else n * factorial (n-1)
;;

(* note the above can also be written like this *)

let rec factorial n = if n = 0 then 1 else n * factorial (n-1);; (* just that this shit is really virtually unreadable so the above is preferred *)

(* the "let ... and ... in" construct is also available for mutually recursive functions *)

let rec 
    is_even = function 
    | 0 -> true
    | n -> is_odd(n-1)
and 
    is_odd = function
    | 0 -> false
    | n -> is_even(n-1)
;;

(*** ANONYMOUS FUNCTIONS ***)

let my_lambda = fun x -> x * x;;

(*** OPERATORS ***)

(* in functional programming land, every operator is a function and can be called as such *)
(+) 3 4;; (* evaluates the same way as 3 + 4 *)

(* OCaml DOESN'T ONLY refrain from implict type conversion between floats and integers, float arithmetic uses wholly different operators *)

12 + 3;; (* integer arithmetic, evaluates to 15 *)
12 / 3;; (* integer arithmetic, evalutes to 4 *)

12.0 +. 3.0;; (* float arithmetic, evaluates to 15.0 *)
12.0 /. 3.0;; (* float arithmetic, evaluates to 4.0 *)

5 mod 2;; (* modulo is universal across floats and integers *)
```
