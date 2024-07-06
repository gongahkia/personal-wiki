# `Prolog`

Logic programming language from 1972 that has inspired many modern languages.

## Comments

```pl
% ----- COMMENT -----

% this is a single-line comment

/* 
this is a 
multi-line 
comment
*/
```

## Printing

```pl
% ----- PRINTING -----
    % write() => receives a string argument which is then printed to the stdout and does not include a newline automatically
    % writeln() => receives a string argument which is then printed to the stdout and includes a newline by default

?- write('this does not have a newline and we must explicitly specify its inclusion\n').
?- writeln('this does have a newline by default').
```

## Quickstart

```pl
% ----- QUICKSTART -----
    % 


CLEAN THIS UP AND ADD EXPLANATION FROM LEARN PROLOG IN Y MINUTES LATER

% Run all examples
run_examples :-
    atom_example,
    numbers_example,
    variables_example,
    compound_terms_example,
    lists_example,
    strings_example,
    dictionaries_example.

% To run the examples, query the predicate `run_examples`.
% ?- run_examples.
```

## Types

```pl
% ----- TYPE -----
    % atom => symbolic constants declared as unquoted lowercase words, special characters or quoted strings, the equivalent of atoms in Lisp and Erlang and symbols in Ruby
    % number => covers both integer number values and floating-point number values
    % string => stored as a list of character codes
    % variable => stores an unknown or mutable value declared as beginning with an uppercase letter or an _ underscore, the equivalent of variable declaration in most other programming languages
        % note that _ underscore-prefixed variables create only a TEMPORARY variable binding
    % compound term => represents user-defined structured data, consisting of both a functor and an argument, called similarly to function calls in most other programming languages
        % functor =>
        % argument =>

atom_example :- 
    A1 = atom, % simple lowercase atom
    A2 = 'Hello World', % atom with spaces, requiring quotes
    A3 = +, % special character atom
    writeln(A1),
    writeln(A2),
    writeln(A3).

numbers_example :- 
    N1 = 42, % an integer
    N2 = -7, % a negative integer
    N3 = 3.14, % a floating-point number
    writeln(N1),
    writeln(N2),
    writeln(N3).

variables_example :- 
    X = 10, % variable X bound to the number value 10
    Result = X + 5, % variable result stores the value X + 5
    _Temp = Result * 2, % temporary variable _Temp stores the value Result * 2
    writeln(X),
    writeln(Result),
    writeln(_Temp).

compound_terms_example :- 
    Term1 = person(john, 25), % compound term with the functor `person` and provided arguments `john` and `25`
    Term2 = point(3, 4), % compound term with the functor `point` and provided arguments `3` and `4`
    Term3 = tree(node, left_subtree, right_subtree), % compound term representing a tree
    writeln(Term1),
    writeln(Term2),
    writeln(Term3).

strings_example :- 
    S = "Hello, Prolog!", % example of a string
    writeln(S).
```

## Operators

```pl
% ----- OPERATOR -----

% --- ARITHMETIC OPERATORS ---

+ % addition
- % subtraction
* % multiplication
/ % division
// % integer division
mod % modulo
** % exponentiation 

arithmetic_operators_example :-
    A is 5 + 3, % an example of addition
    B is 10 - 2, % an example of subtraction
    C is 4 * 7, % an example of multiplication
    D is 8 / 2, % an example of division
    E is 8 // 2, % an example of integer division
    F is 5 mod 2, % an example of modulus
    G is 2 ** 3, % an example of exponentiation
    writeln(A),
    writeln(B),
    writeln(C),
    writeln(D),
    writeln(E),
    writeln(F),
    writeln(G).

% --- COMPARISON OPERATORS ---

= % assignment operator, the equivalent of variable assignment in most other programming languages
=:= % partial equality check for value but not type, but note that this is purely for arithmetic comparison and so only checks for equality in value of evaluated numbers at both ends of the operator
=\= % partial inequality check for value but not type, but note that this is purely for arithmetic comparison and so only checks for inequality in value of evaluated numbers at both ends of the operator
< % comparison operator
> % comparison operator
<= % comparison operator
>= % comparison operator

comparison_operators_example :-
    X = 5, % variable assignment 
    Y = 10, % variable assignment
    (X =:= 5 -> writeln('X is equal to 5'); writeln('X is not equal to 5')), % equality
    (X =\= Y -> writeln('X is not equal to Y'); writeln('X is equal to Y')), % inequality
    (X < Y -> writeln('X is less than Y'); writeln('X is not less than Y')), % less than
    (X =< Y -> writeln('X is less than or equal to Y'); writeln('X is greater than Y')), % less than or equal
    (X > Y -> writeln('X is greater than Y'); writeln('X is not greater than Y')), % greater than
    (X >= 5 -> writeln('X is greater than or equal to 5'); writeln('X is less than 5')). % greater than or equal

% --- LOGICAL OPERATORS ---

, % logical and operator
; % logical or operator
\+ % logical not operator

logical_operators_example :-
    (true, writeln('Logical AND (true and true)')),
    (fail; writeln('Logical OR (fail or true)')),
    (\+ fail -> writeln('Logical NOT (not fail)')).
```

## Control structures

```pl
% ----- CONTROL STRUCTURE -----

% --- CONDITIONALS ---

% --- LOOPS ---
```

## Data structures

```pl
% ----- DATA STRUCTURE -----
    % 

% list => 
% dictionary =>


% Lists
% Lists are ordered collections of elements. 

lists_example :- 
    L1 = [],                       % Empty list
    L2 = [1, 2, 3, 4],             % List of integers
    L3 = [1, foo, X, [a, b]],      % List with mixed types and a sublist
    L4 = [Head | Tail],            % List constructed with head and tail notation
    writeln(L1),
    writeln(L2),
    writeln(L3),
    (L2 = [H | T],                 % Deconstruct list L2 into head H and tail T
    writeln(H),                    % Write head of L2
    writeln(T)),                   % Write tail of L2
    writeln(L4).



% Dictionaries
% Dictionaries store key-value pairs. Note that this is specific to some Prolog implementations, like SWI-Prolog.

dictionaries_example :- 
    Dict = dict{name: "Alice", age: 30},  % Dictionary with keys `name` and `age`
    writeln(Dict).

% Run all examples
run_examples :-
    lists_example,
    dictionaries_example.

% To run the examples, query the predicate `run_examples`.
% ?- run_examples.
```

## Functions

```pl
% ----- FUNCTION -----
    % 

```

## More on

* [swi-prolog.org](https://www.swi-prolog.org/)
* [swi-prolog documentation](https://www.swi-prolog.org/pldoc/index.html)
* [learn prolog in y minutes](https://learnxinyminutes.com/docs/prolog/)
* [introduction to prolog](https://www.sfu.ca/~tjd/383summer2019/prolog_intro.html)
* [learn logtalk in y minutes](https://learnxinyminutes.com/docs/logtalk/)
* [learn clojure in y minutes](https://learnxinyminutes.com/docs/clojure/)
* [learn erlang in y minutes](https://learnxinyminutes.com/docs/erlang/)
* [learn mercury in y minutes](https://learnxinyminutes.com/docs/mercury/)
* [learn haskell in y minutes](https://learnxinyminutes.com/docs/haskell/)
* [learn common lisp in y minutes](https://learnxinyminutes.com/docs/common-lisp/)
* [learn ruby in y minutes](https://learnxinyminutes.com/docs/ruby/)
* [datalog documentation](https://clojure.github.io/clojure-contrib/doc/datalog.html)
* [curry documentation](https://www-ps.informatik.uni-kiel.de/currywiki/)
