# `APL`

A programming language.

## Comments

```apl
⍝ ----- COMMENT -----

⍝ this is a single-line comment

⍝ there is no built-in syntax for 
⍝ multi-line comments in APL but the
⍝ same effect can be achieved as such
```

## Printing

```apl
⍝ ----- PRINT -----
    ⍝ `⎕` => function that receives a string argument which is displayed to the stdout and does not include a newline automatically
    ⍝ `⎕← ⍴⊢` => function that receives a string argument which is then displayed to the stdout and this includes a newline by default

⎕ ← 'this does not include a newline by default and one must be explicitly specified\n'
'this includes a newline and we don't need to specify its inclusion ⎕← ⍴⊢ 
```

## Quickstart

```apl
⍝ ----- QUICKSTART -----
    ⍝ APL is an array-oriented programming langauge 
    ⍝ this means that scalars are also just arrays comprising of 1 element
    ⍝ 
    ⍝ 
```

## Types

```apl
⍝ ----- TYPE -----

⍝ --- SCALAR TYPES ---
    ⍝ integer number
    ⍝ floating-point number
    ⍝ character value, declared within '' single quotation marks
    ⍝ string value, declared within "" double quotation marks 
    ⍝ boolean TRUE and FALSE, declared as 1 and 0 respectively

3 ⍝ stores an integer number value
3.14 ⍝ stores a floating-point number value
'c' ⍝ stores a character value declared within '' single quotation marks
"hello" ⍝ stores a string value declared within "" double quotation marks, handled as a character vector
0 ⍝ boolean FALSE, can be combined with logical operators
1 ⍝ boolean TRUE, can be combined with logical operators

⍝ --- ARRAY TYPES ---
    ⍝ vector: one-dimensional array generated along a single axis, the equivalent of a list in most other programming languages
    ⍝ matrix: two-dimensional array with rows and columns

1 2 3 4 5 ⍝ numeric vector
1 2 ⍴ 3 4 5 6 ⍝ Numeric matrix, reshaped into 2 rows and 3 columns
'hello' 'world' ⍝ character vector (a.k.a a string)
(1 2 3) (4 5 6) ⍝ nested numeric vector
```

## Operators

```apl
⍝ ----- OPERATOR -----

⍝ --- ARITHMETIC OPERATORS ---

+  ⍝ addition
-  ⍝ subtraction
×  ⍝ multiplication
÷  ⍝ division
⍟  ⍝ logarithm operator

⍝ --- COMPARISON OPERATORS ---

=  ⍝ equality operator 
≠  ⍝ inequality operator
<  ⍝ comparison operator
≤  ⍝ comparison operator
>  ⍝ comparison operator
≥  ⍝ comparison operator

⍝ --- LOGICAL OPERATORS ---

∧  ⍝ logical AND
∨  ⍝ logical OR
~  ⍝ logical NOT

⍝ --- ARRAY OPERATORS ---

⍴  ⍝ reshape operator
⍳  ⍝ index generator operator
⍴  ⍝ reshape array operator
,  ⍝ flatten array operator
```

## Control structures

```apl
⍝ ----- CONTROL STRUCTURE -----

```

## Data structures

```apl
⍝ ----- DATA STRUCTURE -----

```

## Functions

```apl
⍝ ----- FUNCTION -----

```

## More on

* [learn APL in y minutes](https://learnxinyminutes.com/docs/apl/)
* [APL documentation](https://aplwiki.com)
* [GNU APL](https://www.gnu.org/software/apl/apl.html)
* [APL routines](https://aplwiki.com/routines)
* [APL Course](https://course.dyalog.com/) by Dyalog
* [TryAPL](https://tryapl.org/)
* [Learning APL](https://xpqz.github.io/learnapl/intro.html)
* [Functional vs Array Programming](https://youtu.be/UogkQ67d0nY?si=-AYn3N3XyKRYzK-O) by code_report
