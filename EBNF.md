# EBNF

Extended Backus-Naur Form is a notation *(metalanguage)* used to define a programming language's syntax.   

Every widely used programming language today has an `.ebnf` file that acts as universally-understandable documentation regarding a language's grammer.

EBNF's syntax shares similarities with regular expressions.

## Simple syntax intro

* `::=` definition operator *(think of it as an assignment operator)*
* `|` alternative operator *(think of it as the or operator)*
* `*` repetition operator for ZERO or MORE
* `+` repetition operator for ONE or MORE
* `?` repetition operator for ZERO or ONE
* `()` grouping operators to specify order of operations
* `..` range operator to denote the full range of characters between the specified start and stop

```ebnf
expression ::= term | term ('+' | '-') expression
term ::= factor | factor '*' term
factor ::= '(' expression ')' | number
number ::= "0" .. "9"
```

More language syntax can be found [here](https://www.freecodecamp.org/news/what-are-bnf-and-ebnf/).
