# `Lambda calculus`

Abstract notation to describe functions and application, providing the basis for functional programming.

## Introduction

* written as $\lambda$-calculus
* created by [Alonzo Church](https://www.britannica.com/biography/Alonzo-Church)
* can represent any [turing machine](https://brilliant.org/wiki/turing-machines/)
* by definition [the world's smallest programming language](https://users.cs.utah.edu/~mflatt/past-courses/cs7520/public_html/s06/notes.pdf)

## Quickstart

### Datatypes

$\lambda$-calculus has 3 foundational datatypes.
1. Variable
    * `<name>`
2. Function
    * $\lambda$ `<parameter>`.`<body>`
    * function parameter and body are . period-delimited
    * functions are single parameter
3. Application
    * `<function><variable OR function>`

In practice, it looks like this.

| Example | Explanation |
| :---: | :---: |
| $x$ | $x$ is a variable |
| $\lambda x . x$ | function definition for a function with parameter $x$ and body $x$ |
| $(\lambda x . x)a$ | function call for the previously declared function $\lambda x . x$ with the argument $a$ |

### Free and Bound variables

* **Free** variable: variable that is *never* declared prior to its specification in a function body
* **Bound** variable: variable that is declared within *both* the function parameter and body

In practice, they look like this.

| Example | Explanation |
| :---: | :---: |
| $\lambda x . x$ | $x$ is a bound variable |
| $\lambda x . y$ | $y$ is a free variable |

### Evaluation

* application evaluation is performed via $\beta$-reduction
* $\beta$-reduction: lexically-scoped substitution, a concept most of us are familiar with from foundational mathematical functions
* allows for higher-order functions, where functions themselves are provided as arguments to another function

In practice, it looks like this.

*example 1*
* $(\lambda x . x)a$ evaluates to $a$

*example 2*
* $(\lambda x . y)a$ evaluates to $y$

*example 3*
* $(\lambda x . (\lambda y . x ))a$ evaluates to $\lambda y . a$  
* this is an example of a higher-order function evaluated using $\beta$-reduction

## More on

* [identity function](https://math.stackexchange.com/questions/1972022/lambda-calculus-identity-function)
* [currying](https://www.matthiaspreu.com/posts/lambda-calculus-fundamentals/#currying---application-of-multiple-arguments)
* [church numerals](https://www.cs.rice.edu/~javaplt/311/Readings/supplemental.pdf)
* [boolean logic in lambda calculus](https://www.cs.columbia.edu/~sedwards/classes/2012/w4115-fall/lambda.pdf)
* [combinators](https://crypto.stanford.edu/~blynn/lambda/cl.html)
* [standard encyclopedia of philosophy on lambda calculus](https://plato.stanford.edu/entries/lambda-calculus/)
* [learn lambda calculus in y minutes](https://learnxinyminutes.com/docs/lambda-calculus/)
* [a tutorial introduction to lambda calculus](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf)
* [cornell CS312 on lambda calculus](http://www.cs.cornell.edu/courses/cs3110/2008fa/recitations/rec26.html)
* [an introduction to functional programming](https://codewords.recurse.com/issues/one/an-introduction-to-functional-programming)