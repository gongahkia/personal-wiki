> Referencing *Learn you a Haskell for Great Good!* pg 11

# The Haskell programming language

![](https://upload.wikimedia.org/wikipedia/en/thumb/4/4d/Logo_of_the_Haskell_programming_language.svg/2560px-Logo_of_the_Haskell_programming_language.svg.png)

## What is Haskell?

Haskell is a <u>strictly **functional**</u> programming language. This entails...

* Functions have ***no side-effects***.
* Functions calculate something and return a result *(referential transparency)*.

Haskell is <u>**lazy**</u>. This entails...

* Haskell ***won't*** execute a function unless it is forced to display its result.

Haskell is <u>**statically typed**</u>, but supports *type inference*. This entails...

* Haskell's compiler can catch most type errors at compile time.

> On the [differences between functional and imperative languages](https://stackoverflow.com/questions/17826380/what-is-difference-between-functional-and-imperative-programming-languages).

---

## Quickstart

### [Installation](https://www.haskell.org/downloads/)

### Usage

Read the second chapter of [*Learn you a Haskell for Great Good!*](http://learnyouahaskell.com/)

--- 

### Functions in Haskell

Haskell is built around functions *(which are built upon expressions)*, so let's start there.

> Functions can be quickly tested out in `ghci` mode with `:l {file name}`, which compiles the Haskell file.

* Functions are called by writing **function name** and **parameters** *(seperated by spaces)*

```Haskell
max 100 101 -- returns 101 to the stdout
```

* Functions are defined by writing **function name**, **parameters**, `=` and **function body**

```Haskell
doubleMe x = x + x
doubleUs x y = x*2 + y*2
```

* Functions that *don't take any parameters* are called <u>**definitions**</u>.

```haskell
conanO'Brien = "It's a me, Conan O'Brien!" -- this is a definition
```

#### `if` statements

* `If` statements in Haskell are **expressions** *(which always evaluate to something)*, so `else` blocks are madantory.

```Haskell
doubleSmallNumber x = if x > 100
                      then x 
                      else x*2 -- else portions are madantory in Haskell if expressions 

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1 -- the backtick ' denotes a strict version of the function / slightly modified version
```

---

### Data Types

* Characters *(`''` single quotes)*
* Strings == Lists of Characters *(`""` double quotes)*
* Boolean *(`True` or `False`)*

---

### Data Structures

#### Lists 

Lists are a **homogenous** data structure *(storing elements of <u>same type</u>)*.

> Strings are lists too!

* Lists are denoted by `[]` square brackets and elements are *comma-seperated*.

```Haskell
lostNumbers = [1,2,3,4,5]
greeting = "hello brother"
```

* `++` operator <u>**concatenates** two lists</u> *(Haskell has to traverse the entire left list before processing the concatenation)*.

```Haskell
yesList = [1,2,3,4,5] ++ [6,7,8,9,10] -- returns [1,2,3,4,5,6,7,8,9,10]
yesGreeting = "hello" ++ " " ++ "uncle" -- returns "hello uncle"
```

* `:` cons operator <u>instantaneously **prepends** a value to a list</u>.

```Haskell
observation = 'A':" small cat" -- this returns "A small cat" at a significantly faster speed than pure concatenation
```

* `!!` operator <u>**extracts list elements** by index</u>.

```Haskell
theHero = "Steve Buscemi" !! 6 -- this returns character 'B', which has an index of 6
```

* `<`, `>` `==` <u>**compares list elements** in [**lexographical order**](https://stackoverflow.com/questions/45950646/what-is-lexicographical-order)</u>.

```Haskell
booleanYesOrNo = [3,4,2] > [3,2] -- returns True
```

##### Other List functions

* `head` returns a **list's head**.

```Haskell
theHead = head [5,4,3,2,1] -- returns 5
```

* `tail` returns a **list's tail** *(by chopping off the list's head)*.

```Haskell
theTail = tail [5,4,3,2,1] -- returns [4,3,2,1] as a list
```

* `last` returns **last element of list**.

```Haskell
theLastElement = last [1,2,3,4,5] -- returns 5
```

* `init` returns **everything except list's last element**.

```Haskell
theInit = init [5,4,3,2,1] -- returns [5,4,3,2] as a list
```

* `length` returns **length of list**.

```Haskell
theLength = length [1,2,3,4,5] -- returns 5
```

* `null` checks whether **list is empty** *(returns a boolean value)*.

```Haskell
theNullCheck = null [] -- returns True
anotherNullCheck = null [1,2,3] -- returns False
```

* `reverse` **reverses a list**.

```Haskell
theReversedString = reverse [5,4,3,2,1] -- returns [1,2,3,4,5] as a list
```

---
