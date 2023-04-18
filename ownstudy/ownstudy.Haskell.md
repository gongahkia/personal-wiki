> Referencing *Learn you a Haskell for Great Good!* Chapter 3.3 Typeclasses 101

# The Haskell programming language

![](https://upload.wikimedia.org/wikipedia/en/thumb/4/4d/Logo_of_the_Haskell_programming_language.svg/2560px-Logo_of_the_Haskell_programming_language.svg.png)

<h2 align="center">What is Haskell?</h2>

Haskell is a <u>strictly **functional**</u> programming language. This entails...

* Functions have ***no side-effects***.
* Functions calculate something and return a result *(referential transparency)*.

Haskell is <u>**lazy**</u>. This entails...

* Haskell ***won't*** execute a function unless it is forced to display its result.

Haskell is <u>**statically typed**</u>, but supports *type inference*. This entails...

* Haskell's compiler can catch most type errors at compile time.

> On the [differences between functional and imperative languages](https://stackoverflow.com/questions/17826380/what-is-difference-between-functional-and-imperative-programming-languages).

---

<h2 align="center">Quickstart</h2>

### [Installation](https://www.haskell.org/downloads/)

### Usage

> Read the second chapter of [*Learn you a Haskell for Great Good!*](http://learnyouahaskell.com/)

--- 

<h3 align="center">Functions in Haskell</h3>

Haskell is built around functions *(which are built upon expressions)*, so let's start there.

> Functions can be quickly tested out in `ghci` mode with `:l {file name}`, which compiles the Haskell file.

* Functions are called by writing **function name** and **parameters** *(separated by spaces)*

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

<h3 align="center">Data Types</h3>

Types are written in capital case.

* Whole numbers
    * `Int` *(<u>**bounded**</u>, having a minimum and maximum value)*
    * `Integer` *(<u>**not bounded**</u>, can represent really large numbers)*

* Floating points
    * `Float` *(<u>**single precision**</u>, represented by 32 bits)*
    * `Double` *(<u>**double precision**</u>, represented by 64 bits)*

* `Bool` *(True or False)*
* `Char` *(denoted by single quotes)*
* `String` == `[Char]` *(denoted by double quotes)*

Functions are accompanied by <u>explicit type declaration</u> for its **parameters** and **return types** as well.

* `::` is read as "has the type of".
* `->` separates each parameter and the return type *(there is no special distinction between the parameter and return value's data type)*.

```Haskell
removeNonUppercase :: [Char] -> [Char] -- explicit type declaration for the previously covered function that removes all uppercase letters
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int -- there is no distinction between the parameters and return type, they are all separated by ->
addThree x y z = x + y + z
```

<h4 align="center">Type variables</h4>

**Type variables** are Haskell's equivalent to *generics* in other languages, allowing us to write <u>general functions</u> if they don't have type-specific behaviour.

* Functions with **type variables** are called <u>*Polymorphic functions*</u>.

```console
ghci> :t head
head :: [a] -> a -- the head function has type variables, and is an example of a polymorphic function

ghci> :t fst
fst :: (a,b) -> a -- the fst function also implements type variables, and is another example of a polymorphic function
```

> We normally give type variables *single-character* names like 'a', 'b', 'c', 'd', though they can be given other non-capitalised names.

---

<h3 align="center">Data Structures</h3>

#### Lists 

Lists are a **homogenous** data structure *(storing elements of <u>same type</u>)*.

> Strings are lists too!

* Lists are denoted by `[]` square brackets and elements are *comma-separated*.

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

<h4 align="center">Other List functions</h4>

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
theReversedList = reverse [5,4,3,2,1] -- returns [1,2,3,4,5] as a list
```

* `take` **extracts** a specified number of elements from the start of a list.

```Haskell
extractedList = take 3 [1,2,3,4,5] -- returns [1,2,3]
anotherExtractedList = take 0 [6,6,6] -- returns [] an empty list
yetAnotherExtractedList = take 5 [1,2] e-- returns [1,2], the entire list
```

* `drop` **drops** a specified number of elements from the start of a list, and returns the remaining elements.

```Haskell
droppedList = drop 3 [1,2,3,4,5,6,7,8] -- returns [4,5,6,7,8]
anotherDroppedList = drop 0 [1,2,3,4] -- returns [1,2,3,4], the entire list
yetAnotherDroppedList = drop 100 [1,2,3,4] -- returns [], an empty list
```

* `maximum` returns element of **maximum value** in a list.

```Haskell
maximumValue = maximum [1,2,3,4,5,83,10] -- returns 83
```

* `minimum` returns element of **minimum value** in a list.

```Haskell
minimumValue = minimum [0,2,200,1,7] -- returns 0
```

* `sum` returns the **sum** of all elements in a list.

```Haskell
sumOfValues = sum [2,3,5,129] -- returns 139
```

* `product` returns the **product** of all elements in a list.

```Haskell
productOfValues = product [2,3,10,4] -- returns 240
```

* `elem` checks whether an element is **part of a list** *(returns a boolean value)*.

```Haskell
isItPartOfList = 4 `elem` [3,4,5,6] -- returns True
isItAlsoPartOfList = 10 `elem` [3,4,5,6] -- returns False
```

#### Tuples

Tuples are a **non-homogenous** data structure *(storing elements of <u>different types</u>)* that can store a <u>**fixed** number of elements</u>.

* Tuples are denoted by `()` normal brackets and elements are *comma-separated*.

```Haskell
someTuple = (8, 11) -- a definition of the tuple someTuple
```
  
> Tuples are differentiated based on the **<u>size</u> of the tuple** and the **<u>data type</u> of its elements**, and each permutation of those two factors is its own *data type*. 
>
> As such, there are **theoretically** an infinite number of tuple types.

<h4 align="center">Other Tuple functions to look into</h4>

* [`fst`](http://zvon.org/other/haskell/Outputprelude/fst_f.html)
* [`snd`](http://zvon.org/other/haskell/Outputprelude/snd_f.html)
* [`zip`](http://zvon.org/other/haskell/Outputprelude/zip_f.html)

> See pg 18 of *Learn you a Haskell for Great Good!* for a fantastic example on tuples and list comprehension.

---

<h3 align="center">Ranges</h3>

Ranges let us make lists that are *arithmetic sequences of elements* which can be enumerated.

> **Numbers** and **Characters** can be enumerated.

* `..` **creates a range** that can be enumerated over.

```Haskell
rangeOfOneToTwenty = [1..20] -- returns a complete list of [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
rangeOfaToz = ['a'..'z'] -- returns a complete list of letters (string) of "abcdefghijklmnopqrstuvwxyz"
```

* We can **specify steps** in ranges as well.

```Haskell
rangeOfOneToTwentyButEvenOnly = [2,4..20] -- returns a complete list of even numbers [2,4,6,8,10,12,14,16,18,20]
rangeOfOneToTWentyButMultiplesOfThree = [3,6,20] -- returns a complete list of [3,6,9,12,15,18]
```

* We can also obtain **infinite lists** using ranges.

```Haskell
firstTwentyFourElementsOfThirteen = take 24 [13,26] -- since Haskell lazily evaluates, it only calculates what is needed when specified by us in the program
```

<h4 align="center">Other Range functions to look into</h4>

* [`cycle`](https://livebook.manning.com/concept/haskell/cycle)
* [`repeat`](http://zvon.org/other/haskell/Outputprelude/repeat_f.html)
* [`replicate`](http://zvon.org/other/haskell/Outputprelude/replicate_f.html)

---

<h3 align="center">List comprehension</h3>

We can implement list comprehension within Haskell, and it functions the same as in mathematical set theory.

The structure for list comprehension is as follows.

***Example:***

*A comprehension for a set that contains the first 10 even natural numbers.*

$$S = \{2.x|x\in N,x<=10\}$$

Wherein <u>each component</u> is as follows.

$$S = \{OutputFunction|VariableAndInputSet,Predicate\}$$

> * **Output function:** Transformation applied on each value before it is output.  
> * **Variable:** Variable that iterates over the input set.  
> * **Input set:** Input set which the variable can iterate over, can be expressed as a range.  
> * **Predicate:** Conditions *imposed on the variable* that limit what values from the input set can be output.  

The same concept *(and terminology)* can be applied in Haskell's list comprehension, with `[{Output function} | {Variable & Input set}, {Predicate}]` in the same format.

* `<-` signals the relationship between the **variable** and **input set**.

```Haskell
desiredList = [x*2 | x <- [1..10]] -- returns the complete list [2,4,6,8,10,12,14,16,18,20]
anotherDesiredList = [x*2 | x <- [1..10], x*2 >= 12] -- returns the complete list [12,14,16,18,20]
yetAnotherDesiredList = [x | x <- [50..100], x `mod` 7 == 3] -- returns the compleye list of numbers from 50 to 100 whose remainder when divided by 7 is 3, [52,59,66,73,80,87,94]
```

* `,` commas can be used to separate **multiple predicates**.

```Haskell
okAndHere'sAnotherList = [x | x <- [10..20], x /= 13, x /= 15, x /= 19] -- multiple predicates that state we don't want 13, 15, 19 from our input set
```

* `,` commas can also be used to separate **multiple variables and input sets**.

```Haskell
ShagLaBro = [x*y | x <- [2,5,10] , y <- [8,10,11], x*y > 50] -- this returns a list of all the possible products from a list that are more than 50, which is [55,80,100,110]
```

We can embed list comprehension inside *functions* as well.

```Haskell
boomBangs userInput = [if x < 10 then "Boom!" else "Bang!" | x <- userInput, odd x] -- where odd and even are functions that return True or False depending on whether said number is true or false
boomBangs [7..13] -- this will return the complete list ["Boom!", "Boom!", "Bang!", "Bang!"]
```

* `_` can be used as a **throwaway** variable that will not be referenced in the future.

```Haskell
length` xs = sum [1 | _ <- xs] -- this function replaces every element of the list with 1, and sums up the values of that list
```

About 200 lines ago, we established that **Strings** are *lists of characters*. Therefore, *list comprehension* can similarly be applied on Strings.

```Haskell
removeNonUppercase inputString = [ c | c <- inputString, c `elem` ['A'..'Z']] -- removes all non-uppercase letters from a string
```

> **Nested list comprehension** is also possible.

---
