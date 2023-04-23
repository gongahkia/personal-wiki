> *Edit on 23 April 2023:*
>
> Continue learning Haskell from *Learn you a Haskell for Great Good!* Chapter 5 onwards when I am back from Haskell burnout!

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

```haskell
max 100 101 
-- returns 101 to the stdout
```

* Functions are defined by writing **function name**, **parameters**, `=` and **function body**

```haskell
doubleMe x = x + x
doubleUs x y = x*2 + y*2
```

* Functions that *don't take any parameters* are called <u>**definitions**</u>.

```haskell
conanO'Brien = "It's a me, Conan O'Brien!" -- this is a definition
```

#### `if` statements

* `If` statements in Haskell are **expressions** *(which always evaluate to something)*, so `else` blocks are madantory.
    * Note the `if`, `then`, `else` block syntax.

```haskell
doubleSmallNumber x = if x > 100
                      then x 
                      else x*2 
                      -- else blocks are madantory in Haskell if expressions 

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1 
-- the backtick ' denotes a strict version of the function / slightly modified version
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

```haskell
removeNonUppercase :: [Char] -> [Char] 
-- explicit type declaration for the previously covered function that removes all uppercase letters
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int 
-- there is no distinction between the parameters and return type, they are all separated by ->
addThree x y z = x + y + z
```

<h4 align="center">Type variables</h4>

**Type variables** are Haskell's equivalent to *generics* in other languages, allowing us to write <u>general functions</u> if they don't have type-specific behaviour.

* Functions with **type variables** are called <u>*Polymorphic functions*</u>.

```console
ghci> :t head
head :: [a] -> a 
-- the head function has type variables, and is an example of a polymorphic function

ghci> :t fst
fst :: (a,b) -> a 
-- the fst function also implements type variables, and is another polymorphic function
```

> We normally give type variables *single-character* names like 'a', 'b', 'c', 'd', though they can be given other non-capitalised names.

<h4 align="center">Typeclasses</h4>

**Typeclasses** are similar to Java *Interfaces*, except better.

* Types that are <u>part of a</u> Typeclass **implement** behavior prescribed by the Typeclass.
* `=>` separates **class constraints** and the function's **parameters** and **return types**.

> Class constraints can be applied to both a function's *parameters* and its *return types*.

```console
ghci> :t (==)
(==) :: (Eq a) => a -> a -> Bool

ghci> :t (>)
(>) :: (Ord a) => a -> a -> Bool
```

> In this example, the equality function takes any two values that are the same type *(`a -> a`)*, and the type must be a member of the `Eq` class, a.k.a the **class constraint** *(`(Eq a)`)*, returning a Bool.
> 
> Similarly, the comparison function takes any two values that are the same type *(`a -> a`)*, and the type must be a member of the `Ord` class, a.k.a the **class constraint** *(`(Ord a)`)*, returning a Bool.

<h4 align="center">Explicit type annotations</h4>

This was somewhat covered above, but we employ **explicit type annotation** with the `::` operator where needed with *certain Haskell functions*.

```console
ghci> :t read
read :: (Read a) => String ->  a
```

> In this example, the read function takes a single value of type String *(`String`)*, and returns the specified type *(`a`)*, where the type must be a member of the `Read` class, a.k.a the **class constraint** *(`(Read a)`)*.
> 
> However, since the return type hasn't been specified, Haskell has no way of knowing what type to assign `a`. 
> 
> Here, we can use **explicit type annotation** to inform Haskell of our desired return type.

```haskell
convertThatString = read "5" :: Int 
-- returns an Int of value 5

convertThatList = read "[1,2,3,4]" :: [Int] 
-- returns a List of Ints, [1,2,3,4]
```

<h4 align="center">Enumerations</h4>

**Enumeration members** are <u>sequentially-ordered</u> types *(part of the `Enum` typeclass)* that can be enumerated, allowing for Haskell's <u>list ranges</u>.

```haskell
exampleRange = ['a'..'e'] 
-- equivalent to "abcde"

anotherExampleRange = [LT..GT] 
-- equivalent to [LT,EQ,GT]

yetAnotherExampleRange = [3..5] 
-- equivalent to [3,4,5]
```

**Enumeration members** also have defined *successors* and *predecesors*.

* `succ` returns the **sucessor** of a given enumeration member.
* `pred` returns the **predecesor** of a given enumeration member.

```haskell
aFinalExampleRange = ['A'..'Z']

singleCharSucc = succ 'E' 
-- returns a Char of value 'F'

singleCharPred = pred 'F' 
-- returns a Char of value 'E'
```

---

<h3 align="center">Data Structures</h3>

#### Lists 

Lists are a **homogenous** data structure *(storing elements of <u>same type</u>)*.

> Strings are lists too!

* Lists are denoted by `[]` square brackets and elements are *comma-separated*.

```haskell
lostNumbers = [1,2,3,4,5]
greeting = "hello brother"
```

* `++` operator <u>**concatenates** two lists</u> *(Haskell has to traverse the entire left list before processing the concatenation)*.

```haskell
yesList = [1,2,3,4,5] ++ [6,7,8,9,10] 
-- returns [1,2,3,4,5,6,7,8,9,10]

yesGreeting = "hello" ++ " " ++ "uncle" 
-- returns "hello uncle"
```

* `:` cons operator <u>instantaneously **prepends** a value to a list</u>.

```haskell
observation = 'A':" small cat" 
-- this returns "A small cat" at a significantly faster speed than pure concatenation
```

* `!!` operator <u>**extracts list elements** by index</u>.

```haskell
theHero = "Steve Buscemi" !! 6 
-- this returns character 'B', which has an index of 6
```

* `<`, `>` `==` <u>**compares list elements** in [**lexographical order**](https://stackoverflow.com/questions/45950646/what-is-lexicographical-order)</u>.

```haskell
booleanYesOrNo = [3,4,2] > [3,2] 
-- returns True
```

<h4 align="center">Other List functions</h4>

* `head` returns a **list's head**.

```haskell
theHead = head [5,4,3,2,1] 
-- returns 5
```

* `tail` returns a **list's tail** *(by chopping off the list's head)*.

```haskell
theTail = tail [5,4,3,2,1] 
-- returns [4,3,2,1] as a list
```

* `last` returns **last element of list**.

```haskell
theLastElement = last [1,2,3,4,5] 
-- returns 5
```

* `init` returns **everything except list's last element**.

```haskell
theInit = init [5,4,3,2,1] 
-- returns [5,4,3,2] as a list
```

* `length` returns **length of list**.

```haskell
theLength = length [1,2,3,4,5] 
-- returns 5
```

* `null` checks whether **list is empty** *(returns a boolean value)*.

```haskell
theNullCheck = null [] 
-- returns True

anotherNullCheck = null [1,2,3] 
-- returns False
```

* `reverse` **reverses a list**.

```haskell
theReversedList = reverse [5,4,3,2,1] 
-- returns [1,2,3,4,5] as a list
```

* `take` **extracts** a specified number of elements from the start of a list.

```haskell
extractedList = take 3 [1,2,3,4,5] 
-- returns [1,2,3]

anotherExtractedList = take 0 [6,6,6] 
-- returns [] an empty list

yetAnotherExtractedList = take 5 [1,2] 
-- returns [1,2], the entire list
```

* `drop` **drops** a specified number of elements from the start of a list, and returns the remaining elements.

```haskell
droppedList = drop 3 [1,2,3,4,5,6,7,8] 
-- returns [4,5,6,7,8]

anotherDroppedList = drop 0 [1,2,3,4] 
-- returns [1,2,3,4], the entire list

yetAnotherDroppedList = drop 100 [1,2,3,4] 
-- returns [], an empty list
```

* `maximum` returns element of **maximum value** in a list.

```haskell
maximumValue = maximum [1,2,3,4,5,83,10] 
-- returns 83
```

* `minimum` returns element of **minimum value** in a list.

```haskell
minimumValue = minimum [0,2,200,1,7] 
-- returns 0
```

* `sum` returns the **sum** of all elements in a list.

```haskell
sumOfValues = sum [2,3,5,129] 
-- returns 139
```

* `product` returns the **product** of all elements in a list.

```haskell
productOfValues = product [2,3,10,4] 
-- returns 240
```

* `elem` checks whether an element is **part of a list** *(returns a boolean value)*.

```haskell
isItPartOfList = 4 `elem` [3,4,5,6] 
-- returns True

isItAlsoPartOfList = 10 `elem` [3,4,5,6] 
-- returns False
```

#### Tuples

Tuples are a **non-homogenous** data structure *(storing elements of <u>different types</u>)* that can store a <u>**fixed** number of elements</u>.

* Tuples are denoted by `()` normal brackets and elements are *comma-separated*.

```haskell
someTuple = (8, 11) 
-- a definition of the tuple someTuple
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

```haskell
rangeOfOneToTwenty = [1..20] 
-- returns a complete list of [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

rangeOfaToz = ['a'..'z'] 
-- returns a complete list of letters (string) of "abcdefghijklmnopqrstuvwxyz"
```

* We can **specify steps** in ranges as well.

```haskell
rangeOfOneToTwentyButEvenOnly = [2,4..20] 
-- returns a complete list of even numbers [2,4,6,8,10,12,14,16,18,20]

rangeOfOneToTwentyButMultiplesOfThree = [3,6..20] 
-- returns a complete list of [3,6,9,12,15,18]
```

* We can also obtain **infinite lists** using ranges.

```haskell
firstTwentyFourElementsOfThirteen = take 24 [13,26..] 
-- since Haskell lazily evaluates, it only calculates what is needed when specified by us in the program
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

$$ S = \{2.x|x\in N,x<=10\} $$

Wherein <u>each component</u> is as follows.

$$ S = \{OutputFunction|VariableAndInputSet,Predicate\} $$

> * **Output function:** Transformation applied on each value before it is output.  
> * **Variable:** Variable that iterates over the input set.  
> * **Input set:** Input set which the variable can iterate over, can be expressed as a range.  
> * **Predicate:** Conditions *imposed on the variable* that limit what values from the input set can be output.  

The same concept *(and terminology)* can be applied in Haskell's list comprehension, with `[{Output function} | {Variable & Input set}, {Predicate}]` in the same format.

* `<-` signals the relationship between the **variable** and **input set**.

```haskell
desiredList = [x*2 | x <- [1..10]] 
-- returns the complete list [2,4,6,8,10,12,14,16,18,20]

anotherDesiredList = [x*2 | x <- [1..10], x*2 >= 12] 
-- returns the complete list [12,14,16,18,20]

yetAnotherDesiredList = [x | x <- [50..100], x `mod` 7 == 3] 
-- returns the complete list of numbers from 50 to 100 whose remainder when divided by 7 is 3, [52,59,66,73,80,87,94]
```

* `,` commas can be used to separate **multiple predicates**.

```haskell
okAndHere'sAnotherList = [x | x <- [10..20], x /= 13, x /= 15, x /= 19] 
-- multiple predicates that state we don't want 13, 15, 19 from our input set
```

* `,` commas can also be used to separate **multiple variables and input sets**.

```haskell
ShagLaBro = [x*y | x <- [2,5,10] , y <- [8,10,11], x*y > 50] 
-- this returns a list of all the possible products from a list that are more than 50, which is [55,80,100,110]
```

We can embed list comprehension inside *functions* as well.

```haskell
boomBangs userInput = [if x < 10 then "Boom!" else "Bang!" | x <- userInput, odd x] 
-- where odd and even are functions that return True or False depending on whether said number is true or false

boomBangs [7..13] 
-- this will return the complete list ["Boom!", "Boom!", "Bang!", "Bang!"]
```

* `_` can be used as a **throwaway** variable that will not be referenced in the future.

```haskell
length` xs = sum [1 | _ <- xs] 
-- this function replaces every element of the list with 1, and sums up the values of that list
```

About 200 lines ago, we established that **Strings** are *lists of characters*. Therefore, *list comprehension* can similarly be applied on Strings.

```haskell
removeNonUppercase inputString = [ c | c <- inputString, c `elem` ['A'..'Z']] 
-- removes all non-uppercase letters from a string
```

> **Nested list comprehension** is also possible.

---

To provide some context before this section begins, Haskell has *2 integral types*, as follows...

* `Int`
* `Integer`

> More on this topic can be found [here](https://www.cantab.net/users/antoni.diller/haskell/units/unit01.html).

<h3 align="center">Function syntax</h3>

<h4 align="center">Pattern Matching</h4>

**Pattern matching** is Haskell's overpowered version of the *switch, case* statement in other languages.

Here are some rules when creating patterns...

1. Patterns are checked from **top to bottom**.
2. As such, the **order is important** when specifying patterns.
3. Always include a **catch-all** pattern to prevent our program from crashing due to unexpected input.

> Here is *pattern matching's* application in a function.

```haskell
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayme x = "Not between 1 and 5!"
-- sayme x is a catch-all pattern
```

Note that the final `sayme x` acts as a **catch-all** pattern *(equivalent of a default statement)* to ensure the function (an expression) <u>evaluates to a value</u> no matter the situation.

> Recursion (another important concept in Haskell) can also be applied in conjunction with **pattern-matching**.
>
> Here is *pattern-matching's* application in a recursive function.

```haskell
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)
```

Note that the above only works because of the <u>order of the patterns</u>.   

For an in-depth explanation of this function, refer to page 31 of [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/).

> We are also able to use *pattern-matching* alongside list comprehension. Should a pattern match fail, it moves on to the next element in the list.

```console
ghci> let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
ghci> [a+b | (a,b) <- xs]
[4,7,6,8,11,4]
```

---

<h4 align="center"><a href="http://learnyouahaskell.com/syntax-in-functions">Guards</a></h4>

---

<h4 align="center"><a href="http://learnyouahaskell.com/syntax-in-functions">Where</a></h4>

---

<h4 align="center"><a href="http://learnyouahaskell.com/syntax-in-functions">Let</a></h4>

---

<h4 align="center"><a href="http://learnyouahaskell.com/syntax-in-functions">Case</a></h4>

---
