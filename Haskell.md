# `Haskell`

Strict statically-typed functional language.

## Comments

```hs
-- ---------- COMMENT ----------

-- this is a single-line comment

{-
this 
is a 
multi-line
comment
-}
```

## Printing

```hs
-- ---------- PRINT ----------
    -- putStrLn => prints a string to the stdout and appends a newline to the output
    -- putStr => prints a string to the stdout and does not include a newline
    -- print => prints a value of the Show typeclass to the stdout and appends a newline to the output

main :: IO ()
main = do 
    putStrLn "this watermelon statement includes a newline at the end automatically"
    putStr "this pineapple statement does not include a newline at the end and must be specified explicitly by us\n"
```

## Quickstart

```hs
-- ---------- QUICKSTART ----------
    -- functional programming language with no side-effects, where everything is an expression that must evaluate to a value, including function definitions
    -- lazy evaluation, where functions will only be evaluated when called 
    -- executable files require a main function to be called, which serves as the entry point for the program allowing for file IO (classified as a side-effect in pure functional languages)

main :: IO ()
main = putStr "fear and hunger truly is a game"
```

## Functions and Definitions

```hs
-- ---------- FUNCTION ----------
    -- as a functional language, Haskell revolves around functions, which are all expressions that evaluate to a value
    -- function defined with the syntax => {FUNCTION NAME} {FUNCTION PARAMETERS SPACE DELIMITED} = {FUNCTION BODY}
    -- functions called with the syntax => {FUNCTION NAME} {FUNCTION ARGUMENTS SPACE DELIMITED}
    -- function parameter and return type type signatures are optionally declared before function defintion, but encouraged for clear code

max 100 101 -- this evaluates to the Int value 101

doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = x*2 + y*2

-- ---------- DEFINITION ----------
    -- as a functional language, Haskell has no concept of 'variables' and every binding is immutable by default since modification of a value binding entails side-effects and instead, every value can be modified through a function expression which evaluates to a transformed return value
    -- these are called definitions, the equivalent of constant bindings in other languages
    -- let => creates a local binding within a specified lexical scope that can only be accessed within its own local scope
    -- raw definition creates a global binding that can be accessed anywhere in the program

let jimmyFallon = "It's a me, Jimmy Fallon!" -- this creates a local binding
conanOBrien = "It's a me, Conan O'Brien!" -- this creates a global binding
```

## Types

```hs
-- ---------- TYPE ----------
    -- Int => bounded integer number with a minimum and maximum value
    -- Integer => unbounded integer number with no minimum and maximum value
    -- Float => single precision floating point number, 32 bits
    -- Double => double precision floating point number, 64 bits
    -- Bool => True, False
    -- Char => single quotation marks
    -- String => double quotation marks, a string list

-- TYPE SIGNATURES
    -- functions have their parameter and return type's type signatures explicitly declared before the function definition
    -- :: => means "has the type of"
    -- -> => seperates each parameter type and return type with a syntax very similar to Clojure

removeNonUppercase :: [Char] -> [Char] -- this function removes all uppercase characters
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int -- this function receives three arguments of type Int and returns a value of type Int
addThree x y z = x + y + z

-- TYPE VARIABLES
    -- used in type signatures for functions that don't have type-specific behaviour and accept arguments of multiple types, Haskell's equivalent of generics in other languages
    -- these functions are called polymorphic functions
    -- type variables are specified with single-character names like 'a' as seen below

head :: [a] -> a -- the type signature of the head function employs a type variable since its behaviour could apply to tuples of Int, Char, Bool, Float or any other type
fst :: (a,b) -> a -- similarly, the type signature of the fst function employs a type variables since its behaviour is not type-specific

-- TYPECLASSES
    -- specify behaviour for types by prescribing them within the typeclass, allowing for a degree of meta-programming similar to interfaces in other languages and metatables in Lua
    -- behaviours specified through class constraints which are decalred in () brackets within the function's type signature before the function's parameter and return types
    -- => => seperates class constraints from the function's parameters and return types

(==) :: (Eq a) => a -> a -> Bool -- the equality function's type signature (a -> a) specifies that it receives any two values that are the same type as arguments and returns a Boolean, while the typeclass specifies that the argument's type must be a member of the Eq class (Eq a)
(>) :: (Ord a) => a -> a -> Bool -- the comparison function's type signature (a -> a) specifies that it receives any two values that are the same type as arguments and returns a Boolean, while the typeclass specifies that the argument's type must be a member of the Ord class (Ord a)
```

## Operators

```hs
-- ---------- OPERATOR ----------

-- ARITHMETIC OPERATORS
    -- + => addition
    -- - => subtraction
    -- * => multiplication
    -- / => division
    -- div => floor divison
    -- mod => modulo operator

-- LOGICAL OPERATORS
    -- && => and
    -- || => or
    -- not => logical not

-- COMPARISON OPERATORS
    -- == => partial equality check for value
    -- /= => partial inequality check for value
    -- > < <= >= are also comparsion operators
```

## Control structures

```hs
-- ---------- CONTROL STRUCTURE ----------

-- CONDITIONALS

-- IF THEN ELSE
    -- else => required in every if statement since every expression (including conditional expressions) must evalaute to a value

doubleSmallNumber :: (Int a, Ord a) => a -> a
doubleSmallNumber x = if x > 100
                      then x 
                      else x*2 

-- CASE OF _
    -- powerful pattern-matching construct similar to the match case statements in other languages
    -- cases are checked top to bottom so order matters
    -- _ => wildcard catch-all operator that acts as the equivalent of the default statement in other languages

numberAsString :: Int -> String -- static type declaration of an expression prior to expression initialisation
numberAsString num = case num of
    1 -> "One"
    2 -> "Two"
    3 -> "Three"
    _ -> "Unknown but also the catch-call wildcard operator"

-- LOOPS DON'T EXIST
    -- higher-order functions, recursion, list comprehension are used in place of imperative loop constructs like for or while loops, which Haskell does not have

-- HIGHER-ORDER FUNCTIONS
    -- map => applies a specified function on each element of an iterable structure and returns the transformed data structure
    -- filter => applies a specified predicate to each element of an iterable structure and returns the data structure with elements that fulfil the predicate
    -- foldl, foldr => reduces a list to a single value by repeatedly applying a specified binary function to each element of the list, the equivalent of reduce in Haskell
    -- zipWith => combines two lists with a specified function
    -- compose => declared with a . period, combines two functions into a new function
    -- flip => receives a function, reverses its first two arguments and returns that new function

squareList :: Int a => [a] -> [a]
squareList xs = map (\x -> x * x) xs -- calling map function

evenNumbers :: Integral a => [a] -> [a]
evenNumbers xs = filter even xs -- calling filter function

sumList :: Int a => [a] -> a
sumList xs = foldl (+) 0 xs -- calling reduce function

addLists :: Int a => [a] -> [a] -> [a]
addLists xs ys = zipWith (+) xs ys -- calling zipWith function

squareAndDouble :: Int a => a -> a
squareAndDouble = (*2) . (^2) -- calling compose function

subtractFrom :: Int a => a -> a -> a
subtractFrom = flip (-) -- calling flip function

-- RECURSION

sumList :: Int a => [a] -> a 
sumList [] = 0
sumList (x:xs) = x + sumList xs -- simple recursion to sum the elements of a list

-- LIST COMPREHENSION
    -- given Haskell's mathematical roots, concepts like list comprehension originate from set theory
    -- list comprehension syntax => [{OUTPUT FUNCTION} | {VARIABLE AND INPUT SET}, {PREDICATE}]
        -- output function => function applied on each element
        -- variable => element iterated over within the input set, commas can be used to delimit multiple variables
        -- input set => iterable collection over which the variable iterates over, can be expressed as a range in Haskell, commas can be used to delimit multiple input sets
        -- predicate => specifies conditional check on the variable that limits what values from the input set can have the output function applied to them, the equivalent of a filter in other programming languages, commas can be used to delimit multiple predicates
    -- <- => specifies the relationship between the variable and input set
    -- _ => catch-all operator also acts as a throwaway variable that is not needed later

multiplyByTwo = [x*2 | x <- [1..10]] -- this evaluates to the Int list of [2,4,6,8,10,12,14,16,18,20] using list comprehension
multiplyByTwoWithPredicate = [x*2 | x <- [1..10], x*2 >= 12] -- this evaluates to the Int list of [12,14,16,18,20] using list comprehension when x fulfills the predicate of 2 * x is bigger or equals to 12
applyAnotherPredicate = [x | x <- [50..100], x `mod` 7 == 3]  -- this evaluates to the Int list of [52,59,66,73,80,87,94] using list comprehension where x fulfills the predicate of x mod 7 == 3
applyMultiplePredicates = [x | x <- [10..20], x /= 13, x /= 15, x /= 19] -- commas are used to delimit multiple predicates that specify x cannot be equals to 13, 15, 19
applyMultipleVariableAndInputSets = [x*y | x <- [2,5,10] , y <- [8,10,11], x*y > 50] -- commas are used to delimit multiple variable and inpiut sets to evaluate to a list of all the possible products from a list that are more than 50, which is [55,80,100,110]
removeNonUppercase inputString = [ c | c <- inputString, c `elem` ['A'..'Z']] -- Strings are just Char lists so we can use list comprehension to removes all non-uppercase letters from a string
length` xs = sum [1 | _ <- xs] -- _ catch-all wildcard operator used to signify throwing away that variable, so this function evaluates to the length of the list where every element of the list is replaced with 1 and the value of the Int list is summed up
squareList :: Int a => [a] -> [a] -- note this employs typeclasses for a generic function that does not have type-specific behaviour
squareList xs = [x * x | x <- xs] -- list comprehension can also be used with functions to generate an Int list where each value is its value squared
```

## Data structures

```hs
-- ---------- DATA STRUCTURE ----------

-- LIST
    -- ordered sequence of elements of the same type (so strings are char lists)
    -- declared with [] square brackets, elements are comma-delimited 

lostNumbers = [1,2,3,4,5] -- an Int list in Haskell

-- LIST OPERATIONS
    -- ++ => concatenates two lists together, though Haskell has to traverse the entire left list before processing the concatenation
    -- : => inserts a value to the start of the list immediately, much faster than concatenation
    -- !! => extracts a list element by index
    -- < > == => compares list elements in lexographical order
    -- head => returns the first element of a list
    -- tail => returns everything but the list's head
    -- last => returns the last element of a list
    -- init => returns everything but the list's last element
    -- length => returns the length of the list
    -- null => checks whether a list is empty and returns a Boolean
    -- reverse => returns a reversed copy of a list
    -- take => extracts a specified number of elements from the start of a list
    -- drop => drops a specified number of elements from the start of a list and returns the remaining elements as a sublist
    -- maximum => returns the element of maximum value in a list
    -- minimum => returns the element of minimum value in a list
    -- sum => returns the sum of all elements in a list
    -- product => returns the product of all elements in a list
    -- elem => checks whether a specified element is part of a list and returns a Boolean

yesList = [1,2,3,4,5] ++ [6,7,8,9,10] -- this evaluates to the list value [1,2,3,4,5,6,7,8,9,10]
yesGreeting = "hello" ++ " " ++ "uncle" -- this evaluates to the String (Char list) value "hello uncle"
observation = 'a':" small cat" -- this evaluates to the String (Char list) value "a small cat"
theHero = "Steve Buscemi" !! 6 -- this evaluates to the Char value 'B', which has an index of 6 in the String (Char list)
booleanYesOrNo = [3,4,2] > [3,2] -- this evaluates to Boolean True
theHead = head [5,4,3,2,1] -- this evaluats to the Int value 5
theTail = tail [5,4,3,2,1] -- this evaluates to the Int list of [4,3,2,1] 
theLastElement = last [1,2,3,4,5] -- this evaluates to the Int value 5
theInit = init [5,4,3,2,1] -- this evaluates to the Int list of [5,4,3,2]
theLength = length [1,2,3,4,5] -- this evaluates to the Int value 5
theNullCheck = null [] -- this evaluates to Boolean True
anotherNullCheck = null [1,2,3] -- this evaluates to Boolean False
theReversedList = reverse [5,4,3,2,1] -- this evaluates to the Int list of [1,2,3,4,5]
extractedList = take 3 [1,2,3,4,5] -- this evaluates to the Int list of [1,2,3]
anotherExtractedList = take 0 [6,6,6] -- this evaluates to the empty list of [] since 0 elements were extracted
yetAnotherExtractedList = take 5 [1,2] -- this evaluates to the Int list of [1,2] since the entire list was extracted
droppedList = drop 3 [1,2,3,4,5,6,7,8] -- this evaluates to the Int list of [4,5,6,7,8]
anotherDroppedList = drop 0 [1,2,3,4] -- this evaluates to the Int list of [1,2,3,4] since no values were dropped
yetAnotherDroppedList = drop 100 [1,2,3,4] -- this evaluates to the empty list of [] since all the values were dropped
maximumValue = maximum [1,2,3,4,5,83,10] -- this evaluates to the Int value of 83
minimumValue = minimum [0,2,200,1,7] -- this evaluates to the Int value of 0
sumOfValues = sum [2,3,5,129] -- this evaluates to the Int value of 139
productOfValues = product [2,3,10,4] -- this evaluates to the Int value of 240
isItPartOfList = 4 `elem` [3,4,5,6] -- this evaluates to Boolean True
isItAlsoPartOfList = 10 `elem` [3,4,5,6] -- this evaluates to Boolean False

-- TUPLE
    -- ordered sequence of elements of a fixed size that can store different types
    -- declared with () brackets, elements are comma-delimited

someTuple = (8, 11) -- an Int tuple in Haskell

-- TUPLE OPERATIONS
    -- fst => receives a pair (tuple with two elements) as an argument and returns the first element
    -- snd => receives a pair (tuple with two elements) as an argument and returns the second element
    -- zip => takes two lists and returns a list of pairs comprised of elements of corresponding positions in the input lists

firstElement = fst (1, 2) -- this evaluates to the Int value 1
secondElement = snd (1, 2) -- this evaluates to the Int value 2
list1 = [1, 2, 3]
list2 = ["one", "two", "three"]
zippedList = zip list1 list2 -- this evaluates to the Int tuple list value of [(1, "one"), (2, "two"), (3, "three")]
```

## More on

* ranges
* enumeration
* guards
* where
* let
* case
* [haskell mooc](https://haskell.mooc.fi/)
* [learn you a haskell for great good](https://learnyouahaskell.com/chapters)
* [a gentle introduction to haskell version 98](https://www.haskell.org/tutorial/)
* [install haskell](https://www.haskell.org/downloads/)
* [haskell.org](https://www.haskell.org/)
* [haskell documentation](https://www.haskell.org/documentation/)
* [learn haskell in y minutes](https://learnxinyminutes.com/docs/haskell/)
* [haskell programming from first principles](https://haskellbook.com/)
* [the haskell school of expression](https://www.cs.yale.edu/homes/hudak/SOE/index.htm)
* [haskell wiki on functional programming](https://wiki.haskell.org/Functional_programming)
