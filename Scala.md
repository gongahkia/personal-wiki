# `Scala`

>[!WARNING]
>add desription here

Scala is a 

## REPL

Scala features a REPL analogous to a commandline interpreter. Any Scala expression received will be evaluated and printed to the stdout.

```sh
# ----- Scala REPL -----

$ scala # initialises a Scala REPL as below
scala>

scala> 2 + 2 # each typed expression is automatically saved as a new numbered value
res0: Int = 4

scala> res0 + 2 # previously declared values can be called
res1: Int = 6

scala> :type (true, 2.0) # checking type of a literal without evaluating an expression
(Boolean, Double)

scala> :save /sites/repl-test.scala # saves a Scala REPL session

scala> :load /sites/repl-test.scala # load a Scala file into the REPL to automatically evaluate it
Loading /sites/repl-test.scala...
res2: Int = 4
res3: Int = 6

scala> :h? # browse REPL history to see past expressions entered
1 2 + 2
2 res0 + 2
3 :save /sites/repl-test.scala
4 :load /sites/repl-test.scala
5 :h?
```

## Comments

```scala
// ----- COMMENT -----

// this is a single-line comment

/* 
this is a 
multi-line
comment
*/
```

## Printing

```scala
// ----- PRINT -----
    // print() => print text to the stdout without a newline
    // println() => print text to the stdout with a newline

print("there's no newline and we must explicitly specify it\n")
println("this includes a newline automatically")
```

## Imports

```scala
// ----- IMPORT -----
    // a Scala file can contain multiple classes and objects
    // import => specifies packages, sub packages and classes to bring into the local scope of the current Scala file
    // {} => group multiple import items together
    // => => rename an import item within the local scope for easier reference
        // _ => can be combined with above to exclude specific import items from being imported

import scala.collection.immutable.List
import scala.collection.immutable._ // this is a sub package
import scala.collection.immutable.{List, Map} // import multiple classes at once
import scala.collection.immutable.{List => ImmutableList} // rename the import from List to ImmutableList
import scala.collection.immutable.{Map => _, Set => _, _} // import all classes except the specified Map and Set
```

## Input and Output

```scala
// ----- INPUT AND OUTPUT -----
    // scala.io.Source => package containing read and write methods, has to be imported
        // .fromFile() => opens file at specified file path
        // .getLines() => retrieves all the lines within a file and stores them within an iterable structure that can be iterated over with a loop
    // PrintWriter() => initialses a new PrintWriter write object on the file at the specified file path
        // util.Properties => package containing further formatting options for writing to the specified file
        // .write() => writes the designated text as a string to the file
        // .close() => closes the PrintWriter object

// --- READ ---

import scala.io.Source // required import
for(line <- Source.fromFile("myfile.txt").getLines())
    println(line)

// --- WRITE ---

val writer = new PrintWriter("myfile.txt")
writer.write("Writing line for line" + util.Properties.lineSeparator)
writer.write("Another line here" + util.Properties.lineSeparator)
writer.close()
```

## Quickstart

```scala
// ----- QUICKSTART -----
    // Scala is statically typed but affords type inference (Scala compiler determines the type of a given variable if not explicitly defined)
    // scalac => CLI command to compile the .scala file
    // val => declares an immutable value that cannot be reassigned
    // var => declares a mutable value that can be reassigned
    // : => declares the type of a variable

val x:Int = 10 
x = 20 // this causes an error
var y:Int = 10
var z:Double = 1.0
y = 20 // this is okay
z = 2.5 // this is also okay



// Your program's entry point is defined in a scala file using an object, with a
// single method, main:
object Application {
  def main(args: Array[String]): Unit = {
    // stuff goes here.
  }
}

```

## Types

```scala
// ----- TYPE -----
    // Scala features a rich type system consisting of both primitive and complex types

// --- PRIMITIVE TYPE --- 
    // Byte => 8-bit signed integer
    // Short => 16-bit signed integer
    // Int => 32-bit signed integer
    // Long => 64-bit signed integer
    // Float => 32-bit single-precision floating point number
    // Double => 64-bit double-precision floating point number
    // Char => 16-bit Unicode character declared with '' single quotes
    // Boolean => true or false
    // Unit => represents the absence of a value with only one instance (), equivalent to void in Java

// --- REFERENCE TYPE ---
    // String => sequence of Char primitives, immutable and backed by the Java String class, declared with "" double quotes
    // Option => data structure representing optional values including None
    // Tuple => data structure storing a fixed number of items of different types
    // List => immutable linked list
    // Seq => ordered sequence of elements
    // Set => unordered sequence of unique elements
    // Map => collection of key-value pairs

// --- ANY ---
    // Any => root type of all datatypes in Scala, allowing for Scala's type inference
    // AnyVal => parent type of all primitive types
    // AnyRef => parent type of all reference types
```

## Operators

```scala
// ----- OPERATOR -----

// ARITHMETIC OPERATORS
    // + => addition
    // - => subtraction
    // * => multiplication
    // / => divison
    // % => modulo

// LOGICAL OPERATORS
    // && => logical and
    // || => logical or
    // ! => logical not

// COMPARISON OPERATORS
    // == => complete equality check for value and type
    // != => complete inequality check for value and type
    // .isInstanceOf[] => mostly used to check if object is of a specific type
    // >, <, >=, <= are comparison operators and operate as you'd expect

// ASSIGNMENT OPERATORS
    // = => simple assignment
    // += => increment by specified value and reassign
    // -= => decrement by specified value and reassign
    // *= => multiply by specified value and reassign
    // /= => divide by specified value and reassign
```

## Control structures

```scala
// ----- CONTROL STRUCTURE -----

// ----- CONDITIONAL -----

// ----- LOOP -----


1 to 5
val r = 1 to 5
r.foreach(println)

r foreach println
// NB: Scala is quite lenient when it comes to dots and brackets - study the
// rules separately. This helps write DSLs and APIs that read like English

// Why doesn't `println` need any parameters here?
// Stay tuned for first-class functions in the Functional Programming section below!
(5 to 1 by -1) foreach (println)

// A while loop
var i = 0
while (i < 10) { println("i " + i); i += 1 }

while (i < 10) { println("i " + i); i += 1 }   // Yes, again. What happened? Why?

i    // Show the value of i. Note that while is a loop in the classical sense -
     // it executes sequentially while changing the loop variable. while is very
     // fast, but using the combinators and comprehensions above is easier
     // to understand and parallelize

// A do-while loop
i = 0
do {
  println("i is still less than 10")
  i += 1
} while (i < 10)

// Recursion is the idiomatic way of repeating an action in Scala (as in most
// other functional languages).
// Recursive functions need an explicit return type, the compiler can't infer it.
// Here it's Unit, which is analogous to a `void` return type in Java
def showNumbersInRange(a: Int, b: Int): Unit = {
  print(a)
  if (a < b)
    showNumbersInRange(a + 1, b)
}
showNumbersInRange(1, 14)


// Conditionals

val x = 10

if (x == 1) println("yeah")
if (x == 10) println("yeah")
if (x == 11) println("yeah")
if (x == 11) println("yeah") else println("nay")

println(if (x == 10) "yeah" else "nope")
val text = if (x == 10) "yeah" else "nope"


```

## Data structures

```scala
// ----- DATA STRUCTURE -----


// Tuple
val tuple: (Int, String, Boolean) = (1, "Scala", true)

// Option
val someValue: Option[Int] = Some(5)
val noValue: Option[Int] = None

// List
val list: List[Int] = List(1, 2, 3)

// Set
val set: Set[String] = Set("apple", "banana", "cherry")

// Map
val map: Map[String, Int] = Map("a" -> 1, "b" -> 2, "c" -> 3)

val a = Array(1, 2, 3, 5, 8, 13)
a(0)     // Int = 1
a(3)     // Int = 5
a(21)    // Throws an exception

val m = Map("fork" -> "tenedor", "spoon" -> "cuchara", "knife" -> "cuchillo")
m("fork")         // java.lang.String = tenedor
m("spoon")        // java.lang.String = cuchara
m("bottle")       // Throws an exception

val safeM = m.withDefaultValue("no lo se")
safeM("bottle")   // java.lang.String = no lo se

val s = Set(1, 3, 7)
s(0)      // Boolean = false
s(1)      // Boolean = true

/* Look up the documentation of map here -
 * https://www.scala-lang.org/api/current/scala/collection/immutable/Map.html
 * and make sure you can read it
 */


// Tuples

(1, 2)

(4, 3, 2)

(1, 2, "three")

(a, 2, "three")

// Why have this?
val divideInts = (x: Int, y: Int) => (x / y, x % y)

// The function divideInts gives you the result and the remainder
divideInts(10, 3)    // (Int, Int) = (3,1)

// To access the elements of a tuple, use _._n where n is the 1-based index of
// the element
val d = divideInts(10, 3)    // (Int, Int) = (3,1)

d._1    // Int = 3
d._2    // Int = 1

// Alternatively you can do multiple-variable assignment to tuple, which is more
// convenient and readable in many cases
val (div, mod) = divideInts(10, 3)

div     // Int = 3
mod     // Int = 1

```

## Functions

```scala
// ----- FUNCTION -----
    // 

// Functions are defined like so:
//
//   def functionName(args...): ReturnType = { body... }
//
// If you come from more traditional languages, notice the omission of the
// return keyword. In Scala, the last expression in the function block is the
// return value.
def sumOfSquares(x: Int, y: Int): Int = {
  val x2 = x * x
  val y2 = y * y
  x2 + y2
}

// The { } can be omitted if the function body is a single expression:
def sumOfSquaresShort(x: Int, y: Int): Int = x * x + y * y

// Syntax for calling functions is familiar:
sumOfSquares(3, 4)  // => 25

// You can use parameters names to specify them in different order
def subtract(x: Int, y: Int): Int = x - y

subtract(10, 3)     // => 7
subtract(y=10, x=3) // => -7

// In most cases (with recursive functions the most notable exception), function
// return type can be omitted, and the same type inference we saw with variables
// will work with function return values:
def sq(x: Int) = x * x  // Compiler can guess return type is Int

// Functions can have default parameters:
def addWithDefault(x: Int, y: Int = 5) = x + y
addWithDefault(1, 2) // => 3
addWithDefault(1)    // => 6


// Anonymous functions look like this:
(x: Int) => x * x

// Unlike defs, even the input type of anonymous functions can be omitted if the
// context makes it clear. Notice the type "Int => Int" which means a function
// that takes Int and returns Int.
val sq: Int => Int = x => x * x

// Anonymous functions can be called as usual:
sq(10)   // => 100

// If each argument in your anonymous function is
// used only once, Scala gives you an even shorter way to define them. These
// anonymous functions turn out to be extremely common, as will be obvious in
// the data structure section.
val addOne: Int => Int = _ + 1
val weirdSum: (Int, Int) => Int = (_ * 2 + _ * 3)

addOne(5)      // => 6
weirdSum(2, 4) // => 16


// The return keyword exists in Scala, but it only returns from the inner-most
// def that surrounds it.
// WARNING: Using return in Scala is error-prone and should be avoided.
// It has no effect on anonymous functions. For example here you may expect foo(7) should return 17 but it returns 7:
def foo(x: Int): Int = {
  val anonFunc: Int => Int = { z =>
    if (z > 5)
      return z // This line makes z the return value of foo!
    else
      z + 2    // This line is the return value of anonFunc
  }
  anonFunc(x) + 10  // This line is the return value of foo
}

foo(7) // => 7
```

## Object Oriented Programming

```scala
// ----- OBJECT ORIENTED PROGRAMMING -----

/*
  Aside: Everything we've done so far in this tutorial has been simple
  expressions (values, functions, etc). These expressions are fine to type into
  the command-line interpreter for quick tests, but they cannot exist by
  themselves in a Scala file. For example, you cannot have just "val x = 5" in
  a Scala file. Instead, the only top-level constructs allowed in Scala are:

  - objects
  - classes
  - case classes
  - traits

  And now we will explain what these are.
*/

// classes are similar to classes in other languages. Constructor arguments are
// declared after the class name, and initialization is done in the class body.
class Dog(br: String) {
  // Constructor code here
  var breed: String = br

  // Define a method called bark, returning a String
  def bark = "Woof, woof!"

  // Values and methods are assumed public. "protected" and "private" keywords
  // are also available.
  private def sleep(hours: Int) =
    println(s"I'm sleeping for $hours hours")

  // Abstract methods are simply methods with no body. If we uncomment the
  // def line below, class Dog would need to be declared abstract like so:
  //   abstract class Dog(...) { ... }
  // def chaseAfter(what: String): String
}

val mydog = new Dog("greyhound")
println(mydog.breed) // => "greyhound"
println(mydog.bark)  // => "Woof, woof!"


// The "object" keyword creates a type AND a singleton instance of it. It is
// common for Scala classes to have a "companion object", where the per-instance
// behavior is captured in the classes themselves, but behavior related to all
// instance of that class go in objects. The difference is similar to class
// methods vs static methods in other languages. Note that objects and classes
// can have the same name.
object Dog {
  def allKnownBreeds = List("pitbull", "shepherd", "retriever")
  def createDog(breed: String) = new Dog(breed)
}


// Case classes are classes that have extra functionality built in. A common
// question for Scala beginners is when to use classes and when to use case
// classes. The line is quite fuzzy, but in general, classes tend to focus on
// encapsulation, polymorphism, and behavior. The values in these classes tend
// to be private, and only methods are exposed. The primary purpose of case
// classes is to hold immutable data. They often have few methods, and the
// methods rarely have side-effects.
case class Person(name: String, phoneNumber: String)

// Create a new instance. Note cases classes don't need "new"
val george = Person("George", "1234")
val kate = Person("Kate", "4567")

// With case classes, you get a few perks for free, like getters:
george.phoneNumber  // => "1234"

// Per field equality (no need to override .equals)
Person("George", "1234") == Person("Kate", "1236")  // => false

// Easy way to copy
// otherGeorge == Person("George", "9876")
val otherGeorge = george.copy(phoneNumber = "9876")

// And many others. Case classes also get pattern matching for free, see below.

// Traits
// Similar to Java interfaces, traits define an object type and method
// signatures. Scala allows partial implementation of those methods.
// Constructor parameters are not allowed. Traits can inherit from other
// traits or classes without parameters.

trait Dog {
    def breed: String
    def color: String
    def bark: Boolean = true
    def bite: Boolean
}
class SaintBernard extends Dog {
    val breed = "Saint Bernard"
    val color = "brown"
    def bite = false
}  

scala> val b = new SaintBernard
res0: SaintBernard = SaintBernard@3e57cd70  
scala> b.breed  
res1: String = Saint Bernard  
scala> b.bark  
res2: Boolean = true  
scala> b.bite  
res3: Boolean = false  

// A trait can also be used as Mixin. The class "extends" the first trait,
// but the keyword "with" can add additional traits.

trait Bark {
    def bark: String = "Woof"
}
trait Dog {
    def breed: String
    def color: String
}
class SaintBernard extends Dog with Bark {
    val breed = "Saint Bernard"
    val color = "brown"
}

scala> val b = new SaintBernard
b: SaintBernard = SaintBernard@7b69c6ba
scala> b.bark
res0: String = Woof
```

## Functional Programming

```scala
// ----- FUNCTIONAL PROGRAMMING -----

// Scala allows methods and functions to return, or take as parameters, other
// functions or methods.

val add10: Int => Int = _ + 10 // A function taking an Int and returning an Int
List(1, 2, 3) map add10 // List(11, 12, 13) - add10 is applied to each element

// Anonymous functions can be used instead of named functions:
List(1, 2, 3) map (x => x + 10)

// And the underscore symbol, can be used if there is just one argument to the
// anonymous function. It gets bound as the variable
List(1, 2, 3) map (_ + 10)

// If the anonymous block AND the function you are applying both take one
// argument, you can even omit the underscore
List("Dom", "Bob", "Natalia") foreach println


// Combinators
// Using `s` from above:
// val s = Set(1, 3, 7)

s.map(sq)

val sSquared = s.map(sq)

sSquared.filter(_ < 10)

sSquared.reduce (_+_)

// The filter function takes a predicate (a function from A -> Boolean) and
// selects all elements which satisfy the predicate
List(1, 2, 3) filter (_ > 2) // List(3)
case class Person(name: String, age: Int)
List(
  Person(name = "Dom", age = 23),
  Person(name = "Bob", age = 30)
).filter(_.age > 25) // List(Person("Bob", 30))


// Certain collections (such as List) in Scala have a `foreach` method,
// which takes as an argument a type returning Unit - that is, a void method
val aListOfNumbers = List(1, 2, 3, 4, 10, 20, 100)
aListOfNumbers foreach (x => println(x))
aListOfNumbers foreach println

// For comprehensions

for { n <- s } yield sq(n)

val nSquared2 = for { n <- s } yield sq(n)

for { n <- nSquared2 if n < 10 } yield n

for { n <- s; nSquared = n * n if nSquared < 10} yield nSquared

/* NB Those were not for loops. The semantics of a for loop is 'repeat', whereas
   a for-comprehension defines a relationship between two sets of data. */

// --- PATTERN MATCHING ---

// Pattern matching is a powerful and commonly used feature in Scala. Here's how
// you pattern match a case class. NB: Unlike other languages, Scala cases do
// not need breaks, fall-through does not happen.

def matchPerson(person: Person): String = person match {
  // Then you specify the patterns:
  case Person("George", number) => "We found George! His number is " + number
  case Person("Kate", number)   => "We found Kate! Her number is " + number
  case Person(name, number)     => "We matched someone : " + name + ", phone : " + number
}

// Regular expressions are also built in.
// Create a regex with the `r` method on a string:
val email = "(.*)@(.*)".r

// Pattern matching might look familiar to the switch statements in the C family
// of languages, but this is much more powerful. In Scala, you can match much
// more:
def matchEverything(obj: Any): String = obj match {
  // You can match values:
  case "Hello world" => "Got the string Hello world"

  // You can match by type:
  case x: Double => "Got a Double: " + x

  // You can specify conditions:
  case x: Int if x > 10000 => "Got a pretty big number!"

  // You can match case classes as before:
  case Person(name, number) => s"Got contact info for $name!"

  // You can match regular expressions:
  case email(name, domain) => s"Got email address $name@$domain"

  // You can match tuples:
  case (a: Int, b: Double, c: String) => s"Got a tuple: $a, $b, $c"

  // You can match data structures:
  case List(1, b, c) => s"Got a list with three elements and starts with 1: 1, $b, $c"

  // You can nest patterns:
  case List(List((1, 2, "YAY"))) => "Got a list of list of tuple"

  // Match any case (default) if all previous haven't matched
  case _ => "Got unknown object"
}

// In fact, you can pattern match any object with an "unapply" method. This
// feature is so powerful that Scala lets you define whole functions as
// patterns:
val patternFunc: Person => String = {
  case Person("George", number) => s"George's number: $number"
  case Person(name, number) => s"Random person's number: $number"
}



```

## More on

* Intersection types
* Union types
* Implicit
* [learn scala in y minutes](https://learnxinyminutes.com/docs/scala/)
* [scala official documentation](https://www.scala-lang.org/)