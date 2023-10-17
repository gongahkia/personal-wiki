# `Nim` 👑

![](https://styles.redditmedia.com/t5_333n4/styles/mobileBannerImage_s36d3rg4pz561.png)

Nim is a **statically-typed**, **compiled** programming language *(which brings together the efficiency of C and the productivity of Python)*.

----------

### Compiling Nim code 

Since Nim compiles to C code, we use a similar syntax to compile a Nim program into an executable and run it.

```console
nim c -r test.nim
```

----------

### Printing to the standard output

To print to the console, we use the `echo` command *(similar to in Bash)*.

```Nim
echo "Hello World!"
```

For **formatted strings**, we use the `fmt` keyword and `{}` curly braces *(similar to in Python)*. 

```Nim
for i in 1 .. 10:
    echo(fmt"{i} is even")
```

----------

### Comments 

**Single-line** comments are prefixed with the `#` hash character *(as you will see, Nim shares a lot syntactically with Python)*.

**Multi-line** comments are surrounded by the `#[` and `#]` characters.

```Nim
# this is a comment in Nim
#[
    This is a multi-line comment
]#
```

----------

### Variables && Constants

To define ***variables***, we use the `var` and `let` keywords *(similar to in Javascript)*.

`var` creates a **mutable** variable, whose value can be modified later on.  
`let` creates an **immutable** variable, whose value cannot be modified after assignment.
* *Note that `let` variables have values that are **run-time assigned**, and can be assigned via user input while running the program executable*.

```Nim
var string = "Nim is"
string.add("handsome")

let age = 10
# age += 25 would be an invalid line of code that would result in an error at compile time
```

`const` creates a **constant**, whose value cannot be modified after assignment.
* *Note that constants have values that are **compile-time assigned**, and must be known before compiling the program into an executable*.

```Nim
const fibonacci = fib(30)
# Nim will compute this constant data at compile time, and set the constant fibonacci to the value of 832040
```

----------

### Data Types 

1. **String** (*string*)

```Nim
var testString: string = "Hello World!"
```

> Strings have a bunch of built-in procedures that allow us to interact with them.

* `$`

`$` dollarsign operator will convert any other data type to a **string data type**.

```Nim
var number1: int = 10
var string1: string = $number1
```

* `&` / `,` / `.add()`

`&` ampersand, `,` comma and `.add()` procedure can all be used for **string concatenation**.

```Nim
var string2 = "ok"
var string3 = " bet"
var someString = string2.add(string3)

echo (string2 & string3)
echo (string2, string3)
echo (someString)

# the above 3 echo statements will all achieve the same thing, printing the string "ok bet" to the console
```

* `[ .. ]`

`[]` square brackets are used to enclose the **range of indexes** of the characters we want to **slice** from the target string *(string slicing!)*.

```Nim
var longString = "Hello World!"
echo (longString[0 .. 4]) # this will print the string "Hello" to the console
```

2. **Char** (*char*)

```Nim
var testCharacter: char = 'A'
```

3. **Integer** (*int*)

```Nim
var testInteger: int = 32
```

4. **Float** (*float*)

```Nim
var testFloat: float = 5.25
```

5. **Boolean** (*bool*)

```Nim
var testBool: bool = true
```

6. **Array** (*array*)

* Nim arrays are lists of values of the **same type** with an **immutable size** at compile time *(the size of the array must be statically declared before compiling and running the program)*.

```Nim
var testArray: array[3, int] = [1, 2, 3]
var anotherArray = [4.23, 5.00, 6.19] # the Nim compiler can infer the data type of the array without you statically declaring it
var thelastArray: array[2, string]
```

> Arrays have a bunch of built-in procedures that we can use to interact with them.

* `[ .. ]`

`[]` square brackets enclose the **range of indexes** of the elements that we want to access from our array.

```Nim
var array1 = [0, 2, 4, 6, 8, 10]
echo (array1[0 .. 2]) # this will print out the sliced array, containing the integers 0, 2 and 4 to the console
```

7. **Sequences** (*seq*)

* Nim sequences are lists of values of the **same type** with a **dynamic size** at compile time *(values can be added and deleted after initializing the array, and the size of the array need not be statically declared)*.
* Sequences are prefixed with the `@` ampersand character to differentiate them from conventional Nim arrays.

```Nim
var testSequence: seq[int] = @[1, 2, 3, 4, 5]
var anotherSequence = @[6.239, 7.432, 8.412, 9.312, 10.123]
var thelastSequence: seq[string]
```

> Sequences also have a bunch of built-in procedures that allow us to interact with them.

* `.add()`

`.add()` takes in the **value** to be appended to the sequence as the argument *(similar to .append() in Python)*.

```Nim
var thefirstofAnotherSequence: seq[int] = @[1, 2, 3, 4]
thefirstofAnotherSequence.add(10) # this will append the value of integer 10 to the sequence
stdout.write(thefirstofAnotherSequence[4]) # this line will print out the number 10 to the console
```

* `.del()` 

`.del()` takes in the **index** of the element to be deleted from the sequence as the argument.


```Nim
var yetanotherArray: seq[int] = @[1, 2, 3]
yetanotherArray.del(0) # this will delete the element of index 0 in the sequence
stdout.write(yetanotherArray[0]) # this line will print out the number 2 to the console
```

* `.len()`

`.len()` returns the **length** of a given sequence *(similar to len() in Python)*.

```Nim
var aSequence: seq[string] = @["aight bet", "aigh beh", "good morning", "peepl"]
echo aSequence.len() # this will print out 4, which is the length of the sequence above
```

* `[ .. ]`

`[]` square brackets enclose the **range of indexes** of the elements that we want to access from our sequence.

```Nim
var sequence1 = @[1, 3, 5, 7, 9]
echo (sequence1[0 .. 2]) # this will print out a sliced sequence containing the integers 1, 3 and 5 to the console
```

8. **Tuples** (*tuple*) 

* Nim tuples are lists of values of **different type** with an **immutable size** at compile time *(the size of the tuple must be statically declared before compiling and running the program)*.

**Anonymous tuples** refer to tuples that have **no named fields**, and that merely contain values *(which do not belong to a variable)*.

```Nim
var someTuple = (10, 0.55, "hello", true)
var lastTuple: (string, int, bool, float) = ("ok", 20, false, 30.21)
# the above 3 lines contain anonymous tuples
```

`tuple` keyword is used to create a **non-anonymous tuple**, which functions similarly to a *Dictionary* in Python.  
* This allows us to **access** and **reassign** values within the tuple via `.` dot notation.

```Nim
var emptyTuple: tuple[name: string, age: int] # this will initialize an empty tuple with the fields name and age

var namedTuple: tuple[name: string, age: int] = ("Adam", 100) # this creates a named tuple
echo(namedTuple.name) # this will print out the string "Adam" to the console
namedTuple.age = 421 # this will reassign the integer value of 421 to the field age within the namedTuple tuple variable
echo(namedTuple.age) # this will now print out the integer 421 to the console
```

9. **Tables**

* Nim tables are **hash tables** that **map key-value pairs**, and are the closest we will come to *Dictionaries* in Nim, aside from named non-anonymous tuples.

`import tables` will include the **tables module** within our program.  
`.toTable()` procedure creates an **unordered table**.

```Nim
import tables

var ioTable = {"output": stdout, "input": stdin}.toTable # note that this table is just an example, just make a note of the .toTable() procedure
```

`.toOrderedTable()` procedure creates an **ordered table**.

```Nim
import tables

var romanDigits = {'M': 1000, 'D': 500, 'C': 100, 'L': 50, 'X': 10, 'V': 5, 'I': 1}.toOrderedTable
```

`.toCountTable()` procedure creates an **unordered table** that also **counts the number of occurrences of every key-value pair**.

```Nim
import tables

var laze = {"why": 1, "am": 2, "i": 3, "so": 4, "handsome": 5, "handsome": 5}.toCountTable

for pair in laze.pairs:
    echo (pair) # this will print out each key value pair, as well as the number of occurences of said pair, in the table
```

> Tables also have some built-in functions which can be used to interact with them.

* `.pairs()`

`.pairs` iterator is used to iterate through the **key-value pairs** in a table *(often in conjunction with a `for` loop)*.

```Nim
var skibbidy = {1: "bop", 2: "boop", 3: "bep", 4: "yes", 5: "yez"}.toOrderedTable

for pair in skibbidy.pairs:
    echo (pair) # this will print out each key-value pairs to the console
```

* `.hasKey()`

`.haskey()` procedure takes in a key as an argument, and **checks whether said key is within the table**.

```Nim
var skibbidy = {1: "bop", 2: "boop", 3: "bep", 4: "yes", 5: "yez"}.toOrderedTable

if skibbidy.hasKey(4):
    echo ("found 4!") # this line will print the string "found 4!" to the console since the ordered table skibbidy contains the key of integer value 4
```
----------

### Static type declaration

Nim has ***static type declaration***, indicated by the `:` colon character when initializing a variable.  
*(While static type declaration is not necessary since the Nim compiler infers data types, it is still advised in larger programs.)*

* Once a variable is initialized, it **cannot** be reinitialized with a different **data type** *(although the value of the variable can still be reassigned if the variable is defined with the `var` keyword)*.

```Nim
var a: int

var 
    d = 10
    e = 5
    w: int
    q: float

let i: int = 20
```

* We can declare and assign **multiple variables** at a time with the `var` or `let` keywords, as seen above.

To derive the **type of a value/variable**, we use the `.type` method

```Nim
var somevariable: string = "good morning people"
echo somevariable.type # this will print out the type 'string' to the console
```

----------

### Type conversion

`.toFloat`: converts any data type *(besides boolean)* to a Float

`.toInt`: converts any data type *(besides boolean)* to an Integer

`$`: converts any data type *(besides boolean)* to a String

----------

### String parsing

For additional **string parsing** capabilities, we include the *strutils* module with the `import` keyword.

`parseInt`: converts an Integer data type to a String  

`parseFloat`: converts a Float data type to a String

```Nim
import strutils

var stringInteger = "100"
var parsedInteger: int

parsedInteger = stringInteger.parseInt
```

----------

### Conditional flow

`if`, `else` and `elif` *(else if)* all function the same way in Nim as in Python.

```Nim
if 10 > 5:
    echo ("10 is larger than 5")
if "Hello" != "Hello":
    echo ("both strings are the same")
elif 27.123.type == true.type:
    echo ("they have the same data type")
else:
    echo ("none of the prior conditions met")
```

### Case statement

**Case statements** are indicated by the keywords `case` *(at the beginning of the case statement)*, `of` *(to indicate the condition for each scenario)*, and `else` *(to indicate the default condition should all other conditions be left unmet)*.  

```Nim
case a:
    of 0:
        echo ("a is 0")
    of 1:
        echo ("a is 1")
    of 2:
        echo ("a is 2")
    else:
        echo ("none of the previous conditions were met")
```

----------

### Standard input && output

`readLine(stdin)` / `stdin.readLine()` takes in user input from the **standard input** (the console), and stops reading input when the user presses *enter* on their keyboard.

* Both examples below achieve the same result.

```Nim
echo ("Type your name: ")
let data = readLine(stdin)
echo (data)

echo ("Username: ")
result = stdin.readLine()
echo (result)
```

`stdout.write()` prints out the argument taken in to the **standard output** (the console) on the same line.
* Note that values in an array, sequence and table can be accessed via **indexing** with the `[]` square brackets *(as in other programming languages)*.

```Nim
var a: array[3, int] = [1, 2, 5]
stdout.write(a[0])
stdout.write(a[1])
stdout.write(a[2])
# this will print out the values 1, 2, 5 all on the same line
```

----------

### Ranges

`..` double colon characters create a **range** of numbers that we can iterate through *(all ranges start with a lower value and end with a higher value)*.

```Nim
import strutils

echo ("Enter a number: ")
let userNumber = stdin.readLine().parseInt

case userNumber:
    of 0 .. 10:
        echo "Your number is smaller than 10"
    of 11 .. 100 
        echo "Your number is from 11 to 100"
    else:
        echo "Your number is larger than a 100"
```

----------

### Loops

![](https://media.tenor.com/zHGtLgNwf3AAAAAC/doge-infinity.gif)

#### For Loops

`for` loops *(accompanied by `in`)* allow us to iterate through a given **data set** or a **range of values**.

`.low` method returns the **lowest index** *(the index of the first item)* in a list.  
`.high` method returns the **highest index** *(the index of the last item)* in a list.  
`..` double colon refers to the **range** operator, as specified above.  

```Nim
a: array[3, int] = [1, 2, 5]
for i in a:
    stdout.write(i) 
    # this line will print out 1, 2 and 5 to the console on the same line

for i in a.low .. a.high:
    stdout.write(a[i])
    # this line will also print out 1, 2 and 5 to the console on the same line, achieved by referencing list elements by index
```

#### While Loops

`while` loops allow us to repeatedly run a given chunk of code until a given condition **fails to be met** *(becomes unmet)*.

* Note that Nim does not have the conventional `%` modulo operator, and instead we use the `mod` keyword to run the **modulo** operation on a number.
```Nim
while true:
    stdout.write("A") # note that this while true loop will endlessly loop if there is no break condition inserted

var i = 0
while i < 10:
    if i mod 2 == 0:
        echo (i) # this will only print out even numbers
```

----------

### Procedures

Procedures are the equivalent of functions in Nim.  

`proc` is used to declare a **procedure**.  
`()` brackets contain the **arguments** to be taken in by the procedure.  
`:` colon is used to declare the **data type of the arguments accepted** and **returned value** of the procedure.
`return` is used to **return said value** from the procedure.  
`result` keyword is assigned as a **variable to the returned value** *(and is offered as an alternative to the return keyword)*.

```Nim
proc hello() = 
    echo ("Hello world!")

# just like functions, procedures don't have to return a value, and can be run for their side-effects

proc aight(): int = 
    result = 2

proc bet(): int = 
    return 2 

# the above 2 procedures achieve the exact same result with the return and result keywords

proc okLah(water: string): string = 
    result =  fmt"{water} is ok"

proc payLah(water: string): string = 
    return fmt"{water} is ok"

# likewise, the above 2 procedures also achieve the same result 

echo (aight()) # this will print out the integer 2 to the console
echo (okLah("yessir")) # this will print out the string "yessir is ok" to the console
```

> There are also procedure specific methods you should be aware of.

To call a variable *(instead of a value)* as an argument in a procedure, we must prefix the data type of said argument with `var`.

```Nim
proc sum(num1: var int): int = 
    return num1 + 5

var a:int = 10
echo(sum(a)) # this will print out 15 to the console
```

`varargs` keyword is used to accept a **variable number of arguments** to a procedure, which it takes in as an *array* data type.


```Nim
proc multiSum(numbers: varargs[int]): int=
    var sum: int
    for num in numbers:
        sum += num
    return sum # this procedure takes in a variable number of integers as arguments, which it reads as an array 
```

To **export a procedure** from one nim program to another, we need to suffix the procedure name with an `*` asterisk.  
* *Note that the target file and the file containing our desired procedure must be within the same file directory*.

```Nim
proc newProcedure* () =
    echo ("Hello World!")
```

`import` keyword is used to **import said procedure** into our program.
* *Note that we import the filename of the nim program containing our target procedure*.

```Nim
import file

newProcedure() # this will call the newProcedure procedure from file.nim, printing the string "Hello World!" to the console
```

`*` asterisk character used above can therefore be used for ***exporting***:
1. **Procedures** `proc newProcedure*`
2. **Variables** `var newVariable*`
3. **Types** `type customArray*`

----------

### Type

`type` keyword is used to create **custom data types** that we can then create variables from (*most often used in conjunction with the tuple data type)*.

```Nim
type 
    customTuple = tuple
        name: string
        age: int

var aTuple: customTuple = ("Andrew", 3000) # this will automatically assign the value of string "Andrew" to the field name, and the integer 3000 to the field age
```

----------

### File Handling

![](https://media.tenor.com/3OttOiE-u6wAAAAC/carpeta-infinity-folder.gif)

To **read** and **write** files, the expectation is that the target file must be in the same file directory as the Nim program we are writing.

#### Reading Files

`.open()` procedure takes the target file name as an argument *(is in read mode by default)*, and is used to **open a file in read mode** *(similar to open() in Python)*.  
* In Nim, we have to **check whether the file has been opened** by declaring our *fhand* file handler under the `File` data type.
* *Note that `open( , fmRead)` is valid code once the target file has been checked to be open.*

`.readLine()` procedure is called on the file handler *fhand*, and **reads each line of our target file**.
* *Note that previously, readLine() was used to read user input from the console with `stdin.readLine()`, and is now used to read data from the target file with `fhand.readLine()`.*

`.close()` procedure **closes** the previously opened file.

```Nim
var fhand: File
if fhand.open("Test.txt") == true:
    echo "File opened"

var data = fhand.readLine()
echo (data)

fhand.close()
```

#### Writing to Files

`.open()` procedure takes the target file name as an argument, and `fmWrite` indicates to **open the file in write mode**.
* If the file to be written to does not exist, `.open( , fmWrite)` procedure will **create a new file with the specified target name** in the same file directory.

`.write()` procedure takes the text to be written as an argument, and is called on the file handler *fhand*, **writing the specified text to the target file**.
* *Note that previously, write() was used to write text to the console with `stdout.write()`, and is now used to write data to a target file with `fhand.write()`.*

`.close` procedure **closes** the previously opened file.

```Nim
var fhand: File
fhand = open("input.txt", fmWrite)

fhand.write("1 2 3 4 5") # this will write the string "1 2 3 4 5" to the target file input.txt

fhand.close()
```

> There are also some special use-case procedures that can be called on File data types.

* `.readAll()`

`.readAll()` procedure reads **all the lines of a file** into a given variable.

```Nim
# assuming we have confirmed that the target file, input.txt exists
f = open("input.txt", fmRead)

var allLines = f.readAll()
echo (allLines) # this would print out all the lines of a textfile (including newline characters) to the console
```

----------

### Enumerators

Enumerators are created as a custom data type, with the `type` keyword covered above.

* `enum` keyword is used to indicate an array of values that will be enumerated across, separated by `,` commas.

```Nim
type 
    Direction = enum
        east, north, west, south

echo Direction.east # this will print the string "east" to the console

var direction: Direction

echo direction # this will likewise print the string "east" to the console, as east is the default first enumerator in the array
```

> Enumerators also have their own set of built-in procedures that allow us to interact with them.

* `.succ()`

`.succ` calls the **successor** *(the next element in the array to be enumerated)*.

```Nim
type 
    Direction = enum
        east, north, west, south

var chooseDirection = east.succ
echo (chooseDirection) # this will print out the string "north" to the console
```

* `.pred()`

`.pred` calls the **predecessor** *(the previous element in the array to be enumerated)*.

```Nim
type 
    Direction = enum
        east, north, west, south

var chooseDirection = south.pred
echo (chooseDirection) # this will print out the string "south" to the console
```

----------

### Procedure Overloading

![](https://hips.hearstapps.com/del.h-cdn.co/assets/16/13/1459267611-bananaslip.gif?crop=1xw:0.9883177570093458xh;center,top&resize=1200:*)

Procedure overloading allows us to **define multiple procedures** *(that take in arguments of **different data types** / **different number** of arguments and return different values accordingly)* under the same name.  
* To achieve this, we just need to define *every possible argument pattern* we want our procedure to receive under the same procedure name.


```Nim
proc sum(x, y: int): int = 
    return x + y

proc sum(x, y: float): float = 
    return x + y

proc sum(x, y: string): string = 
    return x & y

proc sum(x, y: string, c: int): string = 
    return x & y & $c

echo sum(1, 1)
echo sum(0.1, 0.9)
echo sum("Hello ", "World")
echo sum("Hello", "Bro", 12330)
# the above 4 lines are all valid nim code, as we are able to overload the sum procedure with 3 different data types to anticipiate diffeernt user input, whilst retaining the same procedure call
```

----------

### Object-Oriented Programming

Objects in Nim are created as **custom data types** using the `type` keyword, and object attributes can be printed to the console using the `echo` keyword.

#### Creating an Object

![](https://ssb.wiki.gallery/images/4/4a/Steve_Neutral_B_SSBU.gif)

`object` keyword is used to create an **object** *(as opposed to a tuple, as seen previously)*.

```Nim
type
    testObject = object
        name: string
        age: int
```

**Initialization of objects** is achieved by assigning said object to a new variable *(similar to in Python)*.

```Nim
var emptyDeclaration: testObject # this is an empty object declaration
var emptyInitialization: testObject = testObject() # this is an empty object initalization
var obj1:testObject = testObject(age: 21, name: "John") # this initializes the testObject with the values taken in as arguments
```

**Accessing** and **modifying** an object's fields can be achieved via `.` dot notation.

```Nim
obj1.name = "Arthur" # this modifies the value of the field name from "John" to "Arthur"
obj1.age = 1000 # this modifies the value of the field age from 21 to 1000
```

`.reset()` procedure can be run to **reset all of an object's fields** to their **default values**.

```Nim
obj1.reset # this would reset the value of the fields age and name to be 0 and ""
echo (obj1) # this would print out the object (name: "", age: 0), which has had its field values reset to the default
```

`of` operator is used to determine whether a variable *(instance object)* is from the specified object type.

```Nim
if obj1 of testObject: # this checks for whether the variable instance object obj1 originates from the object type testObject
    echo("yeah it is")
else:
    echo("sadge")
```

#### Object Inheritance

![](https://cdn.wikitechy.com/tutorials/r-programming/multiple-inheritance-1.gif)

Object inheritance in Nim allows a child object to **inherit attributes** from **only one** parent object *(similar to in Python)*.

Assigning `object of Rootobj` to our object type creates a **parent object** *(root object)*.

```Nim
type 
    Human = object of Rootobj # Parent Root object, as opposed to a normal object, where we would simply assign object to it
        name: string
        age: 100
```

`object of` keyword is used to assign the **child objects**, while pointing toward the **parent object**.

* Child objects inherit **all the fields** of the parent object, alongside having their own unique fields that differentiate them from their parent object.

```Nim
type
    Student = object of Human # Child object, with Human object type as the parent object
        id : int # this id field is unique to the Student child object type, which it will possess alongside the inherited name and age fields from its Human parent object type

var john = Human(name: "John", age: 20)
var eve = Student(name: "Eve", age: 15, id: 3123)
# the above two lines initialize one parent instance object and one child instance object with their respective properties
```

----------
