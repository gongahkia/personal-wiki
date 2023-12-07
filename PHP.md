# `PHP`

PHP is a scripting language for the server.

## Comments

```php
// this is a single-line comment 

# this is also a single-line comment but the above is more common

/* 
    this is a multi-line
    comment so tyvm
*/
```

## Quickstart

* semicolon language

```php
<?php // php code must be enclosed in <?php tags, anything outside <?php tags are echoed automatically

// These values are printed to the stdout by default, which can be the webpage if PHP running in the browser
print('Hello'); // prints without newline by default, brackets () are optional
echo 'World'; // prints without newline by default, brackets () are optional
echo 100; // prints scalar integer value of 100 directly

>
```

## Variables

```php
<?php

// VARIABLES
    // variables declared with a $
    // variables CANNOT be declared, they are created at value assignment and their type is immediately assigned to them

// BOOLEAN 
$boolean_val = true; // case-insensitive, can be TRUE or True
$another_boolean_val = false; // case-insensitive, can be FALSE or False

// INT
$int_val = 100; // these are normal integer values
$another_int_val = -12;
$octal_int_val = 012; // a leading 0 denotes an octal number value
$hexa_int_val = 0x0F; // a leading 0x denotes a hexadecimal number value
$bin_int_val = 0b11111111; // a leading 0b denotes a binary number value

// DELETE VARIABLES
unset($int_val);

// FLOAT (aka double)
$float_val = 1.234;
$another_float_val = 1.2e3;
$also_a_float_val = 7E-10;

// ARITHMETIC
$sum = 1 + 1; // addition
$difference = 2 - 1; // subtraction
$product = 2 * 2; // multiplication
$quotient = 2 / 1; // division

// SHORTHAND ARITHMETIC
$funny = 0;
$funny += 1; // increments by one
echo $funny++; // increments by one after print statement (so prints 1 here)
echo ++$funny; // increments by one before print statement (so prints 3 here)
$funny /= $float_val; // divide $funny by $float_val and assign the quotient to $funny

// STRING
$string_val = 'quote'; // strings are enclosed in single quotes
$formatted_string = "This is a $string_val"; // functions as a formatted string that embeds the $string_val variable in the string
$another_formatted_string = "This is also a formatted string as seen in {$string_val} here."; // enclosing a variables in curly braces also creates a formatted string, as well as ${}
$escaped = "This will reflect as 4 spaces /t"; // special characters like /t and /n are escaped only in double quotes
$unescaped = 'This will literally show a slash and a t'; // special characters are unescaped in single quotes
echo 'This string' . ' is concatenated'; // the . supports string concatenation
$test = 'watermelon';
$test .= ' sugar high' // string concatenation means .= also works for strings
echo $test; // this will print out "watermelon sugar high"
echo 'Multiple', 'Parameters', 'Valid'; // the , also supports string concatenation

// NULL values
$var_null = null;

>
```

## Constants

```php
<?php

// CONSTANTS
    // constants are defined using define() and cannot be changed at runtime
    // constants can be accessed by calling it without a $

define("FOO", "something"); // defines a constant variable called FOO of string value 'something'
echo FOO; // prints 'something'
echo 'This outputs ' . FOO // prints 'This outputs something'


>
```

## References

* references to variables are created with `&` character

```php
<?php

$x = 1; // assigns scalar integer value of 1 to variable $x
$y = 2; // assigns scalar integer value of 2 to variable $y
$x = $y; // variable $x now contains same value as variable $y as value is copied over
$z = &$y; // variable $z contains a reference that points to variable $y
    // changing value of $z will change value of $y
    // changing value of $y will change value of $z
    // $x will remain unchanged in both cases since original value of $y copied to variable $x

var_dump($z); // prints int(0) to stdout
print_r($array_literal); // prints a variable in a human-readable format

>
```

## Arrays

* all php arrays are stored as hashmaps *(dictionaries with key-value pairs)*
* list literals are implictly assigned integer keys

```php
<?php

// ARRAYS as hashmaps

$eg_array = array('One' => 1, 'Two' => 2, 'Three' => 3); // works in all PHP versions to instantiate an array
$another_eg_array = ['One' => 1, 'Two' => 2, 'Three' => 3]; // works in PHP 5.4

print_r($eg_array); // prints all key value pairs, print_r() is useful for debugging
var_dump($eg_array); // displays more information than print_r(), is useful for debugging

$another_eg_array['Four'] = 4; // assigns a new element to the array
echo $eg_array['One']; // prints 1, this tells me the least as compared to print_r() or var_dump()

// ARRAY LITERALS

$array_literal = ['One', 'Two', 'Three']; // implictly assigns integer key for array literals per their index

$array_literal[] = 'Four'; // appends an element to the end of the array
array_push($array_literal, 'Five'); // also appends an element to the end of the array

unset($array_literal[3]); // removes element from array

$num = 2;
echo $array_literal[1]; // prints 'Two'
echo $array_literal[$num]; // prints 'Three'

// ITERATING THROUGH AN ARRAY
// iterate through a hashmap array or an array literal using the foreach() loop

foreach($stuff as $k => $v) {
    echo "Key=", $k, "Val=", $v;
}

// ARRAY FUNCTIONS

array_key_exists($key, $array); // checks whether a key is in the array
isset($array["key"]); // also checks whether a key is in the array
count($array); // length of the array
is_array($array); // checks whether a variable is an array
sort($array); // sorts the array by its values, loses the key value pair associations
ksort($array); // sorts the array key-value pairs by its keys
asort($array); // sorts the array key-value pairs by its values
shuffle($array); // randomly sorts the array
explode($delimiter, $string); // splits a string by a specified delimiter and assigns that to an array

echo isset($eg_array["name"]) ? "name is set" : "name is not set"; // utilising the ternary operator to check whether a key exists in an array, this can be avoided using the null coalescing operator
echo $_GET["name"] ?? "nobody"; // PHP 7's null coalescing operator can also be used here

>
```

## Logic

```php
<?php

// assert() throws a warning if an argument evaluates to false

$a = 0;
$b = '0';
$c = 1;
$d = '1';

// PARTIAL EQUALITY
    // only checks for value, not type

assert($a == $b); // equality check returns true
assert($a != $c); // inequality check returns true
assert($a <> $c); // also an inequality check returns true
assert($a < $c); // comparison check returns true
assert($c > $b); // comparison check returns true
assert($a <= $b); // comparison and equality check returns true
assert($c >= $d); // comparison and equality check returns true

// COMPLETE EQUALITY
    // checks for value and type

assert($c === $d); // complete equality check returns true
assert($a !== $d); // complete inequality check returns true
assert(1 === '1'); // complete equality check returns false
assert(1 !== '1'); // complete inequality check returns false

// SPACESHIP OPERATOR
    // introduced from PHP 7 onwards
    // kinda odd behaviour, avoid usage until fully understand how it works

$a = 100;
$b = 1000;

echo $a <=> $a; // prints 0 since they are equal
echo $a <=> $b; // prints -1 since $a < $b
echo $b <=> $a; // prints 1 since $b > $a

>
```

## Control structures

```php
<?php

// IF ELSEIF ELSE STRUCTURES

if (true) {
    print 'Kill me';
}

if (false) {
    print 'Isle eating house';
} else {
    print 'Hurt Hurt Skrt Skrt';
}

$watermelon = 10;

if ($watermelon > 10) {
    print 'Watermelon is bigger than 10';
} elseif ($watermelon === 10) {
    print 'Watermelon is 10';
} else {
    print 'Watermelon is smaller than 10';
}

// TERNARY OPERATOR
    // additional ternary shortcut since PHP 5.3

// general structure is as follows
    // CONDITIONAL CHECK ? IF EVALUATES TRUE : IF EVALUATES FALSE

print (false ? 'Isle eating house' : 'Hurt Hurt Skrt Skrt');
$x = false;
print($x ?: 'Hurt Hurt Skrt Skrt'); // ternary operator shortcut in ?:

// SWITCH CASE STATEMENTS
    // remember to include break statements
    // without a break statement, logic flow falls through as in case 'two' as below
    // default: case exists

$x = 0;

switch ($x) {
    case '0':
        print 'Switch can do type coercion so this evaluates.';
        break; // break statements are necessary
    case 'two': // a case flow can be left empty, nothing will occur if this case is hit and since no break statement, logic falls through and case 'three' is evaluated
    case 'three': 
        break;
    default:
        break;
}

>
```

## Null coalescing operator

* `??` coalescing operator to assign a default value if a variable's value is null

```php
<?php

// NULL COALESCING OPERATOR
    // shorthand operator also available

// takes on the following syntax
    // VALUE TO CHECK ?? DEFAULT VALUE TO ASSIGN IF THE AFOREMENTIONED VALUE IS NULL

$variable = $value ?? $default; // if $value is not null, $variable takes value of $value, else $variable takes value of $default
$a = null;
$b = 'Cereal chicken';
$a ?? 'Sambal beans'; // since $a is of value null, it is assigned the value 'Sambal beans'
$b ?? 'Thai fish'; // since $b already has value 'Cereal chicken', it keeps that value

>
```

## Type conversion

```php
<?php

$integer = 1;
echo $integer + $integer; // returns 2

$string = '1';
echo $string + $string; // variable $string coerced into type conversion to become an integer due to the + operator, returns 2

$string1 = 'one';
echo $string1 + $string1; // variable $string1 cannot be coerced into being type converted into an integer, so this returns 0

// there are also other dedicated functions for just type conversion such as strval()

>
```

## Loops

* `continue` and `break` function similarly as in other languages

```php
<?php

// WHILE loop

$i = 0;
while ($i < 5) {
    echo $i++; // prints 01234
}

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // exits out of the entire while loop
    }
    echo $i++; // prints 012
}

// DO WHILE loop

$i = 0;
do {
    echo $i++;
} while ($i < 5); // prints 01234

// FOR loop
    // C-style for loops

for ($x = 0; $x < 10; $x++) {
    echo $x; // prints 0123456789
}

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // skips to next iteration of the loop
    }
    echo $i; // prints 0124
}

// FOREACH loops
    // can iterate over iterable structures like arrays for values or keys and values

$meta = ['cereal chicken' => 1, 'thai fish' => 2, 'nonya veg' => 3];

// iterating over array for values
foreach ($meta as $food_item_level) {
    echo $food_item_level; // prints 123
}

// iterating over array for keys and values
foreach ($meta as $food_item => $food_item_level) {
    echo "Parklane cai fan menu item $food_item has a level of $food_item_level";
}

>
```

## Functions

* function defined with `function`
* `return` functions similarly as in other languages
* `&` is the reference operator and allows us to call by reference similar to C

```php
<?php

// PARAMETERS VS ARGUMENTS
    // parameter is the variable used in the function definition
    // argument is the value passed during function invocation

// FUNCTION
    // default parameter values can be assigned

function shit() {
    return 'Shit';
}
echo shit(); // prints 'Shit'

function add($x, $y=1) { // $y assigned a default value of 1 and it is an optional parameter
    return $x + $y;
}

// CALL BY VALUE
    // function arguments are an alias for the actual variable, they are stored as a separate variable with a copy of the value of the argument during function invocation
    // PHP handles this similarly to most other programming languages
    // this safeguards the function arguments from being modified directly as a copy is returned, the function argument itself will not be altered by the function in memory (unlike a method)
    // this is calling by value

function call_by_value($alias) {
    $alias = $alias * 2; // the $alias being assigned here stores a different value from the $alias function argument
    return $alias;
}

// CALL BY REFERENCE
    // PHP has references using the & (ampersand) operator
    // this allows the function arguments to be directly modified by pointing the function to the function argument in memory, effectively creating a method (since the function does not need to return any value)
    // this is calling by reference

function call_by_reference(&$realthing) { // the & indicates that we are calling the function argument by reference
    $realthing = $realthing * 3; // this will modify the actual value of the funtion argument directly, multiplying it by 3, and no value has to be returned explicitly since the function argument has already been modified
}

// SCOPE
    // conventionally, scope is restricted within the degree of curly braces, which includes functions
    // global scope is for variables available within scope of entire program
    // local scope is for variables available within scope of current set of curly braces (could be for functions, loops, conditional checks etc.)

function tryzap() {
    $val = 100; // this $val here is completely separate from the below val and is in local scope
}

$val = 10; // this $val here is a completely different variable from the above val and is in global scope
tryzap(); // as such, nothing will change here since we are neither calling the variable by reference nor reassigning $val in the global scope to accept a copy
echo $val; // this will print out integer value of 10

// GLOBAL
    // the global keyword converts a variable within the local scope into one in the global scope
    // avoid using global as far as possible, prioritize functional programming practices
    // "this will affect the global scope"

function dozap() {
    global $val2; // causes the local variable $val2 to become a global variable
    $val2= 100; // assigns the global variable $val2 the value of 100 within local scope, this will affect the global scope
}

$val2 = 10; // creates the global variable $val2 of value 10
dozap(); // this will now change the global variable of $val2 since the function invocation converts the local variable $val2 into the global variable $val2
echo $val2; // this will print out integer value of 100

// CHECK IF FUNCTION EXISTS
    // function_exists("function_name") returns TRUE or FALSE based on whether a function exists

if (function_exists("array_combine")) {
    array_combine($some_array);
} else {
    echo "walao eh";
    // do something else
}

// ANONYMOUS FUNCTIONS

$anon = function($x) {
    return $x + 1;
}
echo $anon(2); // prints 3

function ass($x, $y, $z) {
    echo "$x - $y - $z";
}

// NESTED FUNCTIONS
    // functions can return functions
    // use keyword brings outside variables into scope

function bar($x, $y) {
    return function($z) use ($x, $y) { // use to bring in outside variables
        ass($x, $y, $z);
    }
}
$bar = bar('A', 'B');
$bar('C'); // prints "A - B - C"

// CALLING NAMED FUNCTIONS
    // named functions can be called using a string of their name

$func_name = 'add'; // we previously defined an add() function
echo $func_name(1,2); // prints 3, since this invokes the add() function

// VARIABLE NUMBER OF ARGUMENTS
    // ... operator takes in a variable number of arguments as an array that can be iterated over
    // can be used since PHP 5.6

function variable($word, ...$list) {
    echo $word . "||";
    foreach ($list as $item) {
        echo $item . '|';
    }
}

variable("Separate", "Hello", "World"); // prints Seperate || Hello ||| World

>
```

## Error handling

* `try` `catch` works as you would expect
* Exception creates a variable that you can store and access

```php
<?php

try {
    // Do something
} catch (Exception $e){
    // Handle the exception $e
}

>
```

## Other files

* equivalent of a python `import`

```php
<?php

// INCLUDE
    // searches for the specified file, if absent returns a non-fatal error

include 'my-file.php'; // code within my-file.php is now within scope, if the current file is not found it emitts a warning
include_once 'my-file.php'; // will not include code from my-file.php if its been included elsewhere

// REQUIRE
    // functions the same way as include except a fatal error is created if specified file absent

require 'my-file.php'; 
require_once 'my-file.php';

>
```

## Classes

* `public` variables are visible from the global scope
* `private` variables are accesible within the class only
* `protected` variables are accessible from the class and its subclasses
* `static` variables belong to the class itself and can only be called from the class, not from any of its objects
* `final` class methods are unoverridable and `final` classes are unextendable
* `_construct` function is the constructor that is automatically called upon object instantiation, equivalent of \_\_init__ in python
* `$this` is the equivalent of self in python
* `->` is the equivalent of . in python
* `new` is used to instantiate a class object

```php
<?php

class CaiFanShop {

    const carbohydrate = 'Rice';

    public static $publicStaticVar = 'public static'; // public variable visible from global scope, static meaning it can only be accessed from within the class
    private static $privateStaticVar = 'private static'; // private variable accesible from within the class only
    protected static $protectedStaticVar = 'protected static'; // protected variable accessible from wihtin the class and its subclasses during inheritance

    // properties must declare their visibility
    public $property = 'public';
    public $instanceProp;
    protected $prot = 'protected';
    private $priv = 'private';

    // CONSTRUCTOR FUNCTION 

    public function _construct($instanceProp) { 
        $this->instanceProp = $instanceProp; // access instance variables with $this
    }


    public function myMethod() {
        print 'MyClass';
    }

    // FINAL FUNCTIONS
        // final functions are unoverridable

    final function youCannotOverrideMe() {
    }

    // DESTRUCTOR FUNCTION
        // called when object is no longer referenced, used to deconstruct the object

    public function _destruct() {
        print "Destroying";
    }

    // STATIC
        // declaring class properties or methods as static means they are accesible without needing an instantiation of the class
        // a static property or attribute cannot be accessed with an instantiated class object
        // a static method can be accessed with an instantiated class object

    static $sauce = 'Curry';
    public static function myStaticMethod() {
        print 'I am static';
    }
}

echo CaiFanShop::carbohydrate; // prints 'Rice'
echo CaiFanShop::$sauce; // prints 'Curry'
CaiFanShop::myStaticMethod(); // prints 'I am static'

// Instantiate new instance object of name isle_eating_house of the class CaiFanShop
$isle_eating_house = new CaiFanShop('watermelon'); // the string 'watermelon' is assigned to the instance property $this->instanceProp in the constructor function

// Access class members
echo $isle_eating_house->property // prints 'public'
echo $isle_eating_house->instanceProp // prints 'watermelon'
echo $isle_eating_house->myMethod(); // prints 'MyClass'

// NULLSAFE OPERATORS
    // ? used when we're unsure whether an object contains a certain property or method, assigns null by default
    // can be used in conjunction with nullish coalescing operator to ensure proper default value

echo $isle_eating_house->invalid_property // error thrown since property invalid_property does not exist
echo $isle_eating_house?->invalid_property // since the object property invalid_property does not exist, it is assigned the value of null
echo $isle_eating_house?->invalid_property ?? "public" // since the object property invalid_property does not exist, it is assigned the value of null, and since its value is null, it is then assigned the value of string "public"

// CLASS EXTENDS CLASSES
    // inheritance of class methods and attributes
    // method overriding is supported simply by redefining the method
    // parent methods and attributes can be accessed using parent:: keyword if overridden by the child class
    // inherited methods and attributes can be accessed as their own if not overridden by the child class

// SimLimCaiFan is the child class, inheriting the attributes and methods of the CaiFanShop parent class
class SimLimCaiFan extends CaiFanShop {

    function printProtectedProperty() {
        echo $this->prot;
    }

    // overrrides the originally defined myMethod() in the CaiFanShop class
    function myMethod() {
        parent::myMethod(); // since we're overriding the child class' myMethod() method, the parent:: calls the parent CaiFanShop class' method myMethod() which prints "MyClass"
        print ' > MyOtherClass'; // prints "MyOtherClass"
    }

}

$sim_lim_cai_fan = new SimLimCaiFan('strawberry');
$sim_lim_cai_fan->printProtectedProperty(); // prints "protected" as defined in the parent class
$sim_lim_cai_fan->myMethod(); // prints 

// FINAL CLASS
    // final classes are unextendable

final class YouCannotExtendMe {

}

// ABSTRACT or INTERFACES
    // classes can be abstract or implement interfaces 
    // Abstract classes
        // abstract classes can have both abstract methods (no implementation) and concrete methods (with implementation)
        // abstract classes can have properties with or without values
        // single-inheritance
        // constructors can be declared
    // Interfaces 
        // all methods declared in an interface must be abstract (without implementation) as the implementing class provides the actual implementation
        // can only decalre constants, interfaces cannot assign properties with values
        // multiple-interhitance
        // no declaration of constructors
        // complete abstraction, since only method signature is specified and implementation detail left up to implementing class

// INTERFACES
    // declared with the interface keyword
    // interfaces can be extended
    // classes can implement more than one interface, comma-separated

interface InterfaceOne {
    public function doSomething();
}

interface InterfaceTwo {
    public function doSomethingElse();
}

// interfaces can be extended
interface InterfaceThree extends InterfaceTwo {
    public function doAnotherContract();
}

// classes can implement more than one interface
class SomeOtherClass implements InterfaceOne, InterfaceTwo {
    public function doSomething() {
        echo 'doSomething';
    }

    public function doSomethingElse() {
        echo 'doSomethingElse';
    }
}

// ABSTRACT CLASSES

abstract class MyAbstractClass implements InterfaceOne {
    public $x = 'doSomething';
}

class MyConcreteClass extends MyAbstractClass implements InterfaceTwo {
    public function doSomething() {
        echo $x;
    }

    public function doSomethingElse() {
        echo 'doSomethingElse';
    }
}

// TRAITS
    // introduced from PHP 5.4.0
    // declared using trait
    // classes implement a trait with use
    // appear to be somewhat similar to an interface but methods can have their signature and implementation detail specified


trait MyTrait {
    public function myTraitMethod() {
        print 'I have MyTrait';
    }
}

class MyTraitfulClass {
    use MyTrait;
}

$cls = new MyTraitfulClass();
$cls->myTraitMethod(); // this prints "I have MyTrait"

>
```

## Useful magic constants

* `__DIR__` returns full path directory of a file
* `__FILE__` returns full path of a file
* `__FUNCTION__` returns current function name
* `__LINE__` returns current line number
* `__CLASS__` returns current class name, must be used inside a class declaration
* `__METHOD__` returns current method name, only returns a value when called inside a trait or object declaration
* `__TRAIT__` returns current trait name, only returns a value when called inside a trait or object declaration
* `__NAMESPACE__` returns current namespace name

## More on

* `_get`
* `_set`
* `nowdoc`
* `heredoc`
* `phpinfo()`
* namespaces
* late static binding

# Web Applications

## Theory

Everything on the web is built in HTML, CSS, PHP, SQL, JS, JQuery, JSON.

## HTTP
 
HTTP connects the client-side browser to the web server and database server via the **request response cycle**.
* client-side browser: HTMl, CSS, DOM, CSSOM, JS, JQuery, JSON
* web server: Apache, PHP
* database server: MySQL

## Request response cycle

When a URL *(an anchor tag `<a></a>`)* is clicked, the client browser makes a connection to the web server and issues a GET **request** to retrieve the contents of the page at the specified URL.

This connection is also called a "handshake".

```console
# sample GET request
GET http://data.pr4e.org/page2.htm

# old telnet command to send a GET request to the server via the HTTP/1.0 protocol
GET http://data.pr4e.org/page1.htm HTTP/1.0 
```

The web server **responds** with the HTML contents which are received by the client browser. The HTML code is then parsed through the *DOM renderer* to display the HTML page.

## HTML

Parsed and rendered by the DOM.

* [HTML cheatsheet](https://www.geeksforgeeks.org/html-cheat-sheet-a-basic-guide-to-html/)

## CSS

Parsed and rendered by the CSSOM.

* [CSS cheatsheet](https://web.stanford.edu/group/csp/cs21/csscheatsheet.pdf)
* [CSS Tricks](https://css-tricks.com/)

## XAMPP

* [installation guide](https://www.wa4e.com/software-win.php)

## PHP

* [PHP superglobal](https://www.w3schools.com/php/php_superglobals.asp)
