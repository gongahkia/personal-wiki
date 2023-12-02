# PHP

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

$another_eg_array['Four'] = 4; // assigns a new element to the array
echo $eg_array['One']; // prints 1

// ARRAY LITERALS

$array_literal = ['One', 'Two', 'Three']; // implictly assigns integer key for array literals per their index

$array_literal[] = 'Four'; // appends an element to the end of the array
array_push($array_literal, 'Five'); // also appends an element to the end of the array
echo $array_literal[1]; // prints 'Two'
unset($array_literal[3]); // removes element from array

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

```php
<?php

// FUNCTION
    // default parameter values can be assigned

function shit() {
    return 'Shit';
}
echo shit(); // prints 'Shit'

function add($x, $y=1) { // $y assigned a default value of 1 and it is an optional parameter
    return $x + $y;
}

// ANONYMOUS FUNCTIONS

$anon = function($x) {
    return $x + 1;
}
echo $anon(2); // prints 3

// NESTED FUNCTIONS
    // functions can return functions
    // use keyword brings outside variables into scope

function ass($x, $y, $z) {
    echo "$x - $y - $z";
}
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

include 'my-file.php'; // code within my-file.php is now within scope, if the current file is not found it emitts a warning
include_once 'my-file.php'; // will not include code from my-file.php if its been included elsewhere

// REQUIRE
    // functions the same way as include except a fatal error is created if file cannot be included

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
* namespaces
* late static binding
