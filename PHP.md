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

## LOGIC FLOW

```php
<?php



>
```

## More on

* `nowdoc`
* `heredoc`
