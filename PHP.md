# `PHP`

PHP is a scripting language for the server.

## Comments

```php
// ---------- COMMENT ----------

// this is a single-line comment 

# this is also a single-line comment but the above is more common

/* 
    this is a multi-line
    comment so tyvm
*/
```

## Quickstart

```php
// ---------- QUICKSTART ----------
    // semicolon language
    // all runnable php code must be enclosed within <?php and > tags, anything outside of those tags is echoed automatically
    // values are printed to the stdout by default but can be printed to the webpage if php is running within the browser
        // print => prints a string to the stdout and does not include a newline, brackets are optional
        // echo => prints a string to the stdout and also does not include a newline, brackets are optional

<?php

print('Hello and this does not include a newline'); 
echo 'World and this also does not include a newline';
echo 100; // prints scalar integer value of 100 directly

>
```

## Variables and Types

```php
<?php

// ---------- VARIABLE ----------
    // variables declared with a $
    // variables CANNOT be declared, they are created at value assignment and their type is immediately assigned to them

// ---------- TYPE ----------

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
$another_formatted_string = "This is also a formatted string as seen in {$string_val} here."; // enclosing a variables in curly braces allows for evaluation of more complex statements within a string
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

// ---------- CONSTANT ----------
    // constants are defined using define() or the keyword const WITHOUT $ and cannot be changed at runtime
    // constants can be accessed by calling it without a $

const watermelon = "I cannot be changed at runtime, change me upon intialization only";
define("FOO", "something"); // defines a constant variable called FOO of string value 'something'
echo FOO; // prints 'something'
echo 'This outputs ' . FOO // prints 'This outputs something'

>
```

## Type conversion

```php
<?php

// ---------- TYPE CONVERSION ----------

$integer = 1;
echo $integer + $integer; // returns 2

$string = '1';
echo $string + $string; // variable $string coerced into type conversion to become an integer due to the + operator, returns 2

$string1 = 'one';
echo $string1 + $string1; // variable $string1 cannot be coerced into being type converted into an integer, so this returns 0

// there are also other dedicated functions for just type conversion such as strval()

>
```

## References

```php
<?php

// ---------- REFERENCE ----------
    // & => ampersand operator creates a reference to a variable which points to its place in memory

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

## Associative Arrays

```php
<?php

// ---------- ASSOCIATIVE ARRAYS ----------
    // similar to Lua's tables, all php arrays are implictly associative arrays with integer keys

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

// ---------- LOGIC ----------

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

// ---------- CONTROL STRUCTURE ----------

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

```php
<?php

// ---------- NULL COALESCING OPERATOR -----------
    // ?? => colescing operators assigns a default value if a variable stores a value of null 
        // syntax is as follows => {VALUE TO CHECK} ?? {DEFAULT VALUE TO ASSIGN IF THE AFOREMENTIONED VALUE IS NULL}
    // shorthand operator also available

$variable = $value ?? $default; // if $value is not null, $variable takes value of $value, else $variable takes value of $default
$a = null;
$b = 'Cereal chicken';
$a ?? 'Sambal beans'; // since $a is of value null, it is assigned the value 'Sambal beans'
$b ?? 'Thai fish'; // since $b already has value 'Cereal chicken', it keeps that value

>
```

## Loops

```php
<?php

// ---------- LOOP ----------
    // continue and break operate as expected in other languages

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

```php
<?php

// ---------- FUNCTION ----------
    // parameter => variable used in the function definition
    // argument => value passed during function invocation
    // function => declares and creates a function
    // return => specifies the return expression
    // & => reference operator as covered above, allowing us to call values by reference simiar to C
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

```php
<?php

// --------- ERROR HANDLING ---------
    // try and catch operate as you'd expect in other languages
    // Exception => specifies a special exception variable that can be stored and referenced

try {
    // Do something
} catch (Exception $e){
    // Handle the exception $e
}

>
```

## Importing from other files

```php
<?php

// ---------- IMPORTS -----------
    // include => searches for the specified file, if absent returns a non-fatal error, the equivalent of import in Python, emits a warning if specified file absent
    // include_once => searches the specified file, if alreayd imported then DOES NOT include it again to prevent circular importing, emits a warning if specified file absent
    // require => functions the same way as include except a fatal error is created if specified file absent
    // require_once => checks whether a file is already imported, if already imported then will NOT include it again to prevent circular importing, creates a fatal error if specified file absent
        // generally preferred for greater safety offered
    // spl_autoload_register() => built-in function that is called once at the beginning of the file, automatically loads predefined classes which are separated as their own php file, takes in a function name (string) or an anonymous function which receives a $class argument, where the function definition specifies the file path to find the predefined classes' .php files
        // allows for a templated and tiered approach to creating different classes and objects

include 'my-file.php'; 
include_once 'my-file.php'; 
require 'my-file.php'; 
require_once 'my-file.php';

spl_autoload_register(
    function($class){
        require_once "$class.php"; // specified file path to find predefined classes is same directory
    }
)

// spl_autoload_register should only be called once by right but called multiple times here for example's sake
spl_autoload_register( 
    function($class){
        require_once "model/$class.php"; // specified file path to find predefined classes is within the model file directory
    }
)

>
```

## OOP

```php
<?php

// ---------- OBJECT ORIENTED PROGRAMMING ----------

// ----- QUICK YAPPING ----- 

// GENERAL OOP principle 
    // AIM TO REDUCE COMPLEXITY
    // information hiding --> object properties accessed via defined methods like getters and setters
    // callers need not know of an object's inner workings like how exactly its methods are defined

// DATA ACCESS OBJECTS (DAO)
    // a class that provides access to data stored in a file or database, which could include other classes
        // effectively allows for chaining of class object methods that return other class objects which then call their own methods to obtain values
    // one DAO for each datatype (eg. personDAO, parklaneDAO, VehicleDAO)
    // supports CRUD operations (create, read, update, delete) as defined methods by the programmer
    // follows information hiding principles with complexity obscured by the DAO

$roadUser->getVehicle()->getType(); 

// code like above line is possible because of layered obscurity provided by the DAO
    // 1. $roadUser is a local variable that stores an instance object of the RoadUser class
    // 2. $roadUser -> getVehicle() returns an instance object of the Vehicle class
    // 3. getType() is a method called on the Vehicle class to return a value

// ----- PHP DATA OBJECTS (PDO) -----
    // perform CRUD operations on a connected database by interacting with DAOs using SQL queries
    // AVOID writing PDO object code in multiple PHP pages

// 1. Connect to Database by creating a PDO object 
    // PDO is a predefined class within PHP
    // PDO object instantiated with DSN, username and password
        // Data Source Name (DSN) consists of ...
            // database type and host
            // database name 
            // port
    // setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_SILENT) => used to silence any errors and exceptions raised by SQL, useful for final display but should be avoided during development

$dsn = "mysql:host=localhost;dbname=week11Test;port=3306";
$user = "root";
$password = ""; // password is "" for Windows and "root" for Mac
$pdo = new PDO($dsn, $user, $password); // all parameters defined for clarity here but they can be fed in as value literals for generic PDO object instantiation
$pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_SILENT);

// 2. Prepare an SQL statement called by PDO object using a method
    // SQL statement HAS to be within single quotes to create a string literal 
    // prepare() => creates a template SQL query statement that can be executed multiple times with different parameter values
        // each :XX value in the SQL query statement string is a parameter's placeholder value, where : prepends the placeholder within the SQL statement
        // takes in SQL query statement as an argument
    // bindParam() => associates a prepared SQL query's parameter with a PHP variable
        // effectively interpolates the specified variable into the prepared SQL query statement, necessary before execution
        // takes in parameter within SQL query statement, PHP variable to associate it with, and class constraints as arguments
            // CLASS CONSTRAINTS
                // 1. PDO::PARAM_STR => represents datatypes SQL CHAR, VARCHAR, other string types 
                    // !!! also use for SQL FLOAT, DOUBLE, DECIMAL and any datatypes not within other PDO class constraints
                // 2. PDO::PARAM_INT => represents datatype SQL INT
                // 3. PDO::PARAM_BOOL => represents datatype SQL BOOLEAN 
        // NOT ALWAYS NECESSARY to be used, used only when the SQL statement has parameters that are user-defined or variable to change

$isbn = 'isbn1';
$sqlStatement = 'select * from book where isbn = :isbn'; // here :isbn is the placeholder value
$preparedStatement = $pdo->prepare($sqlStatement);
$preparedStatement->bindParam(':isbn', $isbn, PDO::PARAM_STR);

    // 3. Execute an SQL statement 
        // execute() => runs the specified prepared and binded SQL query statement
            // takes no arguments
            // returns a Boolean value (True / False) depending on whether the given action executed succesfully
                // ! often stored within variables to check whether a given value inserted or deleted correctly etc

// just executes the job if no validation is required
$preparedStatement->execute();

// alternatively...
$validateInsertion = $preparedStatement->execute();

    // 4. Retrieve results row-by-row from queried SQL data
        // setFetchMode() => specifies how the rows retrieved from a SQL database query should be returned
            // takes in fetch constraint as an argument
                // FETCH CONSTRAINT
                    // PDO::FETCH_ASSOC
                    // PDO::FETCH_OBJ
                    // PDO::FETCH_BOTH
        // fetch() => iteratively retrieves the next row from the returned SQL result set in response to a database query
            // takes no arguments by default
            // note that while($row = $preparedStatement->fetch()) is not a comparison equality check but an ASSIGNMENT STATEMENT where the value of $preparedStatement->fetch() is assigned to $row
        // fetchColumn() => retrieves the single column value from the returned SQL resulte to a database query
            // takes no arguments by default
        // each SQL database column is a KEY associated with its corresponding record's stored as a VALUE in a KEY-VALUE pair relationship

$preparedStatement->setFetchMode(PDO::FETCH_ASSOC);
while($row = $preparedStatement->fetch()){
    echo $row['isbn'] . "    " . $row['title'];
}

// fetchColumn() makes things like this possible

$sqlStatement = "SELECT COUNT(*) FROM book";
$preparedStatement = $pdo->prepare($sqlStatement);
$preparedStatement->execute();
$count = $preparedStatement->fetchColumn();
echo "Here's your final count $count";

    // 5. Free up resources to close connection to Database and SQL statement

$preparedStatement = null;
$pdo = null;

// ----- DAO and PDO interaction -----
    // allows us to connect our DAO to the SQL database, useful because CRUD operations are normally called on DAO
    // the value of incorporating DAO and PDO lies in allowing for all database connection logic to be encapsulated within a single method
    // generally, individual object state (Book.php) is saved locally and collectively saved as a DAO (BookDAO.php), and the DAO is then written and read from the SQL database (since each record is the equivalent of an individual object and multiple records create a single table, which is the DAO)
        // ! combine the 5 steps covered above and place them within a defined class method

// < BookDAO.php >
class BookDAO {

    public function getAllBooks(){
        $fin = [];
        $dsn = "mysql:host=localhost;dbname=week11;port=3306";
        $pdo = new PDO($dsn, "root", "");
        $sql = 'SELECT * FROM book';
        $job = $pdo->prepare($sql);
        $job->execute();
        $job->setFetchMode(PDO::FETCH_ASSOC);
        while ($row = $job->fetch()){
            $fin[] = new Book($row['isbn']), $row['title']);
        }
        $job = null;
        $pdo = null;
        return $fin;
    }

    public function getSpecificBook(){
        $fin = null;
        $dsn = "mysql:host=localhost;dbname=week11;port=3306";
        $pdo = new PDO($dsn, "root", "");
        $sql = 'SELECT * FROM book WHERE isbn = :isbn';
        $job = $pdo->prepare($sql);
        $job->bindParam(':isbn', $isbn, PDO::PARAM_STR);
        $job->execute();
        $job->setFetchMode(PDO::FETCH_ASSOC);
        if($row = $job->fetch()){
            $fin = new Book($row['isbn'], $row['title']);
        }
        $job = null;
        $pdo = null;
        return $fin;
    }

    public function addSpecificBook($book){
        $dsn = "mysql:host=localhost;dbname=week11;port=3306";
        $pdo = new PDO($dsn, "root", "");
        $sql = 'INSERT INTO book (isbn, title) VALUES (:ibsn, :title)';
        $job = $pdo->prepare($sql);
        $currIsbn = $book->getIsbn();
        $currTitle = $book->getTitle();
        $job->bindParam(':isbn', $currIsbn, PDO::PARAM_STR);
        $job->bindParam(':title', $currTitle, PDO::PARAM_STR);
        $validateAdding = $job->execute();
        $job = null;
        $pdo = null;
        return $validateAdding;
    }

}

// CONNECTIONMANAGER class
    // the value of the ConnectionManager user-defined class is in preventing repeated code as seen above in DAO-PDO interaction

// < ConnectionManager.php >
class ConnectionManager { 
    public function getConnection() {
        $dsn = "mysql:host=localhost;dbname=week11;port=3306";
        $pdo = new PDO($dsn, "root", "");
        return $pdo;
    }
}

// < BookDAO.php >
    // assume that these are public methods declared within the class BookDAO
    // they all make use of the ConnectionManager class (remember to import it into your current PHP file!)
    // my code above now becomes this below instead

public class BookDAO {

    // declared variables, constructor functions etc ...

    public function getBooks() { 
        $connMgr = new ConnectionManager();      
        $pdo = $connMgr->getConnection();  
        $sql = 'SELECT * FROM book';         
        $stmt = $pdo->prepare($sql);
        $stmt->execute();
        $result = [];
        $stmt->setFetchMode(PDO::FETCH_ASSOC);
        while($row = $stmt->fetch()) {
            $result[] = new Book($row['isbn'], $row['title']);
        }
        $stmt = null;
        $pdo = null;
        return $result;
    }

    public function getBook($isbn) {
        $connMgr = new ConnectionManager();      
        $pdo = $connMgr->getConnection();  
        $sql = 'select * from book where isbn=:isbn';         
        $stmt = $pdo->prepare($sql);
        $stmt->bindParam(':isbn', $isbn, PDO::PARAM_STR);
        $stmt->execute();
        $result = null;
        $stmt->setFetchMode(PDO::FETCH_ASSOC);
        if($row = $stmt->fetch()) {
            $result = new Book($row['isbn'],$row['title']);
        }
        $stmt = null;
        $pdo = null;
        return $result;
    }

    public function add($book) {
        $connMgr = new ConnectionManager();      
        $pdo = $connMgr->getConnection(); 
        $sql = 'insert into book (isbn, title)
                values (:isbn, :title)';
        $stmt = $pdo->prepare($sql); 
        $title = $book->getTitle(); 
        $isbn = $book->getIsbn(); 
        $stmt->bindParam(':title', $title, PDO::PARAM_STR);
        $stmt->bindParam(':isbn', $isbn, PDO::PARAM_STR);
        $isAddOK = $stmt->execute();
        $stmt = null;
        $pdo = null;
        return $isAddOK;    
    }

}

// ----- ACCESS MODIFIERS -----

    // public
        // variable and methods VISIBLE from GLOBAL SCOPE
        // GENERALLY, GOOD OOP PRACTICE to declare class methods as public to allow them to be called from the global scope
            // methods are public by default if not specified

    // private
        // variable ACCESIBLE ONLY WITHIN CLASS only
        // variables and their scope can be declared without variable intialization (assigning a value to a variable)
        // GENERALLY, GOOD OOP PRACTICE TO declare class attributes as private to prevent unhandled mutation
            // class attributes are accessed via GETTERS and SETTERS

    // protected 
        // variable ACCESIBLE FROM CLASS AND SUBCLASSES

    // static
        // variable BELONGS TO CLASS and can ONLY BE CALLED FROM CLASS, not any of its objects

    // final
        // METHOD is unoverridable and unextendable

// ----- OOP SYNTAX -----

    // __construct()
        // constructor method automatically called upon an object's instantiation, similar to __init__ in Python
        // constructor method takes arguments that can be assigned to the instance object upon instantiation
    // new
        // instantiates a new instance object of a class
    // $this
        // represents an instance object (similar to .self in Python), used when calling instance variables and methods which are per object
        // always followed by -> operator (similar to . in Python)
        // remember NOT to include another $ after the -> before the instance attribute since $ already placed as part of the $this
    // self 
        // represents a class, used when calling class constants which are per class
        // always followed by :: operator
    // -> 
        // equivalent of . dot notation in Python
        // first called in constructor function to define the instance object's attributes, then called subsquently whenever instance attributes referenced
    // getter
        // generic term for class method that returns a private class property
    // setter
        // generic term for class method that modifies a private class property
    // class constant
        // class constants are declared WITHOUT $
        // constant attribute defined within a class that does not change during runtime, per class
        // self => class constant referenced from within the class by self::constantName
        // :: => class constant referenced from outside the class by className::constantName
    // {}
        // can be used to embed more complex statements (an object or class calling its method or attribute) within a string through string interpolation

class CaiFanShop {

    const carbohydrate = 'Rice'; // class constant

    public static $publicStaticVar = 'public static'; // public variable visible from global scope, static meaning it can only be accessed from within the class
    private static $privateStaticVar = 'private static'; // private variable accesible from within the class only
    protected static $protectedStaticVar = 'protected static'; // protected variable accessible from wihtin the class and its subclasses during inheritance

    // properties must declare their visibility
    public $property = 'public';
    public $instanceProp;
    protected $prot = 'protected';
    private $priv = 'private';

    // CONSTRUCTOR FUNCTION 

    public function __construct($instanceProp) { 
        $this->instanceProp = $instanceProp; // access instance variables with $this
    }

    public function shiok() {
        print 'MyClass';
    }

    public function myMethod() {
        print 'Look at {$this->shiok()}'; // made possible via string interpolation to embed complex statements in strings
    }

    // FINAL FUNCTIONS
        // final functions are unoverridable

    final function youCannotOverrideMe() {
    }

    // DESTRUCTOR FUNCTION
        // called when object is no longer referenced, used to deconstruct the object

    public function __destruct() {
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
* [superglobals](https://www.w3schools.com/php/php_superglobals.asp)

# Web Applications

```php
// ---------- WEB APPLICATION ----------
    // everything on the web is built in HTML, CSS, PHP, SQL, JS, JQuery, JSON
    // DEFINITION
        // HTTP => connects the client-side browser to the web server and database server via the request response cycle
        // client-side browser => rendered in HTMl, CSS, DOM, CSSOM, JS, JQuery, JSON
        // web server => Apache, PHP
        // database server => MySQL
        // dns server => equivalent of a telephone book that stores every hostname and its corresponding ip address
    // URL structure => {PROTOCOL}://{HOSTNAME}:{PORT}/{PATH}?{QUERY}
        // protocol => service used to connect to hostname, such as http, https, ftp
        // hostname => identifier of a web server
        // port => port 80 used by default, port number that specifies which service of a web server we want to call, since web servers can host multiple servicesI
        // path => points to a specific resource in the web server, separated by / similar to in the cli
        // query => optional parameter that augments the url, often including key-value pairs that are ampersand & operator delimited
    // REQUEST RESPONSE CYCLE
        // 1. client user makes a url request for a specific webpage
        // 2. client browser takes the url, extracts the hostname, and feeds that hostname to the dns server, which returns the corresonding ip address of the web server
        // 3. client browser makes a connection to the web server via that ip address and issues a http GET request to retrieve contents of the page at the specified url
        // 4. web server can optionally interact with other processing engines using php and sql to access the database server, and other external resources on the web server's side
        // 5. web server returns a http response with html, css and js contents to the client browser
        // 6. client browser parses html contents via dom renderer and css contents via cssom, then displays the webpage
```

# `HTML`

```html
<!-- quickstart -->

<!DOCTYPE html> <!-- comes at the start of everything else -->
<html>
    <head>
        <title>this is a title</title>
    </head>
    <body>

<!-- BASIC TEXT -->

        <p>this is a paragraph</p>
        <div>this is a div, often used to further augment styled sections of webpages</div>
        <br> <!-- used to indicate a breakline in html -->

<!-- LISTS -->

        <p>this is an unordered list of fruits</p>
        <ul>
            <li>apple</li>
            <li>watermelon</li>
            <li>orange</li>
            <li>lemon</li>
            <li>grape</li>
        </ul>
        <p>this is an ordered list of fruits</p>
        <ol>
            <li>john xina</li>
            <li>ma zongtong</li>
            <li>winnie the poo</li>
        </ol>

<!-- ANCHOR TAGS -->
    <!-- an anchor tag's href can point to an external url, a relative or absolute local filepath, or a specific webpage element based on the id specified with # -->

        <p>this is an <a href="http://www.google.com">anchor tag</a>

<!-- IMAGES -->
    <!-- src, width and height can be specified -->

        <img src="http://www.watermelonImages.com" width="170" height="227"/>

<!-- FORM -->
    <!-- action => specifies the filepath to send the request to -->
    <!-- method => specifies the type of request being made and how the message should be formatted, default values are post and get -->
        <!-- get => data sent with get is appended to the url and visible to the browser, so whatever information sent will be exposed -->
    <!-- form tags are always accompanied by input tags -->

<!-- INPUT -->
    <!-- self-closing tag -->
    <!-- type => specifies the type of input field -->
        <!-- 1. text => creates a single-lined input text field -->
            <!-- name => allows for specification of input tag name -->
            <!-- value => specifies a default value that will be displayed in the text field when webpage first loads -->
            <!-- size => specifies size of text field -->

        <form action="/yes.php" method="post">
            <input type="text" name="aName" value="defaultValue" size="20"/>
        </form>

        <!-- 2. submit => creates a submit button -->
            <!-- value => allows for specification of text that will replace the default Submit text within the submit button -->
            <!-- action => specifies the filepath to send the request too -->

        <form action="test.php">
           <input type="submit"/> 
        </form>

        <!-- 3. password => creates a text field where characters are obscured -->
            <!-- name => allows for specification of input tag name -->
            <!-- size => specifies size of password text field -->

        <form>
            <input type="password" name="pwd" size="20"/>
        </form>

        <!-- 4. radio => creates a radio button (which normally come in a set), of which selection is mutually exclusive so when one is selected, the others are automatically deselected -->
            <!-- name => allows for specification of input tag name, wherein specifically for radio tags, many related radio tags are grouped under the same name -->
            <!-- value => value needs to be distinct as it differentiates the radio tags from each other -->

        <form>
            <input name ="color" type="radio" value="r" checked/>Red
            <input name="color" type="radio" value="g"/>Green
            <input name="color" type="radio" value="b"/>Blue
        </form>

        <!-- 5. checkbox => creates a checkbox (which normally comes in a set), of which more than one checkbox can be selected -->
            <!-- name => allows for specification of input tag name, wherein specifically for checkbox tags, many related checkbox tags are grouped under the same name -->
            <!-- value => value needs to be distinct as it differentiates the checkbox tags from each other -->
            <!-- checked => an additional parameter that can be specified to initialize a checkbox as already being checked -->

        <form>
            <input name="color[]" type="checkbox" value="r" checked/>Red
            <input name="color[]" type="checkbox" value="g" checked/>Green <!-- note that the red and green checkboxes are checked by default here -->
            <input name="color[]" type="checkbox" value="b"/>Blue
        </form>

        <!-- 6. hidden => creates a hidden section that obscures data from the webpage client user -->
            <!-- TAKE NOTE OF THIS TO USE -->
            <!-- name => the name attribute is the key in the key-value pair stored within the submission protocol method (GET/POST) superglobal variable --> 
            <!-- value => programmer can save state across form submissions by ASSIGNING the value in the value attribute within a hidden form field -->
                <!-- hidden input names and values will be submitted alongside any other input fields under the same FORM TAG when the submit button is pressed -->
            <!-- normally HIDDEN input tags are paired with the ACTION attribute within the FORM tag (which specifies the php file to be loaded when submission button is pressed) to preserve input values across multiple sessions-->
        
        <input name="key-pair" value="value-pair" type="hidden">

        <!-- eg. of preserving values using input.php -->

            <!-- page1.php -->

            <html>
                <body>
                    <form method="post" action="page2.php"> <!-- page2.php will be loaded when the submission button is pressed -->
                        <label>Name: <input type="text" name="name"/></label>
                        <input type="submit" value="Next"/>
                    </form>
                </body>
            </html>

            <!-- page2.php -->

            <html>
                <body>
                    <form method="post" action="page3.php"> <!-- page3.php will be loaded when the submission button is pressed -->
                        Age: <input type="text" name="age"/>
                        <?php
                            $name = $_POST['name'];
                            echo "<input type='hidden' name='name' value='$name'/>"; // assigning the value name to the hidden input field, which is submitted to page3.php so we can access the name key within the POST superglobal associative array later
                        ?>
                        <input type="submit" value="Next" name='check'/>
                    </form>
                </body>
            </html>

            <!-- page3.php -->

            <html>
                <body>
                    <form method="post" action="summary.php"> <!-- summary.php will be loaded when the submission button is pressed -->
                        Hobby: <input type="text" name="hobby"/>
                        <?php
                            if (isset($_POST["check"])){ // check if form is submitted, though unnecessary if next form display predicated on submission button already being clicked
                                $name = $_POST["name"];
                                $age = $_POST["age"];
                                echo "
                                    <input type='hidden' name='name' value='$name'></input>
                                    <input type='hidden' name='age' value='$age'></input>
                                "; // assigning the values name and age to the hidden input field, which is submitted to summary.php so we can access the name keys within the POST superglobal associative array later
                            } else {} // form not submitted
                        ?>
                        <input type="submit" value="Next"/>
                    </form>
                </body>
            </html>

        <!-- summary.php -->

            <?php
                // display all 3 values, which have been retained through the hidden input tag
                $name =     $_POST["name"];
                $age = $_POST["age"];
                $hobby = $_POST["hobby"];

                echo "Name: $name<br>"; 
                echo "Age: $age<br>";
                echo "Hobby: $hobby";
            ?>
 

        <!-- 7. reset => resets all webpage components in the html form to their default values, does not send any form data to the web server -->

        <input name="value-you-want" type="reset">

<!-- LABEL -->
    <!-- label => creates label text that is associated with an input tag -->
        <!-- for => links a label to a specified input tag via the input tag's id -->

        <form>
            <input name="color" type="radio" value="g" id="color_g"/>
            <label for="color_g">Green</label>
        </form>

<!-- TEXTAREA -->
    <!-- textarea => creates a mutli-line text input area -->
        <!-- rows => specifies number of rows within the text area -->
        <!-- cols => specifies the number of columns within the text area -->
        <!-- name => specifies a name for the text area for easier identification -->
        <!-- default text => default filler text to be initialized with the text area upon webpage loading can be specified within the textarea opening and closing tags -->

        <form>
            <textarea name="comment" rows="2" cols="30">No comments</textarea>
        </form>

<!-- DROPDOWN LIST -->
    <!-- select => creates a dropdown list whose options can be selected from -->
        <!-- name => specifies a name for the dropdown list for easier identification -->
        <!-- size => specifies the number of options to be visible at a given point of time within the dropdown list viewport -->
        <!-- multiple => used to allow for more than one option to be selected -->
        <!-- option => creates options within the given dropdown field -->
            <!-- value => assigned a unique value to distinguish different selection options from each other within a dropdown list -->

<!-- TABLE -->
    <!-- table => creates a table, comes as a pair with its closing tag -->
        <!-- border => takes an integer value, specifies the thickness of table borders -->
        <!-- colspan => specifies how many additional columns a given cell should occupy -->
        <!-- rowspan => specifies how many additional rows a given cell should occupy -->
    <!-- th => defines tables headers -->
    <!-- tr => defines table rows -->
    <!-- td => specifies table data within cells -->

    <table>
        <th>department</th>
        <th>number</th>
        <tr>
            <td>Fire/Ambulance</td>
            <td>995</td>
        </tr>
        <tr>
            <td>Police</td>
            <td>999</td>
        </tr>
    </table>

<!-- Connect HTML to PHP -->
    <!-- done in context of form handling as covered above -->
    <!-- method => specifies the method for form data to be sent to the web server -->
    <!-- !!! action => specifies the NEXT PHP FILE LOADED when the submission button is pressed, allowing us to handle form data and parse and interact as needed -->

    <form method="post" action="pangsai.php"> <!-- this will run pangsai.php when the submission button is pressed -->
        Enter your name:
        <input type="text" name="fullname"/>
        <input type="submit" value="send"/>
    </form>

    <form method="post" action="form2.php"> <!-- this will run form2.php when the submission button is pressed -->
        Enter your placeholder:
        <input type="text" name="identifier"></input>
    </form>

<!-- HTTP SESSIONS -->
    <!-- stores data shared between USER and the WEBSITE -->
    <!-- stores data across MULTIPLE pages -->
        <!-- !!! DATA AUTOMATICALLY RESET AFTER given period of time -->
        <!-- alternative to hidden input tags or GET URLs -->
    <!-- used to... -->
        <!-- preserve data across MULTIPLE pages -->
        <!-- identify users across MULTIPLE pages in a site -->
    <!-- session_start() -->
        <!-- initializes a session OR resumes existing session -->
        <!-- must be called FIRST @ the START OF EVERY FILE before we can access the $_SESSION superglobal associative array and assign key-value pairs to it WITHIN THAT FILE -->
    <!-- unset($_SESSION[example-key]) -->
        <!-- allows for clearing of all stored variables within a specified key in the SESSION superglobal associative array -->
        <!-- equivalent to assigning an empty array [] value to a given SESSION superglobal associative array key -->
    <!-- session_unset() -->
        <!-- clears a session on the client IMMEDIATELY by deleting session cookies and freeing all session variables currently registerd -->
    <!-- session_destroy() -->
        <!-- destroys a session on the server but client-side cookies will remain until BROWSER CLOSED-->

<!-- eg. using SESSION to store data across multiple pages -->

    <!-- session1.php -->

    <html>
        <body>
            <form method='POST' action='session2.php'>
                Name: <input type='text' name='name'></input>
                <input type='submit' value='next'></input>
            </form>
        </body>
    </htmL>

    <!-- session2.php -->

    <?php
        session_start(); 
    ?>
    <html>
        <body>
            <form method='POST' action='summary.php'>
                Age: <input type='text' name='age'></input>
                <?php
                    $_SESSION['name'] = $_POST['name']; // assigns the key-value pair of name = {value stored in $_POST['name']} to the SESSION superglobal associative array, and values can be retrieved anytime
                ?>
                <input type='submit' value='next'></input>
            </form>
        </body>
    </htmL>

    <!-- summary.php -->

    <?php 
        session_start(); // must call session_start() again to resume the session and have accesss to superglobal associative array SESSION within the file summary.php
        $name = $_SESSION['name'];
        $age = $_SESSION['age'];

        echo "Name: $name<br>";
        echo "Age: $age";
    ?>

<!-- eg. using SESSION to store mutating data across the same page -->

<?php

session_start(); // allows for access of the SESSION superglobal associative array
if (!isset($_SESSION['count'])){
    $_SESSION('count') = 0; // initializes the count key within the superglobal associative array $SESSION if it doesn't exist
}
$_SESSION['count']++;
echo "You have accessed this page " . $_SESSION['count'] . " times";

?>

<!-- SUPERGLOBALS -->
    <!-- superglobals are associative arrays that are ALWAYS accessible REGARDLESS OF SCOPE, and are often used to store important values returned from the html form or assigned by the programmer -->
    <!-- $_POST => collects form data submitted using the specified http post request, where data IS NOT visible in the url (so often used for transmission of sensitive form data) -->
        <!-- it is considered good convention to include a name for your submit button and check whether the submit button is clicked using isset() -->
    <!-- $_GET => collects form data submitted using the specified http get method, where data IS visible in the url (so often used for transmission of neutral non-confidential form data) -->
        <!-- it is considered good convention to include a name for your submit button and check whether the submit button is clicked using isset() -->
    <!-- $_REQUEST => collects form data from both $_GET and $_POST as well as $_COOKIE -->
    <!-- $_SESSION -->
        <!-- $_SESSION[{key-name}] -->
        <!-- session_start() -->
        <!-- session_destroy() -->
        <!-- session_unset() -->
    <!-- other superglobals to check out -->
        <!-- $GLOBALS -->
        <!-- $_SERVER -->
        <!-- $_FILES -->
        <!-- $_ENV -->
        <!-- $_COOKIE -->

<!-- NUGGETS OF INFORMATION -->
    <!-- ANCHOR TAGS can also carry extraneous information from one "page" to another where the URL is used to specify addiitonal data -->

<!-- eg. of anchor tags carrying info across pages -->

    <!-- main.php -->

    <html>
        <body>
            <a href='view_object.php?src=cat.png&width=500'>View Object</a>
        </body>
    </html>

    <!-- view_object.php -->

    <?php
        echo "
        <img src='{$_GET["src"]}' width='{$_GET["width"]}'/> // here, view_object.php can retrieve src and width names from the GET superglobal associative array because they have technically been "submitted" in the URL of the GET request which exposes all user input and customisation within the URL
        " 
    ?>

    </body>
</html>
````

## More on

* [HTML cheatsheet](https://www.geeksforgeeks.org/html-cheat-sheet-a-basic-guide-to-html/)
* [CSS cheatsheet](https://web.stanford.edu/group/csp/cs21/csscheatsheet.pdf)
* [CSS tricks](https://css-tricks.com/)
