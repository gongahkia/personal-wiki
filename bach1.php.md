> *Edit on 4 April 2023*: 
> * Continue BroCode's PHP tutorial on youtube from [03:26:07] when I have learned SQL.
> * Continue adding notes here for OOP in PHP.

# The PHP scripting language

<p align="center">
    <img src="https://d1zviajkun9gxg.cloudfront.net/user/prod/2020/01/03/fastpages-0622317d-e016-4e7a-b25e-8eef9db1610a.png"/>
</p>

PHP is a *server-side* scripting language used to build dynamic web pages that run on the **server** instead of on the user's browser.  

## Quick Start

### Installing XAMPP and running PHP 

> Since PHP is *executed on the server* and *returned to the browser*, we have to use a **web server** like [XAMPP](https://www.apachefriends.org/download.html) to run PHP code.

### Starting the XAMPP web server

```console
$ sudo /opt/lampp/lampp start
```

### Stopping the XAMPP web server

```console
$ sudo /opt/lampp/lampp stop
```

### Testing whether XAMPP is up and running

Enter `http://localhost` into a web browser, it should return *XAMPP start page*.

### Render PHP code to the browser

Enter `https://localhost/{local-file-directory-name}` into a web browser to display PHP code that has been executed on the server.  

> *eg. My folder within `~/../../opt/lampp/htdocs` is titled `website`, so running `https://localhost/website` will render any code from `.php` files within the `website` file directory to the browser.*

### Interaction with HTML

`$_GET` and `$_POST` are *special variables* that store data from form input fields in HTML.  

> * Method *(post / get)* has to be specified in the HTML `<form>` tag.  
> * Key-values pairs are stored in a dictionary, and values can be accessed via `[]` square bracket notation.

* `$_GET`
    * data is appended to the *URL*
    * **not secure** *(appends all key-value pairs to URL)*
    * character limit
    * can be bookmarked
    * `$_GET` requests *can be cached*
    * better for ***search page***

* `$_POST`
    * data is packaged inside body of *http request*
    * **more secure** *(URL unchanged)*
    * no data limit
    * cannot be bookmarked
    * `$_POST` requests are *not cached*
    * better for ***submitting credentials***

*Sample `$_POST` request:*

```php
<!doctype html>
<html>
<head>
    <title>Webpage01</title>
</head>
<body>
    <form action="index.php" method="post">
        <label>username:</label><br>
        <input type="text" name="username"><br>
        <label>password:</label><br>
        <input type="password" name="password"><br>
        <input type="submit" value="Log in">
    </form>
</body>
</html>

<?php
    echo $_POST["username"];
    echo $_POST["password"];
?>
```

---

## Stuff unique to PHP 🐘

*HTML*, *CSS* and *Javascript* code can be written in a `.php` file.

```PHP
<?php

    echo "lol f";

?>

<!doctype html>
<html>
    <head>
        <title>Gooofy-looking ass</title>
    </head>
    <body>
        <button>testing</button>
    </body>
</html>
```

### Syntax and comments

* Any valid PHP code is surrounded by `<?php` and `?>` tags *(within a `.php` file)*.
* `#` or `//` for **single-line** comments.
* `/*` and `*/` for **multi-line** comments.

```PHP
<?php

# this is a single-line comment
// this is also a single-line comment

/* this 
is a 
multi-line comment
*/

?>
```

### Printing to the browser && formatted strings

* `echo` prints to the web browser
* `<br>` is a **break tag** *(similar to newline characters (`\n`) in other languages)*

```PHP
<?php

echo "Hello";
echo "World!";
echo "shitass<br>Yes please"; // "shitass" and "Yes please" are printed to 2 different lines

?>
```

* `{}` allows us to embed variables / values within a string *(somewhat similar to Typescript)*

```PHP
<?php

$number = 123;
$waterBottle = 20;
$okAnd = "thanks";
$total = $number * $waterBottle;

echo "Arigato, \${$number}, thank you<br>";
echo "Thanks a lot {$okAnd}<br>";
echo "That's a total of \${$total} and a quantity of ${number};

?>
```

Also see:

> * [`sprintf()`](https://www.w3schools.com/php/func_string_sprintf.asp)
>   * creates a **formatted string**
>    * takes in 2 or more arguments, the **string** *(embedded values represented by `%`)* and the **values we want to embed inside the string**


### Variable declaration

PHP is a loosely-typed language.

* All variables are prefixed with the `$` dollarsign character from **initialization**.

```php
<?php

$exampleString = "shit ass";
echo $exampleString;

?>
```

### Data Types

* `string`
    * *"this is a string"*
* `integer`
    * whole number integers
* `float`
    * covers **float** *(1.2345)* and **double** *(2.029348319489234829)* data types
* `boolean`
    * *true, false*
* `null`
    * absent or unintialized values

We can check the data type of a variable/value with `gettype()` function.

### Data Structures

* Arrays
    * PHP arrays are initialized with `array()` function.
    * Array elements are accessed via `[]` square bracket notation.
    * [Array functions](https://www.w3schools.com/php/php_ref_array.asp).

```php
<?php

$foodArray = array("apple", "orange", "banana", "coconut");
echo $foodArray[0]; // access individual elements

foreach($foodArray as $food) { // foreach loop, as previously mentioned
    echo $food . "<br>";
}

?>
```

* Associative arrays  
    * Equivalent of **dictionary** in *Python*.
    * PHP associative arrays are initialized with `array()` function and `=>` arrow indicating **key-value** pairs.
    * Associative array elements are accessed via `[]` square bracket notation.
    * [Associative array functions](https://www.w3schools.com/php/php_ref_array.asp).

```php
<?php

$capitals = array("USA"=>"Washington DC",
                  "Japan"=>"Kyoto",
                  "South Korea"=>"Seoul",
                  "India"=>"New Delhi");

echo $capitals["South Korea"]; // access individual values via their respective keys

foreach($capitals as $key => $value) {
    echo "{$key} = {$value} <br>";
}

?>
```

On more [data structures 01](https://www.php.net/manual/en/spl.datastructures.php) in PHP.  
On more [data structures 02](https://medium.com/@rtheunissen/efficient-data-structures-for-php-7-9dda7af674cd) in PHP.

### Conditional flow

Conditional flow, logical operators, equality checks and arithmetic operations work as expected in PHP.

* `if`
* `else if`
* `else`
* `switch`, `case`, `default`
* `break`
* `continue`

### Logical operators

* `&&`
* `||`
* `!`

### Equality

* `==` checks for equality in **value**
* `!=` checks for inequality in **value**
* `===` checks for strict equality in **value** and **type**
* `!==` checks for strict inequality in **value** and **type**

### Arithmetic 

* `+` addition
* `-` subtraction
* `*` multiplication
* `/` division
* `**` to the power of
* `%` modulo
* `++` increment by 1 
* `--` decrement by 1
* `+=` increment by a given value
* `-=` decrement by a given value

> On more [math functions](https://www.w3schools.com/php/php_ref_math.asp) in PHP.

### Loops

* `for`
* `while`
* `do`, `while`
* [`foreach`](https://www.w3schools.com/php/php_looping_foreach.asp) => only usable for *arrays* && *associative arrays*

### Functions

Functions are initialized with the `function` keyword.

```php
<?php

    function happyBirthday() {
        echo "Ah Shit";
        echo "shit ass";
        echo "I haven't put in any break tags yet";
    }
    
    happyBirthday();

?>
```

---

### Object-oriented Programming

PHP handles [OOP](https://www.w3schools.com/php/php_oop_classes_objects.asp) somewhat similarly to Typescript and Java. 

### Objects

### Classes

### Scoping

---

### Of interest

* [`isset()`](https://www.w3schools.com/php/func_var_isset.asp) => interact with HTML elements *(text fields / radio buttons / checkboxes)*
* [`empty()`](https://www.w3schools.com/php/func_var_empty.asp)
* [`filter_input()`](https://www.w3schools.com/php/func_filter_input.asp) => *sanitize user input* prior to processing it
* [`include()`](https://www.w3schools.com/php/php_includes.asp)

* Cookies
    * [`setcookie()`](https://www.w3schools.com/php/func_network_setcookie.asp)

* Sessions
    * [`$_SESSION`](https://www.w3schools.com/php/php_sessions.asp)
    * [`session_start()`](https://www.php.net/manual/en/function.session-start.php)
    * [`session_unset()`](https://www.php.net/manual/en/function.session-unset.php)
    * [`session_destroy()`](https://www.php.net/manual/en/function.session-destroy.php)  

* Server
    * [`$_SERVER`](https://www.w3schools.com/php/php_superglobals_server.asp)

* Password hashing
    * [`password_hash()`](https://www.php.net/manual/en/function.password-hash.php)
    * [`password_verify()`](https://www.php.net/manual/en/function.password-verify.php)

* General purpose functions
    * [string functions](https://www.w3schools.com/php/php_ref_string.asp)
