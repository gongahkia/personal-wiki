> *Edit on 11 April 2023:*
> * Continue making notes off [Learn Rust in 30 mins](https://fasterthanli.me/articles/a-half-hour-to-learn-rust) from "Dots" section
> * Make notes off [Learn Rust in Y minutes](https://learnxinyminutes.com/docs/rust/)
> * Read through [Rust book](https://doc.rust-lang.org/stable/book/) [Chapter 0/21], follow along w projects in [Rust lang playground](https://play.rust-lang.org/?version=stable&mode=debug&edition=2021), add relevant notes and minimal bloat here
> * Make notes off this video (https://www.youtube.com/watch?v=zF34dRivLOw)

# The Rust programming language 🦀

## Table of contents:
<img src="https://preview.redd.it/oifjvyix1us41.jpg?auto=webp&s=169dd56f3eb24adda52abc83cc98bce5765c8538" height="250px" align="right"/>
<!-- vim-markdown-toc GFM -->

* [What is Rust?](#what-is-rust)
* [Quick start](#quick-start)
    * [Installation](#installation)
    * [Usage](#usage)
* [Printing to the console && Comments](#printing-to-the-console--comments)
* [Compile time vs Runtime](#compile-time-vs-runtime)
    * [Compile time ⚙️](#compile-time-)
    * [Runtime 🏃](#runtime-)
* [Variables && Constants](#variables--constants)
* [Data types](#data-types)
    * [Primitive data types](#primitive-data-types)
* [Strings 🧵](#strings-)
* [Data structures](#data-structures)
* [Conditional flow](#conditional-flow)
    * [Match construct](#match-construct)
* [Equality](#equality)
* [Loops](#loops)
* [Functions](#functions)
* [Macros](#macros)
* [Object-oriented programming](#object-oriented-programming)
    * [Objects](#objects)
    * [Classes](#classes)
    * [Inheritance](#inheritance)
* [Methods](#methods)
* [Underscore](#underscore)
* [Destructuring](#destructuring)
* [Blocks && Scope](#blocks--scope)
* [Ownership](#ownership)
    * [References](#references)
    * [Borrowing](#borrowing)
* [Smart pointers](#smart-pointers)
* [Structs](#structs)
* [Enums](#enums)
* [Pattern matching](#pattern-matching)
* [Generics](#generics)
* [Package management](#package-management)
* [Error Handling](#error-handling)
* [Concurrency](#concurrency)
* [Resources](#resources)

<!-- vim-markdown-toc -->

--- 

### What is Rust?

Rust is a **strongly** and **statically-typed** compiled language developed by Mozilla Research, that can be used for *Systems programming* and *Web development* alike.

### Quick start

#### Installation

Refer [here](https://www.rust-lang.org/tools/install) for instructions on installation to your respective machine.

---

#### Usage

Rust files end with the `.rs` extension.

1. `rustc`
    * Calls the ***Rust compiler*** that compiles our program, creating a **binary executable file**.
        > The binary executable will be created with the same name as your Rust file/ Rust package.

```console
$ rustc main.rs
$ ./main
```

***Larger Rust projects*** with multiple dependencies should be initialized with Rust's build feature, `cargo`.

2. `cargo`
    * `cargo new {project name}` creates a new Rust package.
        > All Rust program code is written inside the `main.rs` file *(found inside the `src` folder)*.

```console
$ cargo new exampleProject
```
3. `cargo build` 
    * **Compiles** and **builds** the Rust project *(creating the corresponding executable file)*.
        > The executable file can be found inside the `/target/debug` file path.

```console
$ cargo build
$ cd target/debug
$ ./exampleProject
```

4. `cargo run` 
    * Immediately **compiles** and **runs** the Rust program *(used for quick checks when writing a Rust program)*.

```console
$ cargo run
```

5. `cargo check` 
    * **Checks** whether the Rust program can **compile without errors** *(without actually compiling the Rust program)*.

```console
$ cargo check
```

### Printing to the console && Comments

All Rust code to be executed is placed within the `fn main()` main function *(similar to C and Java)*.

* `println!()` **prints** text to the console
* `//` prefixes any **comments** in Rust
* `{}` used to embed variables and values in **formatted strings** *(similar to Bash)*

```Rust
fn main() {
    println!("Hello World!");
} // and this is a comment

let x:i32 = 40;
println!("x is: {}", x); // formatted strings in Rust look like this
```

### Compile time vs Runtime

Before we learn about variable declaration in Rust, we need to be aware of the [distinction](https://stackoverflow.com/questions/846103/runtime-vs-compile-time) between **Compile time** and **Runtime**.

#### Compile time ⚙️

* Period when program has been written, and is compiling *(using `rustc` / `g++` / `gcc` / `clangd`)*.
* Syntax errors, typechecking errors, compiler crashes can occur at this stage.
* If program succesfully compiled withput error messages, **assembly code** / **object code** / **executable file** created and can be run.

#### Runtime 🏃

* Period when **executable file** is run *(after compilation)*.
* Logical errors, Memory errors, File path and URL errors can occur at this stage *(ie. whatever errors that were not caught by the compiler)*.
* If program succesfully runs, it will output whatever the programmer intended for initially.

### Variables && Constants

> ***Cardinal Rule No. 1 🧛***   
> Variables in Rust are ***immutable*** by default.

* `let` initializes a variable, whose *value* and *data type* are immutable ***(at RUNTIME)***, relating to runtime computed values.
    * *Explicitly declare the variable's data type with a `:` colon*.

* `mut` keyword indicates that a variable is **mutable** *(its value can be changed later in the program)*.  

* `const` initializes a **constant**, whose *value* and *data type* are immutable ***(at COMPILE TIME)***.

```Rust
let x; // declaration and 
x = 100; // assignment can occur on two different lines!

let x:i32 = 10; // variable values are immutable by default

let mut y:i32 = 45; // mut creates a mutable variable
y = 100;
println!("y is: {}:, y");

const SECONDS_IN_MINUTES:u32 = 60; // const creates a constant, whose value cannot be changed
println!("{}", SECONDS_IN_MINUTES);
```

> On the [differences](https://stackoverflow.com/questions/37877381/what-is-the-difference-between-immutable-and-const-variables-in-rust) between `let` and `const`.

### Data types

#### Primitive data types

* `i8`, `i16`, `i32`, `i64`, `i128` 
    * Signed integer *(positive or negative)* 
    * the number indicates the **number of bits** the given integer can take up in memory  
  
* `u8`, `u16`, `u32`, `u64`, `u128`
    * Unsigned integer *(always positive)* 
    > for the same number of bits in memory, an unsigned integer can represent a **larger range of positive numbers** than a signed integer  

* `f32`, `f64` 
    * Floats 
    * `f32` is **single precision**, `f64` is **double precision**  

* `bool`
    * Boolean
    * `true` represented by 0, `false` represented by 1  

* `char`
    * Character
    * single characters are surrounded by `''` *single quotation marks*  

### Strings 🧵

* `&str`
    * **String literal**
    * strings surrounded by `""` *double quotation marks*

* `String`
    * **Heap-allocated string**
    * Stored as a `Vec<u8>` that holds a *valid UTF-8 sequence* that is not null-terminated

### Data structures

* Arrays
    * **Fixed length** sequence of elements that are all the **same data type**.
    * Rust arrays are intialized with `[]` square brackets.
    * Statically define the data type of the array *(like in C++)*.

```Rust
let eg_array:[i32; 10] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

println!("{}", eg_array[7]); // access array elements via square bracket notation
```

* Vectors
    * Arrays with dynamic length.
    * Rust vectors are initialized with `vec![]` and square brackets.
    * Statically define the data type of the vector *(like in C++)*.

```Rust
let mut eg_vector:Vec<i32> = vec![1,2,3,4,5,59,132];

vector.push(5); // appends the integer i32 of 5 to the vector
```

* Tuples
    * Arrays that can hold elements of **different data types**.
    * Rust tuples are initialized with `()` normal brackets.
    * Statically define the data type of each element in the tuple.

```Rust
let eg_tuple:(i32, bool, char) = (1, true, 'A');

println!("{}", eg_tuple.1); // access tuple elements via dot notation
```

### Conditional flow

Unlike many other languages, the **boolean condition** is not surrounded by brackets.

* `if`
* `else`
* `else if`

#### [Match construct](https://doc.rust-lang.org/book/ch06-02-match.html)

Rust also has the powerful `match` construct *(which functions similarly to `switch`, `case` statements in Typescript)*.

```Rust
fn im_feeling_lucky() -> i32 {
    match feeling_lucky {
        true => 6,
        false => 4,
    }
}
```

### [Equality](https://doc.rust-lang.org/book/appendix-02-operators.html)

* `==` checks for equality in **value**
* `!=` checks for inequality in **value**

### [Loops](https://doc.rust-lang.org/reference/expressions/loop-expr.html)

Unlike many other languages, the **loop condition** is not surrounded by brackets.

* `while` loop
* `loop`
* `for`, `in` loop
* `break`
* `continue`

```Rust
while 1 == 1 {
    println!("It works how you would expect, but this is an infinite loop my man!");
}

loop {
    println!("Alahoo"); // you can also create infinite loops easily with the loop keyword
}

// --- 

let an_array:[i32;3] = [1,2,3];
for i:i32 in an_array { // classic for in loop
    println!("{}", i);
}

// this can be applied to ranges too
for i in 0u32..10 {
    println!("{}", i); // this would print 0123456789 to the console
} 
```

### Functions

* `fn` **declares** a function
* `->` indicates the **return type** of said function
* `return` indicates the **return value** *(though it is optional since the function block evaluates to its tail regardless)*

```Rust
fn fair_dice_roll() -> i32 {
    return 4; // this is legal
}

fn unfair_dice_roll() -> i32 {
    10 // this is also legal
}
```

### Macros

### Object-oriented programming

#### Objects

#### Classes

#### Inheritance

### Methods

### Underscore

* `_` is a special character that indicates to Rust to **throw that value away**.

```Rust
let _ = get_thing(); // calls the function but throws away its result that is returned to _

let _x = 42; // compiler will NOT warn about an unused variable if it is prefixed with an _

let (_, right) = slice.split_at(middle); // throw away the left side of the destructured tuple and only return the right
```

### Destructuring

Effectively the same concept as in *Typescript*, where we **extract values from data structures**.

```Rust
// here is destructuring on a tuple, where some_char is now 'a' and some_int is now 17
let (some_char, some_int) = ('a', 17);
```

### Blocks && Scope

Generally, **scope** is defined by a **block of code**.

* Blocks are expressions, that evaluate to a value called the **tail** *(indicated by omission of semicolon)*.
* Blocks can have multiple initialization and assignment statements.
* Blocks exist anywhere in Rust there is code enclosed by `{}` curly braces *(functions, conditional and match statements, loops, objects)*.

```Rust
let x = {
    let y = 1;
    let z = 20;
    y + z // the tail, which evaluates to the int32 value of 21, assigned to x
};
```

### Ownership

#### References

#### Borrowing

### Smart pointers

### Structs

### Enums

### Pattern matching

### Generics

### Package management

### Error Handling

### Concurrency

### Resources

* [Rust quickies](https://fasterthanli.me/articles/a-half-hour-to-learn-rust)
* [Rust Lang Book](https://doc.rust-lang.org/stable/book/)
* [Rust Crash Course](https://www.youtube.com/watch?v=zF34dRivLOw)
* [Learn Rust in Y minutes](https://learnxinyminutes.com/docs/rust/)
* [Rust lang playground](https://play.rust-lang.org/?version=stable&mode=debug&edition=2021)
