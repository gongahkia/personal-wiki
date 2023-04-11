> *Edit on 11 April 2023:*
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
* [Variables && Constants](#variables--constants)
* [Data types](#data-types)
    * [Primitive data types](#primitive-data-types)
* [Data structures](#data-structures)
* [Conditional flow](#conditional-flow)
* [Equality](#equality)
* [Loops](#loops)
* [Functions](#functions)
* [Macros](#macros)
* [Object-oriented programming](#object-oriented-programming)
    * [Objects](#objects)
    * [Classes](#classes)
    * [Inheritance](#inheritance)
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

Rust is a **strongly** and **statically-typed** compiled language, that can be used for *Systems programming* and *Web development* alike.

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

### Variables && Constants

> ***Cardinal Rule No. 1 🧛***   
> Variables in Rust are ***immutable*** by default.

* `let` declares and creates a variable *(explicitly declare the variable's data type with a `:` colon)*.
* `mut` keyword indicates that a variable is **mutable** *(its value can now be changed later in the program)*.
* `const` declares and creates a **constant**, whose **value** and **data type** cannot be changed throughout the program

```Rust
let x:i32 = 10; // variable values are immutable by default

let mut y:i32 = 45; // mut creates a mutable variable
y = 100;
println!("y is: {}:, y");

const SECONDS_IN_MINUTES:u32 = 60; // const creates a constant, whose value cannot be changed
println!("{}", SECONDS_IN_MINUTES);
```

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

### Data structures

* String
    > *add more notes on this later from Rust book*

* Arrays
    * Rust arrays are intialized with `[]` square brackets.
    * **Fixed length** sequence of elements that are all the **same data type**.
    * Statically define the data type of the array *(like in C++)*.

```Rust
let eg_array:[i32; 10] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

println!("{}", eg_array[7]); // access array elements via square bracket notation
```

* Tuples
    * Rust tuples are initialized with `()` normal brackets.
    * Arrays that can hold elements of **different data types**.
    * Statically define the data type of each element in the tuple.

```Rust
let eg_tuple:(i32, bool, char) = (1, true, 'A');

println!("{}", eg_tuple.1); // access tuple elements via dot notation
```

### Conditional flow

### Equality

### Loops

### Functions

### Macros

### Object-oriented programming

#### Objects

#### Classes

#### Inheritance

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
