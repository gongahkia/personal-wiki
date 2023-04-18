> *Edit on 13 April 2023:*
> * Read through [Rust book](https://doc.rust-lang.org/stable/book/) [Chapter 0/21], follow along w projects in [Rust lang playground](https://play.rust-lang.org/?version=stable&mode=debug&edition=2021), add relevant notes and minimal bloat here
> * Make notes off this video (https://www.youtube.com/watch?v=zF34dRivLOw)

# The Rust programming language 🦀

<img src="https://preview.redd.it/oifjvyix1us41.jpg?auto=webp&s=169dd56f3eb24adda52abc83cc98bce5765c8538" height="250px" align="right"/>

<h3 align="center">What is Rust?</h3>

Rust is a **strongly** and **statically-typed** compiled language developed by Mozilla Research, that can be used for *Systems programming* and *Web development* alike.

<h2 align="center">Quick start</h2>

### [Installation](https://www.rust-lang.org/tools/install)

---

### Usage

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

---

<h2 align="center">Compile time vs Runtime</h2>

Before we learn about variable declaration in Rust, we need to be aware of the [distinction](https://stackoverflow.com/questions/846103/runtime-vs-compile-time) between **Compile time** and **Runtime**.

<h4 align="center">Compile time ⚙️</h4>

* Period when program has been written, and is compiling *(using `rustc` / `g++` / `gcc` / `clangd`)*.
* Syntax errors, typechecking errors, compiler crashes can occur at this stage.
* If program succesfully compiled withput error messages, **assembly code** / **object code** / **executable file** created and can be run.

<h4 align="center">Runtime 🏃</h4>

* Period when **executable file** is run *(after compilation)*.
* Logical errors, Memory errors, File path and URL errors can occur at this stage *(ie. whatever errors that were not caught by the compiler)*.
* If program succesfully runs, it will output whatever the programmer intended for initially.

---

<h2 align="center">I love Rust ❤️</h2>

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

Also see:

* On the [differences](https://stackoverflow.com/questions/37877381/what-is-the-difference-between-immutable-and-const-variables-in-rust) between `let` and `const`.

---

<h3 align="center">Data types</h3>

#### Primitive data types

* `i8`, `i16`, `i32`, `i64`, `i128` 
    * Signed integer *(positive or negative)* 
    * the number indicates the **number of bits** the given integer can take up in memory  
  
* `u8`, `u16`, `u32`, `u64`, `u128`
    * Unsigned integer *(always positive)* 
    > for the same number of bits in memory, an unsigned integer can represent a **larger range of positive numbers** than a signed integer  

* `f32`, `f64` 
    * Floats 
    * `f32` is a 32-bit floating point *(single precision)*, `f64` is a 64-bit floating point *(double precision)*  

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

---

<h3 align="center"><a href="https://doc.rust-lang.org/std/collections/index.html">Data structures</a></h3>

* Arrays
    * **Fixed length** sequence of elements that are all the **same data type**.
    * Rust arrays are intialized with `[]` square brackets.
    * Statically define the data type of the array *(like in C++)*.

```Rust
let eg_array:[i32; 10] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

println!("{}", eg_array[7]);
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

println!("{}", eg_tuple.1);
```

---

### Conditional flow

Unlike many other languages, the **boolean condition** is not surrounded by brackets.

* `if`
* `else`
* `else if`

#### [Match construct](https://doc.rust-lang.org/book/ch06-02-match.html)

Rust also has the powerful `match` construct *(which functions similarly to `switch`, `case` statements in Typescript)*, and can be used to catch user-defined conditions, as well as errors and breakcases.

* `match` and `=>` *(match arms)* syntax

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

### Namespaces

* `::`
    * indicates the hierachy of crates, modules and symbols within a **namespace** *(similar to C++)*

        > In the below example...
        > * `std` is a ***CRATE*** *(a library)*
        > * `cmp` is a ***MODULE*** *(a source file)*
        > * `min` is a ***FUNCTION***

```Rust
let least = std::cmp::min(3,8); // this assigns i32 value of 3 to the variable least
```

* `use`
    * brings names from other namespaces *into scope* in your program

```Rust
use std::cmp::min;

let least = min(7,1); // this assigns i32 value of 1 to the variable least
```

* `{}` 
    * curly braces in the context of namespaces are called **globs**, and allow us to import multiple functions from other namespaces easily

```Rust
// this is valid code
use std::cmp::min;
use std::cmp::max;

// this is also valid code
use std::cmp::{min, max};

// this too is valid code
use std::{cmp::min, cmp::max};
```

* `*`
    * wildcard operator that imports **every symbol** from a namespace

```Rust
// this brings min, max and many other symbols from the std::cmp namespace into scope
use std::cmp::*;
```

### [Macros](https://doc.rust-lang.org/book/ch19-06-macros.html)

### [Underscore](https://runrust.miraheze.org/wiki/Underscore)

* `_` is a special character that indicates to Rust to **throw that value away** *(and acts as a catch-all pattern in match constructs)*.

```Rust
let _ = get_thing(); // calls the function but throws away its result that is returned to _

let _x = 42; // compiler will NOT warn about an unused variable if it is prefixed with an _

let (_, right) = slice.split_at(middle); // throw away the left side of the destructured tuple and only return the right
```

### Destructuring

Effectively the same concept as in *Typescript*, where we **extract values from data structures** *(arrays, tuples, vectors, structs, objects)*.

```Rust
// here is destructuring on a tuple, where some_char is now 'a' and some_int is now 17
let (some_char, some_int) = ('a', 17);

struct Vec2 {
    x:f64,
    y:f64,
}

let v = Vec2 { x:3.0, y:6.0 };

// struct destructuring, where x is now 3.0 and y is now 6.0
let Vec2 {x,y} = v;
```

### Blocks && Scope

Generally, **scope** is defined by a **block of code**.

* Fundamentally, <u>all blocks are expressions</u>, and all expressions evaluate to a value called the **tail** *(indicated by omission of semicolon)*, similar to Haskell.
* Blocks can have multiple initialization and assignment statements.
* Blocks exist anywhere in Rust there is code enclosed by `{}` curly braces *(functions, conditional and match statements, loops, objects)*.

```Rust
let x = {
    let y = 1;
    let z = 20;
    y + z // the tail, which evaluates to the int32 value of 21, assigned to x
};
```

### [Lifetimes](https://doc.rust-lang.org/rust-by-example/scope/lifetime.html)

***Variables*** and ***References*** *(including in function arguments)* have a fixed lifetime determined by the **scope** of curly braces they are within.

Also see:

* [Lifetimes in Rust for beginners](https://anooppoommen.medium.com/lifetimes-in-rust-7f2331be998b)
* [Understanding Lifetimes in Rust](https://blog.logrocket.com/understanding-lifetimes-in-rust/)
* [Lifetime elision](https://doc.rust-lang.org/nomicon/lifetime-elision.html)

### [Ownership](https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html)

#### [References](C++/pointers-references.md)

#### [Borrowing](https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html)

### [Smart pointers](https://doc.rust-lang.org/book/ch15-00-smart-pointers.html)

### Object-oriented programming

Rust **does not** have 'classes' or 'objects' *(in the conventional sense)*, and instead has [structs](#structs) and [traits](#traits).

Also see:

* On [complete rundown of Rust's implementation of OOP](https://stevedonovan.github.io/rust-gentle-intro/object-orientation.html)
* Another shorter article on [Rust's version of OOP](https://blog.devgenius.io/object-oriented-programming-in-rust-691baf4d2996)

#### [Structs](https://doc.rust-lang.org/rust-by-example/custom_types/structs.html)

Structs are somewhat similar to objects in *Typescript*, often referred to as **Types** in Rust *(and are Rust's provision for OOP patterns)*.

* declared with the `struct` keyword
    * intialized using **struct literals**
* user-defined **values** simply added to the struct
* user-defined **methods** and **traits** implemented with the [`impl`](https://doc.rust-lang.org/std/keyword.impl.html) keyword *(always used for a Type)*
    * `self` references the given Type

```Rust
struct Vec2 { // struct declaration
    x:f64,
    y:f64,
}

let v1 = Vec2 { x:1.0, y:3.0 }; // a struct literal
let v2 = Vec2 { y:2.0, x:4.0 }; // another struct literal, peep that the order does not matter

impl Vec2 { // implementing a method on a struct 
    fn is_strictly_positive(self) -> bool {
        self.value > 0
    }
}

println!("{}", v1.is_strictly_positive());
```

Also see:

* On [struct update syntax](https://users.rust-lang.org/t/the-struct-update-syntax/16519)
* On [structs as a whole](https://doc.rust-lang.org/book/ch05-01-defining-structs.html)

#### Traits

Traits are similar to interfaces in *Java*, allowing us to **declare** attributes and methods a type must have without defining them *(and allowing for OOP inheritance in Rust)*.

* declared with the `trait` keyword
* implement traits on types with the `impl` keyword *(`impl`, `for` syntax)*

```Rust
struct Number { // previously declared struct
    odd: bool;
    value: i32;
}

trait Signed {
    fn isStrictlyNegative(self) -> bool; // all this does is specify that the type must have a function called isStrictlyNegative that takes self as an argument and returns a boolean
}

impl Signed for Number { // implementing trait for a type
    fn isStrictlyNegative(self) -> (bool) {
        self.value < 0
    }
}

fn main() { // main function
    let n = Number { odd:false, value:-44 };
    println!("{}", n.isStrictlyNegative()); // this prints out true to the console
}
```

> Return to this in the future when I have more experience in Rust to try and understand the Orphan rules.

**[Rust's Orphan Rules](https://github.com/Ixrec/rust-orphan-rules):**
* Can implement one of your traits on foreign and primitive types *(like i32)* ✅
* Can implement other Rust-defined traits on one of our types ✅
* Cannot implement a foreign trait on a foreign type ❎

Also see:

* On [traits](https://doc.rust-lang.org/book/ch10-02-traits.html)
* On [marker traits](https://doc.rust-lang.org/std/marker/index.html)
* On [derive](https://doc.rust-lang.org/rust-by-example/trait/derive.html)

### [Enums](https://doc.rust-lang.org/book/ch06-01-defining-an-enum.html)

#### [Option](https://doc.rust-lang.org/rust-by-example/std/option.html)

#### [Result](https://doc.rust-lang.org/std/result/)

### [Pattern matching](https://doc.rust-lang.org/book/ch18-00-patterns.html)

Also see:

* On [pattern syntax](https://doc.rust-lang.org/book/ch18-03-pattern-syntax.html)

### [Generics](https://doc.rust-lang.org/rust-by-example/generics.html)

### [Package management](https://doc.rust-lang.org/cargo/)

### [Error Handling](https://doc.rust-lang.org/book/ch09-00-error-handling.html)

### [Concurrency 👥](https://doc.rust-lang.org/book/ch16-00-concurrency.html)

---

### Resources

> Return to these in a few months to see how much more Rust code I can understand.

* [Rust in 30 minutes](https://fasterthanli.me/articles/a-half-hour-to-learn-rust)
* [Learn Rust in Y minutes](https://learnxinyminutes.com/docs/rust/)
* [Rust Lang Book](https://doc.rust-lang.org/stable/book/)
* [Rust by example](https://doc.rust-lang.org/stable/rust-by-example/)
* [Rust Crash Course](https://www.youtube.com/watch?v=zF34dRivLOw)
* [Rust lang playground](https://play.rust-lang.org/?version=stable&mode=debug&edition=2021)
