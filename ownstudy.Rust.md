> *Edit on 24 April 2023:*
> * Read through [Rust book](https://doc.rust-lang.org/stable/book/) [pg 102/554], add notes on Ownership section.
> * Make notes off this video (https://www.youtube.com/watch?v=zF34dRivLOw).

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

```rust
fn main() {
    println!("Hello World!");
} 

// this is a comment

// formatted strings in Rust look like this
let x:i32 = 40;
println!("x is: {}", x); 
```

### Variables && Constants

> ***Cardinal Rule No. 1 🧛***   
> Variables in Rust are ***immutable*** by default.

* `let` initializes a variable, whose *value* and *data type* are immutable ***(at RUNTIME)***, relating to runtime computed values.
    * *Explicitly declare the variable's data type with a `:` colon*.

* `mut` keyword indicates that a variable is **mutable** *(its value can be changed later in the program)*.  

* `const` initializes a **constant**, whose *value* and *data type* are immutable ***(at COMPILE TIME)***.

```rust
// declaration and 
let x; 

// assignment can occur on two different lines!
x = 100; 

// variable values are immutable by default
let x:i32 = 10;

// mut creates a mutable variable
let mut y:i32 = 45;
y = 100;
println!("y is: {}:, y");

// const creates a constant, whose value cannot be changed
const SECONDS_IN_MINUTES:u32 = 60; 
println!("{}", SECONDS_IN_MINUTES);
```

> Also see:
> 
> * On the [differences](https://stackoverflow.com/questions/37877381/what-is-the-difference-between-immutable-and-const-variables-in-rust) between `let` and `const`.

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
    * **String literal, stored on the stack**
    * Immutable
    * strings surrounded by `""` *double quotation marks*

* `String`
    * **Heap-allocated string**
    * Mutable
    * Stored as a `Vec<u8>` that holds a *valid UTF-8 sequence* that is not null-terminated

---

<h3 align="center"><a href="https://doc.rust-lang.org/std/collections/index.html">Data structures</a></h3>

* Arrays
    * **Fixed length** sequence of elements that are all the **same data type**.
    * Rust arrays are intialized with `[]` square brackets.
    * Statically define the data type of the array *(like in C++)*.

```rust
let eg_array:[i32; 10] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

println!("{}", eg_array[7]);
```

* Vectors
    * Arrays with dynamic length.
    * Rust vectors are initialized with `vec![]` and square brackets.
    * Statically define the data type of the vector *(like in C++)*.

```rust
let mut eg_vector:Vec<i32> = vec![1,2,3,4,5,59,132];

// appends the integer i32 of 5 to the vector
vector.push(5);
```

* Tuples
    * Arrays that can hold elements of **different data types**.
    * Rust tuples are initialized with `()` normal brackets.
    * Statically define the data type of each element in the tuple.

```rust
let eg_tuple:(i32, bool, char) = (1, true, 'A');

println!("{}", eg_tuple.1);
```

---

<h3 align="center">Conditional flow</h3>

Unlike many other languages, the **boolean condition** is not surrounded by brackets.

* `if`
* `else`
* `else if`

#### [Match construct](https://doc.rust-lang.org/book/ch06-02-match.html)

Rust also has the powerful `match` construct *(which functions similarly to `switch`, `case` statements in Typescript)*, and can be used to catch user-defined conditions, as well as errors and breakcases.

* `match` and `=>` *(match arms)* syntax

```rust
fn im_feeling_lucky() -> i32 {
    match feeling_lucky {
        true => 6,
        false => 4,
    }
}
```

<h3 align="center"><a href="https://doc.rust-lang.org/book/appendix-02-operators.html">Equality</a></h3>

* `==` checks for equality in **value**
* `!=` checks for inequality in **value**

<h3 align="center"><a href="https://doc.rust-lang.org/reference/expressions/loop-expr.html">Loops</a></h3>

Unlike many other languages, the **loop condition** is not surrounded by brackets.

* `while` loop
* `loop`
* `for`, `in` loop
* `break`
* `continue`

```rust
while 1 == 1 {
    println!("It works how you would expect, but this is an infinite loop my man!");
}

// you can also create infinite loops easily with the loop keyword
loop {
    println!("Alahoo"); 
}

// --- 

let an_array:[i32;3] = [1,2,3];

// classic for in loop
for i:i32 in an_array {
    println!("{}", i);
}

// this can be applied to ranges too
// this would print 0123456789 to the console
for i in 0u32..10 {
    println!("{}", i); 
} 
```

<h3 align="center">Functions</h3>

* `fn` **declares** a function
* `->` indicates the **return type** of said function
* `return` indicates the **return value** *(though it is optional since the function block evaluates to its tail regardless)*

```rust
// this is legal
fn fair_dice_roll() -> i32 {
    return 4;
}

// this is also legal
fn unfair_dice_roll() -> i32 {
    10 
}
```

<h3 align="center">Namespaces</h3>

* `::`
    * indicates the hierachy of crates, modules and symbols within a **namespace** *(similar to C++)*

        > In the below example...
        > * `std` is a ***CRATE*** *(a library)*
        > * `cmp` is a ***MODULE*** *(a source file)*
        > * `min` is a ***FUNCTION***

```rust
// this assigns i32 value of 3 to the variable least
let least = std::cmp::min(3,8); 
```

* `use`
    * brings names from other namespaces *into scope* in your program

```rust
use std::cmp::min;

// this assigns i32 value of 1 to the variable least
let least = min(7,1); 
```

* `{}` 
    * curly braces in the context of namespaces are called **globs**, and allow us to import multiple functions from other namespaces easily

```rust
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

```rust
// this brings min, max and many other symbols from the std::cmp namespace into scope
use std::cmp::*;
```

<h3 align="center"><a href="https://doc.rust-lang.org/book/ch19-06-macros.html">Macros</a></h3>

<h3 align="center"><a href="https://runrust.miraheze.org/wiki/Underscore">Underscore</a></h3>

* `_` is a special character that indicates to Rust to **throw that value away** *(and acts as a catch-all pattern in match constructs)*.

```rust
// calls the function but throws away its result that is returned to _
let _ = get_thing(); 

// compiler will NOT warn about an unused variable if it is prefixed with an _
let _x = 42; 

// throw away the left side of the destructured tuple and only return the right
let (_, right) = slice.split_at(middle); 
```

<h3 align="center">Destructuring</h3>

Effectively the same concept as in *Typescript*, where we **extract values from data structures** *(arrays, tuples, vectors, structs)*.

```rust
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

---

<h3 align="center">Blocks && Scope</h3>

Generally, **scope** is defined by a **block of code**.

* Fundamentally, all blocks are expressions, and all expressions evaluate to a value called the **tail** *(indicated by omission of semicolon)*, similar to Haskell.
* Blocks can have multiple initialization and assignment statements.
* Blocks exist anywhere in Rust there is code enclosed by `{}` curly braces *(functions, conditional and match statements, loops, structs)*.

```rust
// here, y + z is the tail, which evaluates to the int32 value of 21, assigned to x
let x = {
    let y = 1;
    let z = 20;
    y + z 
};
```

---

<h3 align="center"><a href="https://doc.rust-lang.org/rust-by-example/scope/lifetime.html">Lifetimes</a></h3>

***Variables*** and ***References*** *(including in function arguments)* have a fixed lifetime determined by the **scope** of curly braces they are within.

> Also see:
> 
> * [Lifetimes in Rust for beginners](https://anooppoommen.medium.com/lifetimes-in-rust-7f2331be998b)
> * [Understanding Lifetimes in Rust](https://blog.logrocket.com/understanding-lifetimes-in-rust/)
> * [Lifetime elision](https://doc.rust-lang.org/nomicon/lifetime-elision.html)

---

<h3 align="center"><a href="https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html">Ownership</a></h3>

Before we talk about Rust's ownership, let's briefly look into how programming languages interface with computer memory.

> Ownership is Rust's most unique feature, and it enables Rust to make memory safety guarantees without needing a garbage collector. 
> 
> *~ The Rust Programming Language* book

#### The Stack and the Heap

Both the Stack and the Heap are parts of *computer memory* that are available for your program to use at **runtime**, but they are structured in different ways.

##### Stack 🥞

* **Last in, first out**
    * Stores values <u>in the order it gets them</u> and removes them in the <u>opposite order</u> *(like a stack of pancakes)*.
    * Adding data = **Pushing onto the stack**
    * Removing data = **Popping off the stack**
* **BLAZINGLY FAST**
    * The stack only ever has to refer to one place for its data *storage* and *retrieval*, the top of the stack.
    * All data on the stack takes up a *known, fixed-size* at compile time.

##### Heap 🚮

* **Less organised**
    * Stores values in the heap at **different locations** (memory addresses), based on the <u>amount of space required to store said value</u> *(like a waiter at a restaraunt)*.
    * Returns a *pointer*, which <u>stores the memory address</u> of said value.
    * Adding data to an empty spot on the heap of sufficient size, pointer returned = **Allocating on the heap**
* **LESS FAST**
    * We have to **follow a pointer** to *retrieve* actual data stored at the given memory address.
    * Data on the heap has an *unknown size at compile time*, or *a size that might change*.
    * Allocating a large amount of space on the heap takes a *long time*.  
  
> Keeping track of what parts of code are using what data on the heap, minimizing the amount of duplicate data on the heap, and cleaning up unused data on the heap so you don't run out of space are all problems that ownership addresses.
> 
> *~ The Rust Programming Language* book

#### Ownership Rules

1. Each value in Rust has a variable that's called an *owner*.
2. There can only be one owner at a time.
3. When the owner goes out of scope, the value will be dropped.

To begin with, here's a simple example of <u>scope</u>.

> *Scope*:   
> Range within a program for which an item is valid.

```rust
// s is not valid here, it has not been declared yet
{
    // s is valid from this point forward till the end of the scope (curly braces), note that s is a string literal and is stack stored
    let s:&str = "hello";
}
// the scope is now over, and s is no longer valid
```

:. Rust **ownership** hinges on the concept that it *automatically returns memory* once the <u>variable that owns said piece of memory goes out of scope</u>.

> See this concept in application below.

```rust
// s2 is not valid here, it has not been declared yet
{
    // s2 is valid from this point forward till the end of the scope (curly braces), note that s2 is a String, which is heap-allocated, and thus has a dynamic length and value
    s2:String = "hello";
}
// this scope is now over, and s2 is no longer valid, Rust returns the space s2 took up in heap memory to the computer
```

### HEAP: Ways that Variables and Data interact

#### Move

TLDR:

* To prevent double free errors, Rust automatically `moves` the **owner** of the value to be the variable that was <u>most recently copied</u>. The previous owner is now invalid.
* This adheres to Rust's **ownership rule number 2**.
  
> For an awesome, diagrammatic explanation on double free errors and Rust's handling of move, refer to *The Rust Programming Language* book `pg 92-94` on **Ways that variables and data interact: Move**.

#### Clone

To **deeply copy** *heap data* of a String *(not just stack data)*, we use the `clone` method.
* This code may be expensive as compared to moving the variable, which Rust does automatically (as covered above).

```rust
// note that "hello" is a string literal, the from method converts it to a String that is mutable
let s1 = String::from("hello");

// this creates a deep copy of the String s1, creating a complete copy of s1's attributes in s2 and taking up another place in the heap to store its value
let s2 = s1.clone();

println!("s1 = {}", "s2= {}", s1, s2);
```

### STACK: Ways that Variables and Data interact

#### Copy

As a caveat, creating deep copies does exist on the *stack* through the `Copy` trait, which Rust does *automatically*.
* This is possible since copies on the *stack* are **quick to make**, so there is no reason to prevent copies of variables from staying valid after creation.

```rust
let x = 5;

// creates a deep copy of x and assigns it to the variable y, x is still valid!
let y = x;

println!("x = {}, y = {}", x, y);
```

However, this kind of behaviour can become frustrating to work with, especially when we consider how Rust functions interact with their parameters and arguments.

As such, Rust provides us with the **incredibly useful**... 

<h3 align="center"><a href="C++/pointers-references.md">References</a></h3>

> Forget everything you think you know about references. We finna learn about them from the ground up today.

References allow us to **refer to a value** <u>without</u> taking ownership of it.
* The ampersand character (`&`) indicates that the given variable is a **reference**.

```rust
fn main() {
    // String allocted on the heap, of dynamic length, created from the string literal "hello"
    let s1 = String::from("hello"); 

    // pass the heap-allocated String s1 by reference to the function calculate_length(), to prevent the string s1 from being moved after the function runs
    let len = calculate_length(&s1);

    println!("The length of '{}' is {}", s1, le);
}

// this function recieves an argument of type reference to a String data type
// s goes out of scope here, but since it does not have ownership of what it refers to, nothing happens
fn calculate_length(s:&String) -> usize {
    s.len() 
}
```

> Note that similar to variables, Rust **references** are *immutable by default*.

<h3 align="center"><a href="https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html">Borrowing</a></h3>

> We call having references as function parameters *borrowing*. As in real life, if a person owns something, you can borrow it from them. When you're done, you have to give it back.  
>  
> *~ The Rust Programming Language* book

---

<h3 align="center"><a href="https://doc.rust-lang.org/book/ch15-00-smart-pointers.html">Smart pointers</a></h3>

---

<h3 align="center">Object-oriented programming 🏗️</h3>

Rust **does not** have 'classes' or 'objects' *(in the conventional sense)*, and instead has [structs](#structs) and [traits](#traits).

> Also see:
> 
> * On [complete rundown of Rust's implementation of OOP](https://stevedonovan.github.io/rust-gentle-intro/object-orientation.html)
> * Another shorter article on [Rust's version of OOP](https://blog.devgenius.io/object-oriented-programming-in-rust-691baf4d2996)

<h3 align="center"><a href="https://doc.rust-lang.org/rust-by-example/custom_types/structs.html">Structs 🏛️</a></h3>

Structs are somewhat similar to objects in *Typescript*, often referred to as **Types** in Rust *(and are Rust's provision for OOP patterns)*.

* declared with the `struct` keyword
    * intialized using **struct literals**
* user-defined **values** simply added to the struct
* user-defined **methods** and **traits** implemented with the [`impl`](https://doc.rust-lang.org/std/keyword.impl.html) keyword *(always used for a Type)*
    * `self` references the given Type

```rust
// struct declaration
struct Vec2 { 
    x:f64,
    y:f64,
}

// a struct lieral
let v1 = Vec2 { x:1.0, y:3.0 }; 

// another struct literal, peep that the order does not matter
let v2 = Vec2 { y:2.0, x:4.0 };

// implementing a method on a struct 
impl Vec2 { 
    fn is_strictly_positive(self) -> bool {
        self.value > 0
    }
}

println!("{}", v1.is_strictly_positive());
```

> Also see:
> 
> * On [struct update syntax](https://users.rust-lang.org/t/the-struct-update-syntax/16519)
> * On [structs as a whole](https://doc.rust-lang.org/book/ch05-01-defining-structs.html)

<h3 align="center">Traits 🧒</h3>

Traits are similar to interfaces in *Java*, allowing us to **declare** attributes and methods a type must have without defining them *(and allowing for OOP inheritance in Rust)*.

* declared with the `trait` keyword
* implement traits on types with the `impl` keyword *(`impl`, `for` syntax)*

```rust
// previously declared struct
struct Number { 
    odd: bool;
    value: i32;
}

// all this does is specify that the type must have a function called isStrictlyNegative that takes self as an argument and returns a boolean
trait Signed {
    fn isStrictlyNegative(self) -> bool; 
}

// implementing a trait for a type
impl Signed for Number { 
    fn isStrictlyNegative(self) -> (bool) {
        self.value < 0
    }
}

// main function
// this prints out true to the console
fn main() { 
    let n = Number { odd:false, value:-44 };
    println!("{}", n.isStrictlyNegative());
}
```  
  
<h4 align="center"><a href="https://github.com/Ixrec/rust-orphan-rules">Rust's Orphan Rules</a></h4>
<p align="center"
   <ul>
      <li>Can implement one of your traits on foreign and primitive types <i>(like i32)</i> ✅</li>
      <li>Can implement other Rust-defined traits on one of our types ✅</li>
      <li>Cannot implement a foreign trait on a foreign type ❎</li>
   </ul>
</p>

> Also see:
> 
> * On [traits](https://doc.rust-lang.org/book/ch10-02-traits.html)
> * On [marker traits](https://doc.rust-lang.org/std/marker/index.html)
> * On [derive](https://doc.rust-lang.org/rust-by-example/trait/derive.html)

---

<h3 align="center"><a href="https://doc.rust-lang.org/book/ch06-01-defining-an-enum.html">Enums</a></h3>

<h3 align="center"><a href="https://doc.rust-lang.org/rust-by-example/std/option.html">Option</a></h3>

<h3 align="center"><a href="https://doc.rust-lang.org/std/result/">Result</a></h3>

<h3 align="center"><a href="https://doc.rust-lang.org/book/ch18-00-patterns.html">Pattern matching</a></h3>

> Also see:
> > 
> * On [pattern syntax](https://doc.rust-lang.org/book/ch18-03-pattern-syntax.html)

<h3 align="center"><a href="https://doc.rust-lang.org/rust-by-example/generics.html">Generics</a></h3>

<h3 align="center"><a href="https://doc.rust-lang.org/cargo/">Package management</a></h3>

<h3 align="center"><a href="https://doc.rust-lang.org/book/ch09-00-error-handling.html">Error Handling</a></h3>

<h3 align="center"><a href="https://doc.rust-lang.org/book/ch16-00-concurrency.html">Concurrency 👥</a></h3>

---

### Resources

* [Rust in 30 minutes](https://fasterthanli.me/articles/a-half-hour-to-learn-rust)
* [Learn Rust in Y minutes](https://learnxinyminutes.com/docs/rust/)
* [Rust Lang Book](https://doc.rust-lang.org/stable/book/)
* [Rust by example](https://doc.rust-lang.org/stable/rust-by-example/)
* [Rust Crash Course](https://www.youtube.com/watch?v=zF34dRivLOw)
* [Rust lang playground](https://play.rust-lang.org/?version=stable&mode=debug&edition=2021)
