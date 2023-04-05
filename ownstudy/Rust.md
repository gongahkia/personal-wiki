> *Edit on 3/3/2023:* Because I am bad and like to rush things, I rushed into learning Rust lang without first mastering C and C++. I have rethought my decision, and will return to learn from the Rust lang book when I've levelled up IRL in other languages as well.

# The Rust programming language 🦀

![](https://preview.redd.it/oifjvyix1us41.jpg?auto=webp&s=169dd56f3eb24adda52abc83cc98bce5765c8538)

[Rust](https://fasterthanli.me/articles/a-half-hour-to-learn-rust) is a **strong** and **statically-typed** compiled programming language, heralded for its *memory safety* and *clear error messages*.

----------

### Printing to the console

Similar to C and C++, *all code to be executed* must be placed within the `main()` function.

* `println!()` **prints** the chosen text to the console

```Rust
fn main() {
    println!("Hello World!");
}
```

Note that the `!` exclamation point character means `println!` is a Rust **Macro** *(which will be elaborated on later)*.

----------

### Creating a Rust project

Rust files are created with the `.rs` extension.

* `rustc` calls the ***Rust compiler*** to compile our rust program, and a **binary executable file** which we can run will be generated
> Note that the binary executable will be created with the same name as your Rust file/ Rust package.

*Below is an example of how to compile a single Rust file to run a small program:*

```bash
rustc main.rs
./main
```

However, larger Rust projects that contain **multiple dependencies** should be initialized with Rust's ***build feature***, `cargo`.

* `cargo new {project name}` **creates** a new Rust package
> All Rust program code is to be written inside the *`main.rs`* file *(which can be found inside the `src` folder)*.

*Below is an example of how to create a new Rust project with the name project1:*

```bash
cargo new project1
```
* `cargo build` automatically **compiles** and **builds** the Rust project for us *(creating the corresponding binary executable file)*
> The binary executable file can be found inside the `/target/debug` folder.

*Below is an example of how to compile a Rust project and run the executable:*

```bash
cargo build
cd target/debug
./project1
```

* `cargo run` **immediately compiles and runs** the program executable *(for quick checks when writing a Rust program)*

```bash
cargo run
```

* `cargo check` **checks** whether your Rust program is able to **compile without error** *(without actually compiling the program)*

```bash
cargo check
```

----------

### Variables && Constants

Variables in Rust are ***immutable by default***.

* `let` declares and creates a **variable**, and we *explicitly* declare the variable's data type with a `:` colon
* `mut` keyword indicates that a variable is **mutable** *(its value can now be changed later in the program)*

```Rust
let x:i32 = 40;
println!("x is: {}", x); // formatted strings in Rust look like this

let mut y:i32 = 45;
y = 100;
println!("y is: {}:, y);
```

* `const` declares and creates a **constant**, whose **value** and **data type** cannot be changed throughout the program

```Rust
const SECONDS_IN_MINUTES:u32 = 60;
println!("{}", SECONDS_IN_MINUTES);
```
----------

### Data types

#### Primitive data types

* Signed integer *(positive or negative)* `i8`, `i16`, `i32`, `i64`, `i128` 
> the number behind `i` indicates the **number of bits** the given integer can take up in memory

* Unsigned integer *(always positive)* `u8`, `u16`, `u32`, `u64`, `u128`
> note that for the same number of bits, an unsigned integer can represent a **larger range of numbers** than a signed integer

* Floats `f32`, `f64` 
> `f32` is **single precision**, `f64` is **double precision**

* Boolean `bool`
> `true` can also be represented by 0, and `false` by 1

* Character `char`
> single characters are surrounded by **single quotation marks** `''`

#### Compound data types

* String
> *add more notes on this later from Rust book*

* Tuple `({data types of values within tuple})`
> **fixed length** sequence of elements *(that can be of different data types)*  
> values in a tuple can be accessed by *index* via `.` **dot notation**

```Rust
let eg_tuple:(i32, bool, char) = (1, true, 'A');
println!("{}", eg_tuple.1); // this would print out true to the console, which has an index of 1
```

* Array `[{data type of values within array};{number of values within array}]`
> **fixed length** sequence of elements *(that are all of the same data type)*  
> values in an array can be accessed by *index* via `[]` **square bracket notation**

```Rust
let eg_array:[i32; 10] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
println!("{}", eg_array[7]); // this would print out the integer 8 to the console, which has an index of 7
```

----------

### User input

**User input** in Rust is handled by the *io module* under the standard namespace, and is imported with `use std::io`.

* `io::stdin()` and `.read_line()` method is called to **accept user input** from the console *(standard input)*, and **store** it within the String variable

```Rust
use std::io;

fn main() {
    println!("Type something: ");
    let mut input = String::new(); // declares and creates a new empty mutable string variable
    io::stdin().read_line(&mut input);
}
```

----------
