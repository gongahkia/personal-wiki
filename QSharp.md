# `Q#`

> add description of Q# and make it succint accurate and expressive

## Comments

```qs
// ----- COMMENT -----

// this is a single-line comment

// there is no default implementation for 
// multi-line comments in Q# but this effectively
// achieves the same effect
```

## Printing

```qs
// ----- PRINTING -----
    // Message() => operation that receives a String argument and prints it to the stdout without including a newline by default
    // there is no built-in Q# implementation for printing to the stdout with a newline automatically included

Message("this does not include a newline");
Message("this has a newline but only because we explicitly specified it\n");
```

## Quickstart

```qs
// ----- QUICKSTART -----
    // semicolon language
    // set =>
    // mutable =>

```

## Types

```qs
// ----- TYPE -----

// --- PRIMITIVE DATATYPES ---
    // Bool => true, false
    // Int => stores a signed 32-bit integer number value
    // BigInt => stores a signed arbitrary-precision integer number value
    // Double => stores a double-precision floating-point number value
    // String => stores a sequence of Unicode characters, declared within "" double quotation marks
    // Unit => special value representing the absence of a value, equivalent to void and null in other languages

// --- QUANTUM DATATYPES ---
    // Qubit => stores a quantum bit value, the foundational basic unit of quantum information
    // Result => stores the result of a quantum measurement
        // Zero
        // One
        // Plus
        // Minus
    // Pauli => stores the value of pauli matrices
        // PauliI
        // PauliX
        // PauliY
        // PauliZ
```

## Operators

```qs
// ----- OPERATOR -----

// --- ARITHMETIC OPERATOR ---

+ // addition
- // subtraction
* // multiplication
/ // divison
% // modulo

// --- COMPARISON OPERATOR ---

== // complete equality check for both value and type for primitive types, partial equality check for value for quantum types
!= // complete inequality check for both value and type for primitive types, partial inequality check for value for quantum types
> // comparison operator
< // comparison operator
>= // comparison operator
<= // comparison operator

// --- LOGICAL OPERATOR ---

&& // logical and
|| // logical or
not // logical not
```

## Control structures

```qs
// ----- CONTROL STRUCTURE -----

// --- CONDITIONALS ---

// IF ELSE IF ELSE
    // note that the result of a conditional construct can be directly assigned to an operation or variable binding as seen below

operation ExampleIfElse(value : Int) : Unit {
    if (value > 0) {
        Message("Value is positive.");
    } else if (value < 0) {
        Message("Value is negative.");
    } else {
        Message("Value is zero.");
    }
}

// MATCH CASE => 
    // provides a degree of basic pattern-matching in Q# similar to Rust, the equivalent of switch case constructs in other languages
    // note that each predicate case condition is ; semicolon-delimited
    // similar to the above conditional construct, the result of a match case construct can be assigned to an operation or variable binding as seen below

operation ExampleMatchCase(result : Result) : Unit {
    match result {
        case Result.Zero => Message("Result is Zero.");
        case Result.One => Message("Result is One.");
        case Result.Plus => Message("Result is Plus.");
        case Result.Minus => Message("Result is Minus.");
    }
}

// --- LOOPS ---

// FOR IN 
    // functions similarly to for in and foreaach loops in other programming languages like Python and PHP
    // .. => dynamically creates an iterable range structure that allows traversal and iteration over each element of the iterable structure

operation ExampleLoop() : Unit {
    mutable sum = 0;
    for (i in 1 .. 5) {
        set sum += i;
    }
    Message($"Sum of numbers from 1 to 5: {sum}");
}

// WHILE 
    // functions similarly to while loops in other programming languages like Python

operation ExampleWhileLoop() : Unit {
    mutable count = 0;
    while (count < 5) {
        Message($"Count: {count}");
        set count += 1;
    }
}
```

## Data structures

```qs
// ----- DATA STRUCTURE -----
    // array =>
    // tuple =>
    // map =>
    // range =>
    // struct =>
    // newtype =>

```

   - **`Array`**: Represents a mutable array of elements of the same type.
   
   - **`Tuple`**: Represents an immutable ordered collection of elements of possibly different types.
   
   - **`Map`**: Represents a key-value mapping where keys and values can be of any type.
   
   - **`Range`**: Represents a range of integer values (`Range Start Step End`).

4. **User-Defined Types:**

   - **Structs and Newtypes**: Allows defining custom data structures using `struct` or `newtype` keywords.

## Operations and Functions

```qs
// ----- OPERATION -----
    // 

// ----- FUNCTION -----
    // 

```

5. **Callable Types:**

   - **Operations**: Represent quantum operations or classical functions.
   
   - **Functions**: Represent classical functions that return a value.

## More on

* [Q# documentation](https://learn.microsoft.com/en-us/azure/quantum/)
* [learn Q# in y minutes](https://learnxinyminutes.com/docs/qsharp/)
