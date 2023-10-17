# Dart programming language

![](https://dart.dev/assets/shared/dart-logo-for-shares.png?2)

Dart is a single-threaded (client-optimized) language used to develop fast apps on any platform.

## Quick start

Here are some facts about Dart.

* Semi-colon language
* Object-oriented language
* C-style syntax
* Transpiles to Javascript
* Used in Fluttr, Google's cross-platform mobile application framework
* Print statement allows for formatted strings with `${variable}` operator
* Comment with `//`

```Dart
var numWatermelon = 10;
print("You have ${numWatermelon} watermelons.");
// this is a comment
```

## Compile time vs Runtime

Before we learn about variable declaration in Dart, we need to be aware of the [distinction](https://stackoverflow.com/questions/846103/runtime-vs-compile-time) between **Compile time** and **Runtime**.

### Compile time ⚙️

* Period when program has been written, and is compiling *(using `Dart Native` / rustc` / `g++` / `gcc` / `clangd`)*.
* Syntax errors, typechecking errors, compiler crashes can occur at this stage.
* If program succesfully compiled withput error messages, **assembly code** / **object code** / **executable file** created and can be run.

### Runtime 🏃

* Period when **executable file** is run *(after compilation)*.
* Logical errors, Memory errors, File path and URL errors can occur at this stage *(ie. whatever errors that were not caught by the compiler)*.
* If program succesfully runs, it will output whatever the programmer intended for initially.

## Variable declaration

1. `var`

* Variables are declared before use with the `var` keyword
* ❗Data type of `var` variables decided after variable assignment
* ❗Data type of `var` variables are immutable after variable assignment
* `dynamic` declared variables can change data type even after initial variable assignment
* Default value of variables is `Null`

```Dart
var name = "Let's get Rusty"
```

The `final` and `const` keywords are used to declare constants (a type of variable whose value is immutable), whose value cannot be modified after constant assignment.

2. `final`

* `final` variable initialised at **run-time** when executable file is run, can only be assigned ONCE
* Can be defined in classes and functions
* In Fluttr, when state is updated, everything in build method will be reinitialized
* ❗Use `final` when you **don't know** what a variable's value will be at compile-time *(eg. retrieve data from an API, which occurs when running code at run-time)*

3. `const`

* `const` variable initialised at **compile-time**, already been assigned when at run-time
* Object's entire deep state and all component parts determined entriely at **compile-time**, object is frozen and completely immutable
* Does not have access to any values or variables calculated at run-time
* Can be defined in functions
* In Fluttr, everything in the build method will not be reinitialized again when stats is updated
* ❗Use `const` when you **know** a variable's value at compile-time already, and can assign it definitively without need for reassignment

> Differntiating const and final [here](https://itnext.io/difference-between-const-and-final-in-dart-78c129d0c573)

## Data types
* `int`: Integer
* `double`: Long float
* `String`: Sequence of UTF-16 code units *(string)*
* `bool`: Boolean
* Lists: ordered group of objects *(array)*
    * **Static** Fixed Length List: length cannot be changed at run time, immutable, can be implemented with `IList()`
        ```Dart
        var staticList = List(INITIAL_SIZE);
        ```
    * **Dynamic** Growable List: length can change at run-time using `List.add()`, mutable
        ```Dart
        var dynamicList = List();
        ```
* Map: dynamic collection of key-value pairs *(dictionary)*, and length can be changed at runtime, mutable
    * Key-value pairs assigned and retreived similar to Python
    * Declared using **map literals**
    ```Dart
    var details = {"Username": "cheers", "Password": "Password123"}
    ```
    * Declared using **map constructor** `Map()`
    ```Dart
    var details = new Map()
    details["Username"] = "cheers"
    details["Password"] = "Password123"
    ```
* `dynamic`: Dynamic type assigned to variable which is not explicitely typed, can be used as a type annotation

## Operators

## Arithmetic operators

* `+`: Addition
* `-`: Subtraction
* `-expr`: Unary minus
* `*`: Multiply
* `/`: Divide
* `~/`: Floor divide 
* `%`: Modulo
* `++`: Increment by 1 and reassign
* `--`: Decrement by 1 and reassign

## Equality and relational operators

* `>`: greater than
* `<`: lesser than
* `>=`: greater than or equal to
* `<=`: lesser than or equal to
* `==`: partial equality
* `!=`: not equal

## Type testing operators

* `is`: True if object has specified type
* `!is`: False if object has specified type

## Assignment operator

* `=`: simple assignment
* `??=`: assign value only if variable is `Null`
* `+=`: increment by right operand and reassign
* `-=`: decrement by right operand and reassign
* `*=`: multiply by right operand and reassign
* `\=`: divide by right operand and reassign

## Conditional statements

* `if`
* `else if`
* `else`
* `switch`, `case` and `default` statements: require the `break` statement

```Dart
if (boolean_expression0) {

} else if (boolean_expression1) {

} else {

}
```

```Dart
switch (variable_expression) {
    case constant_expression1: {

    }
    break;
    case constant_expression2: {

    }
    break;

    default: {

    }
    break;
}
```

## Loops

* `for` loop
* `while` loop

```Dart
for (var i=0; i<3; i++){
    print("Loop:${i}");
}
```

```Dart
var num = 3;

while (num >= 1){
    print("New count:${num}");
    num--;
}
```

## Functions

* Data type of returned value declared in function declaration *(eg. `void`)*

### Optional parameters

* Used when arguments need not be compulsorily passed for a function’s execution
* Optional parameters have a question mark appended to their names
* Optional parameter are set as the last argument in a function

#### Optional positional

```Dart
void functionName(param1, [optionalParam1, optionalParam2]) {
    // function body
}
```

#### Optional named

```Dart
void functionName(a, {optionalParam1, optionalParam2}) {
    // function body
}
```

#### Optional Parameters with Default Values

```Dart
void functionName(param1, {param2=default_value}) {
    // function body
}
```

#### [Lambda functions](https://www.educative.io/answers/how-to-use-lambda-functions-in-dart-programming)

* Single-line anonymous functions that usually complete simple tasks
* `[returnType]function_name(parameters)=>{return expression}`
* `[returnType]function_name(parameters){return expression}`

```Dart
int test() => 123;
String test2(){return "Rut";};
```

## OOP



## Null-safety



Reference was taken from...
* [this article](https://medium.com/nerd-for-tech/dart-for-dummies-learn-dart-in-30-minutes-e212328b81f0)
* [dart documentation](https://dart.dev/tutorials)
* [learn x in y minutes](https://learnxinyminutes.com/docs/dart/)
