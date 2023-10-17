> *Edit on 30 March 2023:* Watch FreeCodeCamp youtube Typescript tutorial speedrun

# `Typescript`

![](https://kirkstechtips.com/content/images/2023/03/typescript.jpg)

Typescript is a **strongly**, **statically typed**, **compiled** programming language. 

As a superset of Javascript, *Typescript code compiles to Javascript code*.

## Quick Start

### Installing and compiling Typescript 

We want to **avoid** installing Typescript globally as far as possible. Instead, we install it as a *local dependency* for each project with the following commands.

```console
mkdir typescript-proj
cd typescript-proj

npm install typescript --save-dev
npx tsc --init
npx tsc
```

* `npm install typescript --save-dev` installs Typescript as a local dependency in your project.
* `npx tsc --init` creates a `tsconfig.json` file.
* `npx tsc` compiles your Typescript file to a Javascript file, creating a `.js` file in the same file directory.

```console
npx tsc -w
```

* `npx tsc -w` puts the Typescript compiler in *watch mode*, indicating to it to recompile your Typescript file whenever a change is made.

### Running Javascript files

```console
node helloworld.js
```

### Calling Web APIs using Typescript

```typescript
async function getData (targetUrl:string) {
    const response = await fetch(targetUrl);
    const data = await response.json();
    console.log(data);
}
```

Put very simply, this is the easiest way to call a Web API using a Typescript [`async`](https://www.atatus.com/blog/introduction-to-async-await-in-typescript/) function.

* `await fetch()` **fetches the given URL**, and waits for the values stored at said URL to be determined prior to carrying on.
* `await response.json()` **reads the data** as a `.json` file format, allowing us to read it as a dictionary, and waits for the value to be returned prior to continuing the program.

However, do note that there are *some issues* with this initial implementation.

* There is no edge-case checking for invalid URLs.

## Stuff unique to Typescript

### Printing to the console && formatted strings

* `console.log()` prints to the console

```typescript
console.log("shit");
```

* `${}` and `` ` `` is used to embed values in formatted strings

```typescript
let age: number = 37;
console.log(`you are on my mind, mr ${number}`);
```

### Comments

* `//` **single-line** comment
* `/* */` **multi-line** comment

```typescript
// this is a single-line comment
/*
    this is a multi-line comment
*/
```

### Variable declaration

* `let` declares variables with a mutable value
* `const` declares variables with an immutable value

### Data Types

Typescript can *infer* **data types**. However, we normally statically define data types anyway for easier type checking on our part.

* `string`
    * *"this is a string"*
* `number` 
    * covers **int** (*12*), **float** (*1.2345*), **double** (*2.0293847571923*) data types
* `boolean` 
    * *true*, *false*
* `any`  
    * special flexible data type that disables type checking for the specified variable
* `null` / `undefined`
    * absent or uninitialized values
* `never`
    * when all possible options in a Union have been eliminated

We can check the data type of a variable/value/object with `typeof` operator.

### Unions

Typescript unions allow us to declare variables with **multiple possible data types** *(and account for each possibility)*.

* `|` pipe character used to declare the Union data type.

```typescript
const numOfDoors: string | string[]; // declaring a Union data type, where numOfDoors can either hold a string value, or an array of strings

onReceiveData(data: string | number) {
    console.log(typeof data);
    if (typeof data === "number") {
        return data + 12;
    } else if (typeof data === "string") {
        return data.toUpperCase();
    }
}
// Union data type being applied in a function, where the return value differs based on the argument received
```

### [Literal types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#literal-types)

### Data Structures

* Arrays
    * Typescript arrays are initialized with `[]` square brackets.
    * Statically define the data type of the array *(like in C++)*.

```typescript
const myArray: number[] = [1,2,3,4,5];
const myArray2: Array<number> = [2.13,4.50,1] 
// both of the above achieve the same thing

const anyArray: Array<any> = [12,"thirteen",false] // initialize an array with multiple data types using the 'any' type
```

* Tuples
    * Arrays with **fixed number of elements**.
    * Element values and order can be reassigned *(as long as there are no type errors)*.
    * Statically define the data type of the tuple *(like in C++)*.

```typescript
let myTuple: [number, string] = [2000,"ok bet"]

myTuple[0] = 14;
myTuple[1] = "steve"
// value reassignment is allowed in tuples
```

* Enums
    * `enum` keyword declares an **Enum data structure**.
    * Allows us to assign **strings** to **numeric values** *(index value of each element in the Enum)*.
    * Enums can start from a non-zero value, so **order of elements** matter.

```typescript
enum foods {"bacon","tomato","lettuce"}

console.log(foods[1]) // this will print out "tomato" to the console
console.log(foods.lettuce) // this will print out 2 to the console
console.log(foods["bacon"]) // this will print out 0 to the console
```

On more [data structures](https://levelup.gitconnected.com/typescript-data-structures-in-10-minutes-4cd785272ad0) in Typescript.  
On more [data structures](https://www.freecodecamp.org/news/data-structures-in-javascript-with-examples/) in Javascript.

### Conditional flow

Conditional flow and equality checks work as expected in Typescript.

* `if`
* `else if`
* `else`
* `switch`, `case` 
* `break`
* `continue`  

### Equality

* `==` checks for equality in **value**
* `!=` checks for inequality in **value**
* `===` checks for strict equality in **value** and **type**
* `!==` checks for strict inequality in **value** and **type**

### Loops

* `for` 
* `for`, `in`
* `while`
* `do`, `while`

### Functions

Functions can be called one of two ways.

* Remember to statically define the data types of **function arguments** and **return values**.
* Typescript has `void` functions as well.

1. `function`
    * the option we are more familiar with, used in C++
2. `=>`
    * indicates the **function body**

```typescript
function subtract(x: number, y: number): number {
    return x - y;
}
// here, I specified the argument and return value data types

const add = (x: number, y: number) => x + y;
// here, I specify the argument data types only, and the return value's data type is inferred

function voidLog(): void {
    console.log("shit");
}
// a void function, a function with no return value

subtract(100, 10);
add(1000, 2.1238);
voidLog();
```

Typescript supports **overloaded functions** too.

### Types 

Functions somewhat similarly to types in *C++*.

> Note that `type` is initialized **with** an `=` equals sign.

* Allows us to create custom data types.
* Call `type` as the **data type** when constructing a *variable* from it.
* Variable must possess the values and methods specified in the type.

```typescript
type calculator = {
    names: string[],
    hobbies: string[],
    output: (test:booelan) => string[]; // methods can be included in types
}

let instrument:calculator = {
    names: ["ah","shit","here","we","go"],
    hobbies: ["having fun","talk cock sing song"],
    output: (test) => {
        return test + this.hobbies;
    }
}
// creating a new variable off the type
```

### Interface

Functions somewhat similarly to interfaces in *Java*.

> Note that `interface` is initialized **without** an `=` equals sign.

* Allows us to create custom data types and templates that can be extended.
* Call `interface` as the **data type** when constructing a *variable* from it.
* `extends` keyword used to copy attributes and methods from an interface to another interface.
* `implements` keyword used to implement an interface's attributes and methods on a class.
* Variable must possess the values and methods specified in the interface.

```typescript
interface exampleInterface {
    x: number,
    y: number,
    z?: string, // optional properties are suffixed by a ?
    output: (test:boolean) => string[]; // methods can be included in interfaces
}

const obj1: exampleInterface {
    x: 123;
    y: 456;
    output: (test) => {
        if (test) {
            return ["ok", "ok", "okok", "kanye"];
        } else {
            return ["aight", "poopy pants"];
        }
    }
}

const obj2; exampleInterface {
    x: 789;
    y: 101112;
    z: "ah shit";
    output: (test) => {
        return [test.toString() + "okie", "ok"];
    }
}
```

* For the most part, Types and Interfaces can be used interchangably. However, it should be noted that while an interface is **always extendable**, a type **cannot be re-opened** to add more properties after declaration.
* `&` operator also allow for [**intersection types**](https://www.typescriptlang.org/docs/handbook/2/objects.html#intersection-types) that combine existing object types.

On the [difference between Types and Interfaces in Typescript](https://medium.com/@martin_hotell/interface-vs-type-alias-in-typescript-2-7-2a8f1777af4c).

### Object-oriented Programming

Typescript handles [OOP](https://www.typescriptlang.org/docs/handbook/2/objects.html) somewhat similarly to Java.

### Objects

Typescript objects can store variables/values as object **attributes**, and functions as object **methods**.

* Typescript objects are initalized with `{}` curly braces.
* Statically define the data type of each of the object's attributes *(like in C++)*.
* We can **define** and **call methods** on Typescript objects as well.
* Object attributes and methods are accessed via `.` dot notation.
* `this.` refers to the object's attributes or methods *(similar to `self.` in Python)*.

```typescript
let data1: = {name: "rovio", numberOfPCs: 12, hobbies: ["bobsledding", "skydiving", "ok"]}
// allowing data types of object attributes to be inferred

let data2: {name: string, age: number, hobbies: string[]} = {name: "Jonathan", age: 30, hobbies: ["running", "swimming", "coding"]}
// statically defining data types of object attributes

let data3: {ages: number[], hobbies: string[], output: (test: boolean) => number[]} = {
    ages = [1,2,3,4,5],
    hobbies = ["hiking","travelling","swimming"],
    output: function(test: boolean): number[] {
        return this.ages;
    }
}
// statically defining data types of object attirbutes and methods
```

* `in` operator checks whether a specified property belongs to an object.
* `instanceof` operator checks whether an object is an instance of a class, or whether a value is an instance of another value.

### Classes

Typescript classes allow us to easily instantiate multiple objects *(that follow the class' template)* off the same class.

* `class` keyword used to declare a class.
* `constructor()` keyword used to define the constructor method for the class.

```typescript
class Vehicle {
    engine: string;
    doors: number[];
    color: string;
    start: () => void;

    constructor(engine:string) {
        this.engine = engine;
        this.start = function():void {
            console.log(`This mf ${this.engine} boutta whoop someone's ass`);
        }
    }
}
```

On the [difference between Classes and Interfaces in Typescript](https://ultimatecourses.com/blog/classes-vs-interfaces-in-typescript#Using_TypeScript_class_vs_using_Typescript_interface).

### Universal across Interfaces, Classes and Objects

* `readonly` modifier prevents an attribute from being written to during type-checking.

### Inheritance

* `extends` keyword allows for child class to inherit attributes and methods of parent class.
* `super()` keyword must be called in child **constructor** before referencing any of the parent's attributes.
* `super.` syntax must be called in child class to reference any of the parent's methods.

```typescript
class Base {
    k:number = 4;
}
// parent class

class Derived extends Base {
    constructor() {
        super();
        console.log(this.k);
    }
}
```

### Scoping

* `public` member can be accessed anywhere.
* `protected` member visible to *subclasses* of the class they are declared in.
* `private` member can be accessed only within the class it is declared in.
* `static` members are class attributes and methods.
* `abstract` members / classes do not have an **implementation** provided, and cannot be directly **instantiated**.

### Destructuring

> *Edit on 4 April 2023*: Return to this topic when I have more experience in Typescript.

[Destructuring](https://basarat.gitbook.io/typescript/future-javascript/destructuring) is a fancy term for *extracting* values from data structures *(eg. objects, nested arrays)*.

Typescript supports the following 2 types of destructuring.

* **Object** destructuring
* **Array** destructuring

```typescript

```

### Casting

[Casting](https://www.w3schools.com/typescript/typescript_casting.php) is the process of overriding and redefining the *data type* of a **variable**.

Typescript supports casting one of two ways.

* `as` keyword
* `<>` operators

```typescript
let x:unknown = "ah shit";
console.log((x as string).length);

let y:unknown = "shitass punk";
console.log((<string>y).length);
// both of the above 2 examples are valid uses of casting 
```

> ***Important note***: Casting doesn't actually change the data type of the *value* stored within the variable, it only changes the data type of the **variable** itself.

Typescript also suports **Force casting**, which forcibly recasts a variable's data type.

```typescript
let x = "hello";
console.log(((x as unknown) as number).length); // --- note that since x isn't actually a number, this will return undefined
```

### [Generics](https://www.typescriptlang.org/docs/handbook/2/generics.html)
