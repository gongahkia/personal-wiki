> *Edit on 3/3/2023:* Note that the other 2 files, `main.cpp` and `main2.cpp` were made in late 2022 when I first started learning the language. As such, some information might be outdated and wrong. 

# The C++ programming language

 Welcome to Hell. 😔

![](https://media.tenor.com/Kp3Q2HiVUFQAAAAM/cat-on-fire-sitting.gif)

> While C++ shares many of its idiosyncrasies with C, given it is a **superset** of the C programming language, it retains many archaic features that make me ***sad***.

* This introduction assumes a rudimentary understanding of how programming languages are *structured in general*, and a basic knowledge of the [C programming language](https://github.com/gongahkia/study-notes).

----------

### "It's like C ah bro" *(the similarities)*

![](https://i.ytimg.com/vi/ZpviFa4rU6w/maxresdefault.jpg)

> C++ is a static, strongly-typed programming language.  
> ~2SG Ong 🫡

#### General similarities

* **Variables** and **Constants**
* **Implicit** and **Explicit** type conversion
* Comments `//`
* Including stuff `#include <iostream>`
* **Conditional** statements `if`/`else if`/`else`
* **Switch** statements `switch()`, `case`, `default`
* `for` *(`for (int i=0; i < 10; i++)`)*, `for each` *`(for (std::string student: students)`)*, `while` and `do while` **loops**
* `break` and `continue` statements
* **functions** declared with their *return types* (`void`/`int`/`float`/`double`/`char`/`string`/`bool`/`[]` *array*), the *data types* of their **arguments** and the `return` keyword
* **local variables** *(declared within a function and indicated by `{}` curly braces)*, **global variables** *(declared outside all functions)* and the `::` **scope resolution** operator
* **Arrays** *(list in Python)* `[]` and accessing array elements by *index*
* `sizeof()` operator returns the **size of a given data value** in *bytes*
* `fill()` function **fills a range of elements with a specified value**, and takes in 3 arguments, the *array name*, *array name + number of elements to be filled* and the *value* to be filled
* **Memory Addresses** via the `&` *(address-of operator)*, **[Pointers](https://github.com/gongahkia/study-notes)** via the `*` *(dereference operator)* and that ***Arrays are already memory addresses***, **Dynamic memory management** via the `new` and `delete` keyword
* `struct` keyword creates a **structure** *(data structure that groups different variable names of different data types together)*, individual values referenced using `.` **dot notation**, and struct objects are created off the struct *data type*
* `enum` keyword to implement **enumerations** 

#### Data types

* integer `int`
* float `float`
* double `double`
* boolean `bool`
* single character `char`
* string `string` *(strings are provided from the **standard namespace**, and must be declared with `std::string`)*

#### Arithmetic operators

* equality `=` and inequality `!=`
* larger than `>` and smaller than `<`
* addition `+`
* subtraction `-`
* multiplication `*`
* division `/`
* modulo `%`

#### Math related functions

From the *Standard Namespace*:

* **maximum** of 2 values `std::max()`
* **minimum** of 2 values `std::min()`

From *[Cmath Header File](https://cplusplus.com/reference/cmath/)* (`#include <cmath>`):

* raises first number **to the power** of the second number `pow()`
* **squarerooted** value of a number `sqrt()`
* **absolute** value of a number `abs()`
* rounds a value **up or down** depending on its decimal `round()`
* **always** rounds a value **up** `ceil()`
* **always** rounds a value **down** `floor()`

#### Logical operators

* and `&&`
* or `||`
* not `!`

#### String methods

* `.length()` returns an *integer value* for the **length** of a string
* `.empty()` returns a *boolean value* on whether a string is **empty**
* `.clear()` **clears** the string value stored in a variable
* `.append()` **appends** a string *(taken in as an argument)* to the other chosen string
* `.at()` returns a *character* from the string at the **specified index** *(taken in as an argument)*
* `.insert()` **inserts** a *character* in the string at the **specified index** *(the index and character are taken as arguments)*
* `.find()` **finds** and returns the *index* at which the **specified character** is located at
* `.erase()` **erases** a slice of a string from the **specified start index** to the **end index** *(taken in as arguments)*

Additional string methods can be found [here](https://cplusplus.com/reference/string/string/?kw=string+).

#### Object oriented programming

* `class` keyword to declare a **class**
* `public` (accesible outside the class) and `private` (not accessible outside the class) keywords to differentiate between **public** and **private** *attributes* and *methods*
* **Constructors** are called as a *method* within the class object, using the ***same name*** as the class object *(eg. `class Student` class object's constructor method is called with the `Student()` method)* 
* `this` keyword and the `->` operator used to reference and assign **instance object attributes** and **variables**
* **Overloaded constructors** are created in the same way as normal overloaded functions, by including *every desired variation* of the constructor method in the class object

```C++
class Student{

    public:
        std::string name;
        int age;
        double gpa;
    // these public attributes are accesible outside the class
    
    Student(std::string name, int age, double gpa){
        this -> name = name;
        this -> age = age;
        this -> gpa = gpa;
    }
    // the constructor method, which is called automatically when the Student object is instantiated
    // this has the same effect as using self. in Python

}
```

* Object **attributes** and **methods** can be accessed and called via `.` *dot notation*
* Access private attributes via **getters** *(public method that makes a private attribute readable)* and **setters** *(a public method that makes a private attribute writeable)*
* `:` colon and `public` keyword used to indicate **inheritance** for the relationship between the *child to parent* class 

```C++
class faeces{
    public:
        bool alive = true;

    void sound(){
        std::cout << "Plop plop plop plop\n";
    }
}
// faeces is the parent class

class diarrhoea: public faeces{
    void waterfall(){
        std::cout << "Pshhhhhhhhhhhhhh\n";
    }
}
// diarrhoea is the child class, and will retain all its parent's methods as well as its own, such as waterfall

class constipation: public faeces{
    void sound(){
        std::cout << "You're gonna be here for a while buddy\n";
    }
}
// additionally, constipation (child of the faeces parent class), can redefine its sound() method, so the sound() method when called on constipataion will print "You're gonna be here for a while buddy\n" to the console instead of "Plop plop plop plop\n"
```

----------

### "Uh but C doesn't have" *(the differences)*

![](https://i.kym-cdn.com/photos/images/newsfeed/002/322/167/b9f.gif)

#### Print statements

As opposed to the reasonable `printf()` of C, C++ implements the lovely `cout <<` and `<< endl`.

```C++
std::cout << "Hello World!" << std::endl;
```

#### Namespaces

Namespaces prevent name conflicts between multiple entities that share the same name in a project, as long as the ***namespaces are different*** *(similar to local and global scope)*.

* `namespace` keyword declares a **new namespace**, and variables created within the namespace can later be referenced by prefixing the variable with the **namespace** and `::` **double colons**

```C++
#include <iostream>

namespace first{
    int x = 1;
}

int main() {

    int x = 0;

    std::cout << x << std::endl; // this prints out the integer 0 to the console

    std::cout << first::x << std::endl; // this prints out the integer 1 from the namespace first to the console

    return 0;
}
```

* `using namespace` keyword can be used as an alternative to the `::` double colons

```C++
#include <iostream>

namespace second{
    int x = 2;
}

int main() {

    using namespace second;

    std::cout << x << std::endl; // this prints out the integer 2 from the namespace second to the console

    return 0;
}
```

This is also why we oftentimes add `using namespace std` to our C++ programs to include the **standard namespace**, as it includes hundreds of different entities *(eg. cout, string, data)*.

#### Type Def && Using

Type definitions create an additional name for an *existing data type*, increasing **readability** and reducing typos.

* `typedef` keyword prefixes the *data type* of the new type we want to create, and the *name* of said type is included at the end

```C++
#include <iostream>
#include <vector>

typedef std::vector<std::pair<std::string, int>> pairlist_t;
typedef std::string text_t;

int main() {

    pairlist_t pairlist; // this allows us to easily reference an existing data type that we have renamed to pairlist_t

    text_t wisdom = "aight bet"; // this allows us to reference the std::string data type via an easier name, text_t

    return 0;
}
```

Note that `typedef` has largely been **replaced** by the `using` keyword *(as it is more suitable for templates)*.

* `using` keyword is paired with the `=` equal operator to assign *existing data types* to new, easier to reference *names*

The above example would now look like this:

```C++
#include <iostream>
#include <vector>

using pairlist_t = std::vector<std::pair<std::string, int>>;
using text_t = std::string;

int main() {

    pairlist_t pairlist; 

    text_t wisdom = "aight bet"; 

    return 0;
}
```

#### User input

* `cin` *(character in)* and the `>>` *(extraction operator)* are used to **receive user input**

Note that `std::cin` is part of the standard namespace, and thus `std::` must prefix the character in `cin`.

```C++
std::string name;

std::cout << "NRIC rank and name: " << std::endl;
std::cin >> name; // this reads the value taken from the console input to the string variable name
```

However, `cin` **stops reading input** from the console once it detects a *whitespace character*. In these situations, we use the `getline()` function.

* `getline()` function takes in the *input source* `std::cin`, and the *variable to store user input* as arguments

```C++
std::string dimsum;

std::cout << "Enter your favourite siew mai name: " << std::endl;
std::getline(std::cin, dimsum); // this reads the value taken from the console input to the string variable dimsum, including any whitespace characters
```

#### Overloaded functions


**Overloaded functions** are functions which can take in a **variable number of arguments** of *different data types* based on the use case.

* we declare **multiple** functions under the *same name*, with each being a possible version that we call in the program

```C++
#include <iostream>

void papa_pizza() {
    std::cout << "Here is your pizza\n";
}

void papa_pizza(std::string topping1) {
    std::cout << "Here is your " << topping1 << "pizza\n";
}

void papa_pizza(std::string topping1, std::string topping2) {
    std::cout << "Here is your " << topping1, topping2 << "pizza\n";
}

int main() {

    papa_pizza(); // this will call the first version of the papa_pizza() void function, and print out the basic string
    papa_pizza("hawaian"); // this will call the second version of the papa_pizza() void function
    papa_pizza("chicken supreme"); // this will call the third version of the papa_pizza() void function

    return 0;
}
```

#### Function templates

**Templates** are an alternative to *creating multiple versions of the same function* (when using ***overloaded functions***).

* **declared letter** `typename` replaces any *data type* for **return value** and **arguments taken in** by our overloaded function, and the template function will now take in arguments of ***any data type***
* `template <typename T>` *template definition* must be included before the template is declared 

```C++
#include <iostream>
template <typename T>

T max(T x, T y){
    return (x > y) ? x:y; // using the ternary operator instead of an if else statement
    }

int main() {

    std::cout << max(1.1,2.1) << "\n" // the function template will accept the float/double data type, and return the relevant value
    std::cout << max('a', 'b') << "\n" // the function template will accept the character data type and return the relevant output
    std::cout << max(10, 100) << "\n" // the function template will accept the integer data type, and return the relevant output

    return 0;
}
```

To include multiple arguments of **different data types**, we just have to declare a seperate `typename` for each data type.

* `auto` keyword can be used to automatically deduce the **return value** data type

Here's the above template function implemented for arguments with two data types: 

```C++
#include <iostream>
template <typename T, typename U>

auto max(T x, U y){
    return (x > y) ? x:y
}

int main() {

    std::cout << max(1, 2.1) << "\n" // the function template is now able to receive arguments of 2 different data types

    return 0;
}
```

----------
