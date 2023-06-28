# `Pointers` in C++

![](https://pbs.twimg.com/media/FV5AgCHWAAEP2X5?format=jpg&name=medium)

* Every variable and value in C++ has a **specific memory address**, which can be represented by a variable in the *pointer* data type, denoted with the `*` aestricks character
* a **variable** of `pointer` data type contain **values** of `memory addresses` of variables within the program

```cpp
 int *p_integer {}; // this refers to a pointer storing the memory address of an Integer data type
```

```cpp
 double *p_fractional {}; // this refers to a pointer storing the memory address of a Double data type
 ```

* note that pointers can **only** store memory addresses of variables they have been statically defined to store (an int* pointer cannot store the memory address of a float/ double/ string/ char/ bool)

Additionally, note that the following are accepted as valid pointer notation:
* `int* p_integer`
* `int *p_integer`

------

### nullptr
C++ allows us to explicitely intialize pointers to the *nullptr* memory address when there is no defined memory address we want to store inside said pointer
```cpp
int *p_integer {nullptr};
```
------

### &
the ampersand `&` operator allows us to access the **exact memory address** of the variable placed behind it
```cpp
int *p_integer = &integer; // this stores the memory address of the Integer variable within the p_integer pointer
```
  
------

### Memory Address Assignment
to **reassign** the memory address of a given variable in C++, we make use of both the `*` aestricks character [deferencing operator] and the `&` ampersand character [operator to obtain memory address from a variable]
```cpp
int *p_int = &integer;
p_int = &integer2; // this reassigns the memory address of the variable integer2 to the pointer p_int
```
* note there is **no need** to *statically redeclare* the `pointer` p_int's data type in the second line and assignment is all that is neccesary since it has been statically declared in the first line

------

### Dereferencing
dereferencing is obtaining the value of a given variable from the memory address of the variable, achieved through the `*` dereferencing operator, which prefixes the pointer variable (that points to the desired variable we want to extract said value from) 
```cpp
int *p_int2 {};
int int_data {56};
p_int2 = &int_data;
int value = *p_int2;
```

------

### Pointer to Char types
```cpp
char *p_char{nullptr};
char character1{'A'};
p_char = &character1;
char value = *p_char;
```
* char pointers can also be initialized with a **string literal**, where the string is taken as an *array of constant characters*

```cpp
char *p_message = "Hello World";
```
* the above example will *point at* the first character of the character array "Hello World", since the data type of said pointer is a **character pointer**

As such:

1. `cout << *p_message << endl` will print out the character 'H'
2. `cout << p_message << endl` will print out the entire string "Hello World"

------

## Computer Memory
Computer memory is composed of the following 5 components:
1. System
2. Stack
3. Heap
4. Data
5. Text

### Stack
* finite memory
* developer **lacks control** of Stack's memory lifetime
* Lifetime is determined by C++'s *SCOPE MECHANISM* (curly braces {} determine the scope of variables and values)

### Heap
* finite memory
* developer has **full control** over Heap's memory allocation and release lifetime
* Lifetime is controlled explicitily through `new` and `delete` operators

 ----------

## TLDR on Pointers 

![](https://i.redd.it/r01sq96mnz691.jpg)

1. `Pointers` are a DATA TYPE, and pointer **Variables** store `memory addresses` as their **values**

2. In the initialization and assignment statement for a `pointer` variable, use the `*` aestricks character to **suffix** the data type that the pointer will be pointing toward
* as seen below in line 2, the *int** pointer data type indicates that the pointer variable *p_number* is storing the memory address of an integer variable
```cpp
int number = 10;
int *p_number = &number;
```

3. To obtain the `memory address` of a given variable, we use the `&` ampersand character to **prefix** our chosen variable, as seen above in line 2

4. `Memory addresses` are to be ***stored inside*** `Pointer` variables (data type), otherwise it will result in a *data type mismatch error* at compile time

5. To obtain a variable/value from its `memory address`, we use the `*` aestricks character [dereference operator] to prefix the `Pointer` variable/ Memory address of the variable/value we wish to obtain
* as seen below in **p_number1* in line 3, the dereference operator is used to dereference the `Pointer` variable *p_number1*, and obtain the value stored in said variable, in this case, the integer 11
```cpp
int number1 = 11;
int *p_number1 = &number1;
int value_number1 = *p_number1
```

6. Dereferencing CAN occur in assignment statements as well. As such, be **extremely clear** on whether a variable is a `Pointer`, or the `Variable stored within said Pointer`. 
* as seen below in line 3, we assign a new `memory address` of 20 to the `Pointer` variable, *p_number2*, while in line 4, we assign a **new integer value** of 20 to the dereferenced `Pointer` variable *p_number2*, which is pointing to the integer variable *number2*, thus assigning a new value of 20 to the integer variable *number2*
```cpp
int number2 = 12;
int *p_number2 = &number2;
p_number2 = 20;  //technically not possible, will result in compile error, addressed in point 7
*p_number2 = 20
```

7. **DO NOT** get confused with data types, as assigning an integer value to a `Pointer` variable is **IMPOSSIBLE** since `Pointer` variables (pointer data type) can only hold `Memory addresses` as their value!
* such *low-level control of memory* is not possible. As seen above, line 3 is incorrect due to a mismatch in data types. A fix would be to assign a new `memory address` to the `Pointer` variable *p_number2*, such as `p_number2 = &number3` if the integer variable *number3* were to exist

8. In C++, we deal with 2 kinds of memory primarily. 
* **STACK memory** is determined by the Scope of a given set of curly brackets `{}`, and everything **within** the Scope is killed the moment the program exits said Scope
* **HEAP memory** is determined by the `new` and `delete` operators, which will store chosen variables on the **HEAP** as *dynamically allocated memory*, allowing said variables to be referenced even past their **STACK** lifetime

9. Most times, when creating `Pointer` variables, we will be **dynamically allocating space** on the Heap for said variable with the `new` and `delete` operators, and the `&` ampersand character is rarely used, though it is important to know the respective function of each

------

### New 
* used to control the lifetime of variables in **HEAP** memory 
* `new` **prefixes** a chosen variable (normally a `pointer`) and stores said variables on the **HEAP** instead of on the Stack, essentially allowing said variable to continue to be referenced even past its STACK lifetime (determined by the {} scope)

EXAMPLE 1:
* as seen below in line 2, prefixing the data type "int" with `new` indicates to C++ that we wish to **dynamically allocate a reserved space in memory** on the **Heap** for the integer variable/value that is pointed to by the `Pointer` variable *p_number4*
* as seen below in line 3, we are assigning an integer value of 77 to the space in memory which the `Pointer` variable *p_number4* is pointing to, accessed by **dereferencing** said `Pointer` variable, and this space in memory has been reserved in the heap in line 2, when declaring "new int" for the `Pointer` variable 
```cpp
int *p_number4 = nullptr;
p_number4 = new int;
*p_number4 = 77
```

EXAMPLE 2:   
* as seen below in line 2, the integer variable *local_var* will die the moment the program exits the internal scope {} it is within
* however, the `Pointer` variable *p_local_var* has been **dynamically allocated a space in memory** in the **Heap**, and will not die even after the program has exited the stack's Scope, it can only be killed with the `delete` operator 
```cpp
int main (int argc, char **argv) 
{
    {
        int local_var = 33;
        int *local_pointer_var = new int;
    }
    return 0;
}
```

------

### Delete

* used to **return memory** to the **HEAP** by deleting variables that were stored in **HEAP** memory 

EXAMPLE 1:
* as seen below in line 4, upon the `Pointer` variable *p_number5* and its corresponding value at the reserved **Heap** `memory address` being used, we are to **prefix** the `Pointer` variable to be deleted with the `delete` operator to **release memory** back to the **Heap**
* as seen below in line 5, it is good practice to reassign the `Pointer` variable *p_number5* the value of nullptr once it has served its purpose inside your code, to **prevent** random memory addresses from being stored in a now empty `Pointer` variable and to **prevent** accidental usage of a now empty `Pointer` variable
```cpp
int *p_number5 = nullptr;
p_number5 = new int;
// rest of code
delete p_number5;
p_number5 = nullptr
```

------

## Dangling pointers
* `Pointer` variables that point to **invalid memory addresses**
* trying to dereference a dangling `pointer` will result in **errors at compile time**

There are 3 kinds of dangling pointers:
* Unitialized `pointers`
* Deleted `pointers`
* Multiple `pointers` pointing to the same `memory address`

------

### Unitialized pointers
occurs when creating a `Pointer` variable **without** assigning it any `memory address` or the nullptr value, and then attempting to print the `Pointer` variable/ **dereference** said `Pointer` variable
* as seen below in lines 2 and 3, attempting to print out an empty `Pointer` variable *p_number6* or **dereference** an empty `Pointer` variable will cause the program to crah 
```cpp
int *p_number6;
cout << p_number6 << endl;
cout << *p_number6 << endl;
```

------

### Deleted pointers
occurs after **deleting** a pre-existing `Pointer` variable, and then attempting to print said `Pointer` variable out/ **dereference** said `Pointer` variable
* as seen below in line 4, upon deleting the `Pointer` variable p_number7 and **releasing said memory** back to the **Heap**, attempting to **dereference** that `Pointer` variable will result in a crash
```cpp
int *p_number7 = new int 70;
// rest of code
delete p_number7;
cout << *p_number7 << endl;
```

------

### Multiple pointers
occurs when multiple `Pointer` variables are **assigned** the same `memory address`, and one of the `pointers` is then **deleted**, causing attempts to print out said `Pointer` variable/ **dereference** said `Pointer` variable will result in a crash 
* as seen below in lines 6 and 7, upon deleting the `Pointer` variable *p_number8*, which the `Pointer` variable *p_number9* derives its memory address (value) from, trying to print out *p_number9* and **dereference** the `Pointer` variable *p_number9* to obtain the variable stored at its `memory address` will result in a crash since said variable *p_number8* has been deleted 
```cpp
int *p_number8 = new int 80;
int *p_number9 = p_number8;
cout << p_number8 << endl;
cout << p_number9 << endl;
delete p_number8;
cout << p_number9 << endl;
cout << *p_number9 << endl;
```

------

## Reminders

to avoid the aforementioned problems, always do the following:
1. Always intialize `Pointer` variables with a `memory address`, if not, with the nullptr value
2. Upon deleting your `Pointer` variable and **releasing said memory** back to the **Heap**, reassign the value of nullptr to your `Pointer` variable to **prevent** possible junk memory addresses from forming
3. Assign a single `Pointer` variable to be the ***Master*** `Pointer` when assigning multiple `Pointer` variables the same `memory address`, such that the other ***Slave*** `Pointers` only read said `memory address` as a value, while the ***Master*** `Pointer` has the responsibility of *DELETING* itself and *RELEASING* the memory it takes up back to the **Heap** once the program has finished running

------

### Dynamic Arrays
Arrays that can be allocated onto **Heap** memory using the `new` operator, instead of conventionally on **Stack** memory (as seen above with other data types)
* as seen below in line 1, a `Pointer` variable that points to an Array of Doubles of size 10 is created and space on the **Heap** is dynamically allocated for said Array with the `new` operator
* in line 2, values are assigned to the **space reserved in the Heap memory** for the double array located at `memory address` of p_salaries
```cpp
double *p_salaries = new double [10];
*p_salaries = {1,2,3,4,5,6,7,8,9,10};
```

To delete the array from **Heap** memory once it has served its purpose and so **memory can be released** back to the **Heap**, we use the `delete` operator in the same way as with other data types
* as seen below in line 1, we **release memory back** to the **Heap** by deleting the `Pointer` variable *p_salaries* once it has been used. Additionally, we must remember to reassign the value of nullptr to our now empty `Pointer` variable to prevent garbage values from residing within it
```cpp
delete[] p_salaries;
p_salaries = nullptr;
```

**Dynamic arrays** are very different from Static arrays (conventional arrays) that exist on the **Stack**, and they have unique behaviours and properties which restrict them from utilising certain functions we used on normal arrays

* `std::size()` and *range-based* `for` *loops* do not work on Dynamic arrays, and attempts to do so will result in the Dynamic array **decaying into a pointer**. These functions will work on Static arrays (as we have previously explored).

* below is a Dynamic Array, which lives on the **Heap** (as space has been **dynamically allocated** for it in line 1)
```cpp
int *p_students = new int [20];
*p_students = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};
```
* below is a Static Array, which lives on the **Stack**, and will die once it is out of scope
```cpp
int students [10] = {1,2,3,4,5,6,7,8,9,10};
```

------

## References

![](https://miro.medium.com/max/704/1*CSUqZlUUG1qebGBl66cnVw.jpeg)

aliases for existing variables within our program, allowing us to *reference* these aliases to access the values and any other metadata (`memory address`, data type) stored within the main variable

### Declaring Reference variables
`Reference` variables are declared with the `&` ampersand character **suffixing** the data type of the *main variable* it is to reference, similar to the creation of `Pointer` variables above
* note that the value passed to a `Reference` variable is the main variable we want our Reference variable to access
* note that you **must** assign a value when intializing your `Reference` variable, it **cannot** be created without a value *(innate characteristic of Reference variables)*
* as seen below in line 2, a `Reference` variable *reference_number9* is created, and since it is created to reference the integer variable in line 1 *number9*, the `Reference` variable's data type is denoted as "int&"
```cpp
int number9 = 40;
int &reference_number9 = number9;
```

------

### Modifying Reference variables
altering the value of a `Reference` variable is done through the same simple assignment statements we have been using throughout C++
* note that any changes made to a `Reference` variable will **reflect** themselves on the original *main variable* as well *(characteristic unique to Reference variables)*
* as seen below in line 2, a `Reference` variable *reference_number10* that is referencing the integer variable *number10* is created. After the **reassignment of a new value** to the `Reference` variable *reference_number10* in line 3, the value of integer variable *number10* has now changed to 200 as well
```cpp
int number10 = 100;
int &reference_number10 = number10;
reference_number10 = 200;
cout << number10 << endl; // line 4 will print out the number 200
```

* the reverse situation (where the original main variable is modified, causing the `Reference` variable to reflect those changes) is likewise **true**, though this is extremely intuitive and does not need explaining

### A small note

* know that `Reference` variables and `Pointer` variables each have **their uses**, and while their applications might occasionally overlap, each of the two types have their quirks that make them suitable for specific situations 
* TLDR: `Reference` variables **do not** make `Pointer` variables obselete, and vice versa

------
