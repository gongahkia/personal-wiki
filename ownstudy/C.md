# `C`

* C is a low-level programming language, with a large number of operating systems and programming language compilers being written in C

----------

### Printing to the console 

```C 
#include <stdio.h>
#include <stdlib.h>

int main () {

printf("Hello World!");

return 0;
}
```

* `printf("Hello World!")` : printing to the console/ terminal is much easier in C than the nonsense that is `cout << "Hello World!" << endl;` in C++

### Formatted strings

```C 
char personName [] = "Shelby";
printf("My name is %s", personName);
```

* Here, the `%s` acts as a placeholder variable, allowing us to incorporate string data types into our print statements
* Note that there are different placeholder values used for each data type

```C 
int Number = 20;
char Name [] = "Wazowski";
char Grade = 'A';
float GPA = 2.1930;

printf("Hello, my name is %s and I have %d siblings, with a GPA of %f and an overall grade of %c, Name, Number, GPA, Grade");
```

As seen above...
* `%s` is used for Strings (character arrays)
* `%c` is used for Characters
* `%d` is used for Integers (digits)
* `%f` is used for Floats 
* `%lf` is used for Doubles (long float)
* `%p` is used for Pointers (which store memory addresses)

Additionally, note that the order of the variables mentioned after the print statement are important to their relative order in the formatted string

----------

### Variables 

* C is a statically typed language, which means a value's data type must be statically declared when initializing a variable
* Similar to C++, value reassignment to a variable does not require a redecleration of the variable's data types

```C 
char personName [] = "Juan";
int personAge = 20;
personName = "Tamares";
```

### Data Types

* Integer `int`
* Float `float`
* Double `double`
* Character `char`
* String (Character Array) `char []`
* Pointers (Memory addresses) `&`

```C 
int age = 40;
float waterbottles = 20.1;
double manywaterbottles = 10.87364550;
char grade = 'A';
char word [] = "Why Wenjie angry at me bro?";
```

* note that C does not have the Boolean data type, and the `Int` values of 1 and 0 are conventionally used to represent true and false respectively

----------

### Constants 

* Constants are values that **cannot** be modified after initialization and assignment
* `const` prefixes the constant variable, though note that the constant's data type is still statically declared during initialization

```C
int number = 10;
const int NUM = 20;
```

* By convention, Constants variable names are written in full Uppercase to distinguish them from normal variables

----------

### Integers && Floats && Doubles

* `+` is used for Addition
* `-` is used for Subtraction
* `*` is used for Multiplication
* `/` is used for Division
* `%` is the Modulo operator

Implicit type conversion (eg. any operation between an Integer and a Float/Double will result in a Float/Double)

### Math Functions 

C has a bevy of built-in Math functions that can be called at any time

* `pow()` takes in 2 arguments, and raises the first argument to the **Power** of the second
* `sqrt()` takes in 1 argument, and returns the **Squarerooted** value
* `ceil()` takes in 1 decimal argument, and **Rounds up** the argument 
* `floor()` takes in 1 decimal argument, and **Rounds down** the argument

----------

### Comments 

* `/* */` comments out **multiple lines**
* `//` comments out a **single line**

----------

### User Input

* `scanf()` takes in 2 arguments, the placeholder data type of the user input we want to accept and the variable name to store said user input in, which is prefixed by the ampersand `&` character
* note that the variable we wish to store user input in should be initialized already

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    
    int age;
    printf("Enter your age: ");
    scanf("%d", &age);

    return 0;
}
```

* additionally, note that acccepting Strings (character arrays) as user input operate slightly differently as compared to the other data types 
* since C is a statically typed language, we will need to declare the length of the given character array we wish to take in as user input during intialization 
* note that the variable name of the character array does **not** need to be prefixed by the ampersand character
* one of the **limitations** of `scanf()` when taking in Strings as user input is that it stops scanning once it hits a *whitespace character* 

```C
#include <stdio.h>
#include <stdlib.h>

int main() {

    char Name [20];
    scanf("%s", Name);

    return 0;
}

```

* `fgets()` takes in 3 arguments, the variable in which we wish to store the user input, the number of characters we want to accept as user input, and ***stdin*** (which specifies to C that we want to accept the user input from the console)
* this function indiscriminately takes in the entirity of a user's input as a string of characters (character array), **including whitespace characters**
* note that similar to `scanf()`, `fgets()` demands we first initailize the variable we wish to store our user input within before we can use the function

```C
#include <stdio.h>
#include <stdlib.h>

int main() {

    char Name [20];
    fgets(Name, 20, stdin);

    return 0;
}
```

----------

### Arrays 

* list data structure
* data types of the values stored in the array, as well as the size of the array, must be statically declared upon initialization or assignment (such as a String, which is a character array `char String [];`)

```C
int luckyNumbers [] = {4, 8, 15, 16, 23, 42}; 
printf("d", luckyNumbers[0]);

int unknownNumbers [10];
unknownNumbers[0] = 2;
unknownNumber[1] = 3;
```

* similar to in other languages, indexing of items within an array occurs with `[]` square brackets

### 2-dimensional Arrays

* a nested array
* note that we must statically declare the data type of the nested array with two `[]` square brackets

```C
int numsHolder [][] = {
                        {1,2},
                        {3,4},
                        {5,6}
                    };
```

* additionally, accessing elements in a nested array via index functions the same way as in other languages, with double `[]` square brackets

```C
printf("%d", numsHolder[1][1]);
```

----------

### Functions 

C lacks object-oriented programming patterns (which are introduced in C++), and so functions are even more important in C   
* C functions operate the same way as C++, where we need to statically declare the data type returned by said function

* `void` functions return no value
* `int` functions return an Integer value
* `char` functions return a Character value
* `float` functions return a Floating point value
* `double` functions return a Long floating point value

```C
void sayHi() {
    printf("Hello World!");
}
```

* additionally, any parameters taken in by a given function must also statically declare their data types

```C 
char firstname(char name[]) {
    return "%c, name[0]";
}
```

----------

### If && Else if && Else

* conditional flow in C operates the same way as other programming languages, with a near identical syntax to that of C++ and Javascript
* as mentioned above, there is no Boolean data type in C, with true being represented by the Integer 1, and false being represented with the Integer 0

```C
int max(int number1, int number2) {
    if (number1 > number2) {
        return number1;
    } else if (number1 == number2) {
        return "The numbers are of equal value";
    } else { return number2;
    }
}
```

### && and || and !

As in other languages, the following logical operators can be combined with the aforementioned `If`, `Else if` and `Else` conditional flow operators to check multiple conditions 

* `&&` is the **and** operator
* `||` is the **or** operator
* `!` is the **not** operator

----------

### Switch statements

* switch statements are offered as an alternative to multiple `if`, `else if` and `else` statements, and function similarly to how they work in C++, Javascript and Python
* always remember to include the `break` statement (break condition) in your `switch()` statement so as to ensure the statement closes after the given condition has been met

```C
int main() {
   
   char grade = 'A';

   switch(grade) {
    case 'A':
        printf("Your grade is A")
        break
    case 'B':
        printf("Your grade is B")
        break
    case 'C':
        printf("Your grade is C")
        break
    case 'D':
        printf("Your grade is D")
        break
    default:
        printf("Unrecognised input")
    }
    return 0;
}
```

* as seen above, the `default` case is reserved for a situation where none of the above conditions are fulfilled, in which case the `default` condition will be run instead 
* note that since the `default` case is the last possible option, there is **no need** for a `break` statement to follow it, since the control flow will naturally terminate after the `default` statement has been executed

----------

### Structs

* A `Struct` is a data structure in C, that appears relatively similar to a dictionary, but allow for some object oriented patterns to be practiced in C

```C
struct Student{
    char name[50];
    char major[50];
    int age;
    double gpa;
}
```

* to intitialize an **instance** of a `Struct` however, we have to declare it as its own *data type*, as seen below
* note that since we cannot casually reassign a ***String value*** to an instance variable of the `Struct` Student, we have to use the `strcpy()` function to copy said value into the String (character array) variable Name
* `strcpy()` takes in 2 arguments, the **instance variable** we want to copy said value into, and the **value** we want to copy into said variable

```C
int main() {

    struct Student student1;
    student1.age = 22;
    student1.gpa = 4.267;
    strcpy(student1.name, "Markos");
    strcpy(student1.major, "Youtube");

    struct Student student2;
    student2.age = 31;
    student2.gpa = 3.198;
    strcpy(student2.name, "Johnny Dogs");
    strcpy(student2.major, "History");
    
    return 0;
}
```

----------

### While loops

* `while` loops in C function exactly the same as in any other programming language, where the chunk of code enclosed in the loop repeatedly executes until the `while` condition is **broken** (condition becomes unmet)
* in the absence of Boolean data values, *while true* can simply be replaced by *while value = 1*

```C
int index = 1;
while (index <= 5) {
    printf("%d", index);
    index++;
}
```

* note that `do while` loops function similarly in C as in other programming languages as well

### For loops 

* `for` loops in C function near identically to how they function in any other progamming language, where the chunk of code enclosed in the loop is repeated as long as the condition within the `for` loop is iterated over
* as in C++ and Javascript, `for` loops can be used to **iterate over** arrays and Strings (character arrays)

```C
for (let i=0; i<5; i++) {
    printf("%d", &i);
}

char luckyString [] = "Hello Brother";

for (let q=0; q<strlen(luckyString); q++) {
    printf("%c", luckyString[q]);
}
```

* note that nested `for` loops can be used to iterate through a two-dimensional nested array to access all elements

----------

### Memory addresses

* every variable stored in a program has a specific address in the computer's memory, known as its **memory address**

### Pointers 

* `pointers` are their own unique *data type* (such as int, float, double, char, string), and are the variable within which we store the `memory address` values
* to print out the `memory address` of a variable to the console, we use the `%p` pointer placeholder value

```C
int age = 30;
double gpa = 4.2;
char grade = 'A';

printf("%p", &age);
printf("%p", &gpa);
printf("%p", &grade);
```

* the `&` ampersand character is the **operator** we use to access a variable's `memory address`, by **prefixing** the given variable with the `&` ampersand character
* we assign the obtained value (`memory address`) to a `pointer` variable (which is initialized by **prefixing** the `pointer` variable with the `*` aestricks character [dereferencing operator])
* note that the naming convention for pointer variables is *p_{variable name}*

```C
int personAge = 30;
double personGpa = 3.4;
char personGrade = 'B';

int *p_personAge = &personAge;
double *p_personGpa = &personGpa;
char *p_personGrade = &personGrade;
```

### Dereferencing Pointers

* the `*` aestricks character is the **dereferencing operator**, which we use to obtain a variable's ***value*** from its `memory address`
* effectively, the dereferencing operator `*` has the opposite effect to the operator `&`, which is used to obtain a variable's memory address from a variable

```C
int boyAge = 5;
int *p_boyAge = &boyAge;

printf("%d", *p_boyAge);
```

----------

### Writing && Appending to Files

* to **write** or **append** to files in C, we create a `pointer` variable which points to a file within our local machine, with the *FILE* stream (effectively its own data type) prefixing the pointer at initialization
* `fopen()` function takes 2 arguments, the **file name** and the **mode** we want to access the file with (`r` for read, `w` for write, `a` for append) 
* `fprintf()` function takes 2 arguments, the **pointer variable** which points to the file we opened, and the **data** we want to write to the file
* `fclose()` function takes 1 argument, the **pointer variable** which points to the file we opened

```C
#include <stdio.h>
#include <stdlib.h>

int main() {

    FILE *p_file = fopen("input.txt", "w");
    fprintf(p_file, "Hello World!\nIt's time to do the singing and the dancing at Orchard road.")
    fclose(p_file);

    return 0;
}
```

* note that while in `w` write mode, any data written to a file will **completely override** any *existing* data on the file
* we use `a` append mode to write *additional* data to the existing data stored in the file 

### Reading Files

* to **read** information from a file, we use the aforementioned `fopen()` and `fclose()` functions in the same manner, with `fopen()` being invoked in `r` read mode 
* `fgets()` function takes in 3 arguments, the **variable** we want to assign the data `fgets()` reads to, the **maximum amount** of data we want `fgets()` to read each time (an *integer*), and the **pointer variable** which points to the file we opened

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    
    char line[255];
    FILE *p_file2 = fopen("input2.txt", "r");
    fgets(line, 255, p_file2);
    printf("%s", line);
    fclose(p_file2);

    return 0;
}
```

----------
