#include <iostream>
#include <cmath>

using namespace std;

int main () {

  int* p_number{};//C++ will automatically take an empty initalization (as seen here) to be an initialization to nullptr
  double* p_fractional_number{};

  int* p_number1{nullptr};
  double* p_fractional_number1{nullptr};
  
  return 0;
}

// --------------------

// (1) C++ POINTERs
// every variable and value in C++ has a specific memory address, which can be represented by a variable in the POINTER data type, denoted with an aestricks (*)
// *TLDR: a variable of POINTER DATA TYPE contains values of MEMORY ADDRESSES of different variables within the program
// eg. (int* p_integer {};) refers to a pointer storing the memory address of an INTEGER data type
// eg. (double* p_fractional {};) refers to a pointer storing the memory address of a DOUBLE data type

// ^^ NOTE that pointers can only store memory addresses of variables they have been statically defined to store (an int* pointer cannot store the memory address of a FLOAT/DOUBLE/STRING/CHAR/BOOL)
// additionally, note that the following are accepted as valid pointer notation (int* p_integer, int *p_integer)

// (1.1) nullptr
// C++ allows us to explicitely intialize pointers to the "nullptr" memory address when there is no defined memory address we want to store inside said pointer
// eg. (int* p_integer {nullptr};)

// (1.2) &
// the ampersand "&" operator allows us to access the exact memory address of the variable placed behind it
// eg. (int* p_integer = &integer;) will store the memory address of the integer variable within the p_integer pointer
//  
// (1.3) MEMORY ADDRESS ASSIGNMENT
// to reassign the memory address of a given variable in C++, we have to make use of both the (*) aestricks notation and the (&) ampersand operator, as seen below
// eg. (int* p_int = &integer;
//      p_int = &integer2;) will reassign the memory address of the variable integer2 to the pointer p_int
// ^^ NOTICE how there is no need to statically redeclare the pointer p_int's data type in the second line and assignment is all that is neccesary since it has been statically declared in the first line, similar to every other instance of static type declaration in C++

// (1.4) DEREFERENCING
// dereferencing is obtaining the value of a given variable from the memory address of the variable, achieved through the (*) dereferencer operator which prefixes the pointer variable (which points to the desired variable we want to extract said value from) 
// eg. (int* p_int2 {};
//      int int_data {56};
//      p_int2 = &int_data;
//      int value = *p_int2;) here, the dereferencer operator (*) is used in the fourth line to prefix the pointer variable, from which we are trying to obtain the value said variable contains

// (1.5) POINTER to CHAR types
// operates the same way we have been using pointers thusfar
// eg. (char* p_char{nullptr};
//      char character1{'A'};
//      p_char = &character1;
//      char value = *p_char;)
//
// *HOWEVER, char pointers can also be initialized with a STRING literal, where the string is taken as an array of constant CHARs
// eg. (char* p_message = "Hello World";) will take the first character of our character array (string literal) "Hello World", and point toward it
// therefore, the pointer p_message will point to the first character 'H', due to the data type of said pointer being a CHARACTER pointer
// as such:
// 1. "cout << *p_message << endl" will print out the character 'H'
// 2. "cout << p_message << endl" will print out the entire string "Hello World" due to the unique behaviour of CHAR arrays and pointers in C++

// ^^ NOTE that for most interaction with strings and characters, we can achieve the same result through use of string manpiluation instead of through pointers

// ---------------------

// (2) MEMORY MAP
// computer memory is composed of the following 5 components:
// 1. System
// 2. Stack
// 3. Heap
// 4. Data
// 5. Text

// (2.0) zooming in...

// 2. STACK
// *finite memory
// *develepor LACKS control of Stack's memory lifetime
// Lifetime is determined by the SCOPE MECHANISM (determined by the curly braces {} in C++ to determine the scope of variables and values)

// 3. HEAP
// *finite memory
// *develepor HAS full control over Heap's memory allocation and release lifetime
// Lifetime is controlled explicitily through NEW and DELETE operators

// ----------


// (2.1.0) ~SOME THINGS TO CLARIFY BEFORE WE MOVE ON~
// for further clarification, refer to my telegram chat with Ze Ming on 1/5/2022, as well as the picture of Anya pointing toward another pointer for reference
// regarding pointers, understand the following:

// 1. Pointers are a DATA TYPE, and pointer Variables store memory addresses as their values

// 2. In the assignment statement for a pointer variable, use the aestricks (*) to suffix the data type that the pointer will be pointing toward
        // eg. as seen below in line 2, the "int*" pointer data type indicates that the pointer variable p_number is storing the memory address of an integer variable

int number = 10;
int* p_number = &number;

// 3. To obtain the memory address of a given variable, we use the ampersand (&) operator to prefix our chosen variable, as seen above in line 2

// 4. Memory addresses are to be stored inside Pointer variables (data type), otherwise it will result in a data type mismatch error at compile time

// 5. To obtain a variable/value from its memory address, we use the dereference aestricks (*) operator to prefix the Pointer variable/ Memory address of the variable/value we wish to obtain
        // eg. as seen below in line 3, the "*p_number1" dereference operator is used to dereference the Pointer variable p_number1, and obtain the value stored in said variable, in this case, the integer 11

int number1 = 11;
int* p_number1 = &number1;
int value_number1 = *p_number1

// 6. Dereferencing CAN occur in Assignment Statements as well. As such, be extremely clear on whether a variable is a Pointer, or the Variable stored within said Pointer. 
        //eg. as seen below, line 3 will assign a new memory address of 20 to the Pointer variable, p_number2, while line 4 will assign the new integer value of 20 to the dereferenced Pointer variable (p_number2), which is pointing to the integer variable (number2), thus assigning a new value of 20 to the integer variable number2!

int number2 = 12;
int* p_number2 = &number2;
p_number2 = 20; // technically not possible, will result in compile error, addressed in point 7
*p_number2 = 20

// 7. DO NOT get confused with data types, as assigning an integer value to a Pointer variable is IMPOSSIBLE since Pointer variables (pointer data type) can only hold Memory addresses as their value!
        // eg. as seen above, such low-level control of memory is not possible. Line 3 is incorrect due to a mismatch in data types. A fix would be to assign a new memory address to the Pointer variable (p_number2), such as "p_number2 = &number3" if the integer variable number3 were to exist

// 8. In C++, we deal with 2 kinds of memory primarily. 
        // STACK memory is determined by the Scope of a given set of curly brackets {}, and everything within the Scope is killed the moment the program exits said Scope. 
        // HEAP memory is determined by the "new" and "delete" operators, which will store chosen variables on the HEAP as dynamically allocated memory, allowing said variables to be referenced even past their STACK lifetime.

// 9. Most times, when creating Pointer variables, we will be dynamically allocating space on the Heap for said variable with the "new" and "delete" operators, and the ampersand (&) operator is rarely used, though it is important to know the respective function of each. 

// ----------

// (2.1.1) NEW 
// used to control the lifetime of variables in HEAP memory 
// "new" prefixes a chosen variable (normally a POINTER) and stores said variables on the HEAP instead of on the Stack, essentially allowing said variable to continue to be referenced even past its STACK lifetime (determined by the {} scope)
        // EXAMPLE 1:
        // eg. as seen below in line 2, prefixing the data type "int" with "new" indicates to C++ that we wish to dynamically allocate a reserved space in memory on the heap for the integer variable/value that is pointed to by the Pointer variable (p_number4)
        // eg. as seen below in line 3, we are assigning an integer value of 77 to the space in memory which the Pointer variable p_number4 is pointing to, accessed by dereferencing said Pointer variable, and this space in memory has been reserved in the heap in line 2, when declaring "new int" for the Pointer variable 

int* p_number4 = nullptr;
p_number4 = new int;
*p_number4 = 77

        // EXAMPLE 2:   
        // eg. as seen below in line 2, the integer variable local_var will die the moment the program exits the internal scope {} it is within
        // eg. however, the Pointer variable p_local_var has been dynamically allocated a space in memory in the Heap, and will not die even after the program has exited the stack's Scope, it can only be killed with the "delete" operator 

int main (int argc, char **argv) 
{

    {
        int local_var = 33;
        int* local_pointer_var = new int;
    }

    return 0;
}

// ----------

// (2.1.2) DELETE
// used to return memory to the HEAP by deleting variables that were stored in HEAP memory 
        // EXAMPLE 1:
        //eg. as seen below in line 4, upon the Pointer variable p_number5 and its corresponding value at the reserved Heap memory address being used, we are to prefix the Pointer variable to be deleted with the "delete" operator to release memory back to the Heap
        //eg. as seen below in line 5, it is good important practice to reassign the Pointer variable p_number5 the value of nullptr once it has served its purpose inside your code, to prevent random memory addresses from being stored in a now empty Pointer variable and to prevent accidental usage of a now empty Pointer variable

int* p_number5 = nullptr;
p_number5 = new int;
//{rest of code};
delete p_number5;
p_number5 = nullptr

// ----------

// (2.1.3) Dangling pointers
// Pointer variables that point to invalid memory addresses, trying to dereference a dangling pointer will result in errors at compile time
// there are 3 kinds of dangling pointers, Unitialized pointers, Deleted pointers, and Multiple pointers pointing to the same memory address

// -----

// (2.1.3.1) Unitialized pointers
// occurs when creating a Pointer variable without assigning it any memory address or the "nullptr" value, and then attempting to print the Pointer variable/ dereference said Pointer variable
        //eg. as seen below in lines 2 and 3, attempting to print out an empty Pointer variable p_number6 or dereference an empty Pointer variable will cause the program to crah 

int* p_number6;
cout << p_number6 << endl;
cout << *p_number6 << endl;

// -----

// (2.1.3.2) Deleted pointers
// occurs after deleting a pre-existing Pointer variable, and then attempting to print said Pointer variable out/ dereference said Pointer variable
        //eg. as seen below in line 4, upon deleting the Pointer variable p_number7 and releasing said heap space back to memory, attempting to dereference that Pointer variable will result in a crash

int* p_number7 = new int 70;
{rest of code};
delete p_number7;
cout << *p_number7 << endl;

// -----

// (2.1.3.3) Multiple pointers
// occurs when multiple Pointer variables are assigned the same memory address, and one of the pointers is then deleted, here, attempts to print out said Pointer variable/ derefence said Pointer variable will result in a crash 
        //eg. as seen below in lines 6 and 7, upon deleting the Pointer variable p_number8, which the Pointer variable p_number9 derives its memory address (value) from, trying to print out p_number9 and dereference the Pointer variable p_number9 to obtain the variable stored at its memory address will result in a crash since said variable (p_number8) has been deleted 

int* p_number8 = new int 80;
int* p_number9 = p_number8;
cout << p_number8 << endl;
cout << p_number9 << endl;
delete p_number8;
cout << p_number9 << endl;
cout << *p_number9 << endl;

// -----

// (2.1.3.4) REMINDERS
// to avoid the aforementioned problems (2.1.3.1 - 2.1.3.3.), rememeber to do the following:
        // 1. Always intialize Pointer variables with a memory address, if not, with the "nullptr" value
        // 2. Upon deleting your Pointer variable and releasing said Heap memory back to the Heap, reassign the value of "nullptr" to your Pointer variable to prevent possible junk memory addresses from forming
        // 3. Assign a single Pointer variable to be the "MASTER" Pointer when assigning multiple Pointer variables the same memory address, such that the other "SLAVE" Pointers only read said memory address as a value, while the "MASTER" Pointer has the responsibility of DELETING itself and RELEASING the memory it takes up back to the Heap once the program has finished running

// --------------------

// (3) DYNAMIC ARRAYS
// *Arrays that can be allocated onto Heap memory using the "new" operator, instead of conventionally on Stack memory (as seen above with other data types)
        //eg. as seen below in line 1, a Pointer variable that points to an array of doubles of size 10 is created and space on the Heap is dynamically allocated for said array with the "new" operator, and in line 2, values are assigned to the space reserved in the Heap memory for the double array located at memory address of p_salaries

double* p_salaries = new double [10];
*p_salaries = {1,2,3,4,5,6,7,8,9,10};

// to delete the array from Heap memory once it has served its purpose and for memory can be released back to the Heap, we use the "delete" operator in the same way as with other data types
        //eg. as seen below in line 1, we release memory back to the Heap by deleting the Pointer variable p_salaries once it has been used. Additionally, we must remember to reassign the value of nullptr to our now empty Pointer variable to prevent garbage values from residing within it

delete[] p_salaries;
p_salaries = nullptr;

// -----

// (3.1) MOST IMPORTANT TAKEAWAY:
// Dynamic arrays are very different from Static arrays (conventional arrays) that exist on the Stack, and they have unique behaviours and properties which restrict them from utilising certain functions we used on normal arrays
// std::size() and range-based for loops do not work on Dynamic arrays, and attempts to do so will result in the Dynamic array 'decaying into a pointer'. These functions will work on Static arrays (as we have previously explored) however.

        //eg. below is a DYNAMIC ARRAY, which lives on the Heap (as space has been dynamically allocated for it in line 1)
int* p_students = new int [20];
*p_students = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};

        //eg. below is a STATIC ARRAY, which lives on the Stack, and will die once it is out of scope
int students [10] = {1,2,3,4,5,6,7,8,9,10};

// --------------------

// (4) REFERENCES
// aliases for existing variables within our program, allowing us to 'reference' these aliases to access the values and any other metadata (memory address, data type) stored within our main variable

// -----

// (4.1) DECLARING Reference variables
// Reference variables are declared with the ampersand (&) operator suffixing the data type of the main variable it is to reference, similar to the creation of Pointer variables above
// *NOTE that the value passed to a Reference variable is the main variable we want our Reference variable to access
// *NOTE that you MUST assign a value when intializing your Reference variable, it cannot be created without a value (innate characteristic of Reference variables)

        //eg. as seen below in line 2, a Reference variable reference_number9 is created, and since it is created to reference the integer variable in line 1 number9, the Reference variable's data type is denoted as "int&"
int number9 = 40;
int& reference_number9 = number9;

// -----

// (4.2) MODIFYING Reference variables
// altering the value of a Reference variable is done through the same simple assignment statements we have been using throughout C++
// *HOWEVER, note that any changes made to a Reference variable will REFLECT on the original main variable as well! (characteristic unique to Reference variables)

        //eg. as seen below in line 2, a Reference variable reference_number10 that is referencing the integer variable number10 is created. After the reassignment of a new value to the Reference variable reference_number10 in line 3, the value of integer variable number10 has now CHANGED to 200 as well
int number10 = 100;
int& reference_number10 = number10;
reference_number10 = 200;
cout << number10 << endl; // here, line 4 will print out the number 200

//*OBVIOUSLY, the reverse situation (where the original main variable is modified, causing the Reference variable to reflect those changes) is likewise TRUE, though this is extremely intuitive and does not need explaining

// -----

// (4.3) CAVEAT
// *know that Reference variables and Pointer variables each have their uses, and while their applications might occasionally overlap, each of the two types have their quirks that make them suitable for specific situations 
    //(TLDR; Reference variables do NOT make Pointer variables obselete, and vice versa)

// --------------------
