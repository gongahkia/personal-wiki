#include <iostream>
#include <cmath>

using namespace std;

// int main() works similarly to fn main() in Rust, automatically running upon execution of the C++ program
int main()
   {

    // statically define the data type of said variable when declaring it
    // like in Rust, we can declare a variable first, and subsequently assign a value to said variable, as seen in line 17
    // ^^ however, be aware that for some bizzare reason, C++ sometimes assigns random garbage values to variables which have been statically declared, but have not been assigned a value at declaration time (documentation found at (https://stackoverflow.com/questions/15030379/c-why-does-this-variable-return-a-random-value))
    // *note that while Assignment initialization is the most commonly used (=), other methods like Braced initialization and Functional initialization have their respective pros and cons, that should be considered prior to their usage

    string output_message = "Hello World";
    int output_time = 20;
    float output_minutes;
    output_minutes = 25.9;

    // string concatenation in C++ is more annoying, have to include it in print order
    // print statements are comprised of cout (console out) and endl (endline)
    cout << "This is our output message: " << output_message << ", and this is our output time: " << output_minutes << endl;


    // declaring a variable of integer data type to store user input within
    int age;
    cout << "Enter your age: ";

    // cin (console in) to indicate the program is taking in user input, note the opposite direction of arrows indicating inflow of user input (as compared to the arrows for cout)
    // store user input in a variable by indicating said variable after the cin >> command
    // *note that the above "cin" works for taking in user inputted CHAR, INT, FLOAT and DOUBLE
    cin >> age;
    cout << "You are " << age << " years old." << endl;

    // declaring a variable of string data type to store user input within
    string username;
    cout << "Enter your name: ";

    // getline() creates an entire line of text that we can allocate user input to
    // ^^ the getline() function takes in 2 parameters, "cin" and the name of the variable we want to use to store the user's line of text
    getline(cin, username);

    cout << "Your name is: " << username;

    return 0;
}

// --------------------

// Installing C++ compiler
// (1) install the mingw C++ compiler build for windows set-up
// (2) add the folder within which the C++ compiler (g++) and other compiler executable files are in (normally 'bin') to the main file path [search 'edit the system environment variables' in settings]
// (3) restart laptop and input "g++" into terminal/ windows powershell to test whether g++ has been succesfully installed

// Compiling and running C++ files
// *similar to Rust, C++ files must first be compiled before they can be run as an executable file
// (1) to compile a C++ file, input "g++ <C++ file name>" into the powershell/ terminal
// ^^ the executable file will be named 'a' by default, to specify file name, upon compiling, input "g++ <C++ file name> -o <desired executable file name>"

// Update C++ to desired verison
// *enter terminal, input "-std = c++ <version number>"
// eg. "-std=c++17" to install C++ version 17

// ----------

// C++ FEATURES
// (1) Core features
// these are classified as the fundamental logic and syntax that govern how the C++ program works (such as declaration and assignment statements)

// (2) Standard Library features
// these are highly specialised features, which have been curated into specific libraries as needed for C++'s more complex requirements
// eg. #include <iostream>, #include <fstream>, #include <string>, #include <vector>, #include <numeric>

// (3) STL features
// these are a collection of container types contaning various algorithms, an offshoot of the C++ standard library

// ----------

// Standard namespaces (using namespace std;)
// *by mentioning to use the standard namespace at the beginning of the C++ program, we indicate to C++ that we want to take all the utility present in the std namespace and apply it within the global namespace
// ^^ doing the above allows us to avoid the syntax of "std" and "::" (the scope operator) whenever we use functions that require the standard namespcae (eg. std::cout, std::endl, std::cerr, std::clog)

// *cin 
// these functions handle user input, used to take in data from the console inputted by the user at runtime

// *cout, endl 
// these function as print statements, used to output data to the console at runtime, explained above in the main function (int main())

// *cerr, endl
// console error functions similarly to cout, though it is used to dictate data that should be outputted to the console when an error is detected, functionally allowing us to add desired information to error logs
// ^^ as seen above, "cerr" functions identically to the cout print statement, and can be used in conjunction with "endl" to print a newline at the end of said error message

// *clog 
// in larger systems, "clog" is used to log data for non-critical events that occur during compile/ runtime, and is thus preferred as compared to "cerr"
// ^^ note that for most of the small project programming that I will be initially doing with C++, "cerr" and "clog" will be rarely used

// ----------

// DATA TYPES in C++ (must statically define data types of variables)
// (1) INT: if a variable is declared as an INT type, and is subsequently assigned a FLOAT/DOUBLE value, the variable's value will round down/up to fit its INT data type
// (2) FLOAT: stores less decimal points than DOUBLE, less memory required
// (3) DOUBLE: can store more decimal points than FLOAT, more memory required
// (4) CHAR: single character, represented with single quotation marks '', *we can use ASCII table values interchangably with C++ values of the char data type, (eg. "char value = 65" when printed, will cout 'A', since the ASCII value of 'A' is 65), and the character data type takes up 8 bits/ 1 byte
// (5) STRING: an array of CHARs , represented with double quotation marks ""
// (6) BOOL: true returns true or 1 (when printed out), false returns false or 0 (when printed out), *notice true and false are both lowercase, and the boolean data type takes up 8 bits/ 1 byte
// (7) POINTERS: items that point to memory addresses (elaborated more on below under "pointers in C++")
// (8) AUTO: a C++ reserved word that indicates to the C++ compiler to deduce the data type of a given value for us
// (9) VOID: a typeless type, mostly relevant for use in void functions which return no value

// ----

// (10) USER INPUT: use "cin" for taking in CHAR, INT, FLOAT, DOUBLE and "getline()" for STRINGs

// -----

// DATA STRUCTURES in C++ (must statically define data type of values within data structure)
// (11) ARRAY: lists in C++, where the size of the array must be STATICALLY declared, and square brackets are replaced by {} 
// (12) VECTOR: lists in C++, where the size of the array can be DYNAMICALLY altered, and square brackets are replaced by {}
// (13) UNORDERED MAP: dictionaries in C++ (similar to dictionaries in Python, which store key-value pairs in an unordered state)
// (14) HASH MAP: ordered dictionary in C++, which stores key-value pairs in an ordered state

// ----------

// for further documentation on any reserved words gone over here, read up at (https://cplusplus.com/)

// ----------

// (1) INTs, (2) FLOATs, (3) DOUBLEs
// INT: whole numbers
// FLOAT, DOUBLE: decimal point numbers, a DOUBLE can hold more decimal points than a FLOAT
// '+' addition , '-'subtraction, '/' division, '*' multiplication, '%' modulo operators all function the same way as in Python
// C++ adheres to all BODMAS rules: brackets take precedence over multiplication and division which takes precedence over addition and subtraction
// *note that C++ accepts numbers in their Decimal (15), Octal (017), Hexadecimal (0x0F) and Binary (0b00001111) representations, though we most commonly interact with the Decimal representation
// *additionally, the "sizeof(<number>)" function allows us to get the size of a given integer, float, double or long double in bytes

// -----

// (1.1) NUMBER OPERATIONS
// use number operations the same way you would with Python
// note that the number operations below are valid on INT, FLOAT and DOUBLE data types, barring the below caveat
// *CAVEAT: all operations that occur between an INT and a FLOAT/DOUBLE will return a FLOAT/DOUBLE
// *for all examples below, take eg. 'int intger = 5'

// (1.1.1) ++
// eg. 'intger ++' would return 6
// to increment a given INT by 1, often used within a loop or recursion

// (1.1.2) --
// eg. 'intger --' would return 4
// to decrement a given INT by 1, often used within a loop or recursion

// (1.1.3) += and -=
// eg. 'intger += 50' would return 55
// functions the same way as in Python, to increment/decrement a given INT by the value after the +=/-=, often used within a loop or recursion

// (1.1.4) *= and /=
// eg. 'intger *= 2' would return 10
// eg. 'intger /= 5' would return 1
// functions the same way as in Python, to multiply/divide a given INT by the right operand, and assign the result to the initial variable, often used within a loop or recursion

// (1.1.5) %=
// eg. 'intger %= 2' would return 1
// functions the same way as in Python, to modulus a given INT by the right operand, and assign the result to the initial variable, often used within a loop or recursion

// *caveat: note that for the above examples (1.1.1) and (1.1.2), these fall under POSTFIX increment/ decrement (the value of said variable is incremented/ decremented after processing the variable's value per recursion)
// seperately, PREFIX increment/ decrement can also be used when the situation calls for a variable's value to be incremented/ decremented prior to its value being processed by the program per recursion
// ^^ for documentation on the above note (5:16:00 from https://youtu.be/8jLOx1hD3_o)

// -----

// (1.2) NUMBER FUNCTIONS
// math functions must be imported into your C++ file using the <cmath> module ('#include <cmath>)
// use number functions the same way you would with Python
// *google for more C++ math function documentation

// (1.2.1) pow()
// eg. 'pow(2,5)' would return 32
// returns the value of first parameter raised to the value of the second parameter (in this case, 2^5)

// (1.2.2) sqrt()
// eg. 'sqrt(36)'would return 6
// returns the squarerooted value of the inputted parameter

// (1.2.3) round()
// eg. 'round(4.6)' would return 5
// returns the rounded value of the inputted parameter, following numerical rounding convention
// *also see: ceil() always rounds up to nearest whole number
// *also see: floor() always rounds down to nearest whole number

// (1.2.4) fmax(), fmin()
// eg. 'fmax(7,14)' would return 14
// eg. 'fmin(7,14)' would return 7
// fmax() returns the larger of the two inputted parameters
// fmin() returns the smaller of the two inputted parameters

// -----

// (1.3) NUMBER MODIFIERS [signed, unsigned, short, long, long long]
// these all refer to categories integers fall into regarding their length and nature, and how many bytes they occupy in memory
// ^^ these modifiers operate as declarative data types, similar to Rust's static declaration of signed and unsigned integers which can store an integer value of variable amount of bits (i8,i16,i32,i64,i128,u8,u16,u32,u64,u128)
// *note that these modifiers only work on integral types (data types in which decimal and whole numbers can be stored [int])

// (1.3.1) SIGNED, UNSIGNED
// similar to Rust, signed and unsigned integers refer to integers that can store positive values only (unsigned), or both negative and positive values (signed), similar to i8-i128 and u8-u128 in Rust
// eg. 'unsigned int value1 = 1'
// eg. 'signed int value2 = 2'
// eg. 'signed int value3 = -10'
// ^^ additionally, note that misdeclaring data types (eg. 'unsigned int value4 = -10') will result in a COMPILE error at compile time

// (1.3.2) SHORT, LONG, LONG LONG
// similar to Rust, short, long, and long long all refer to different sizes integers fall into based on their value, similar to i8, i16, i32, i64, i128 and u8, u16, u32, u64, u128 in Rust
// SHORT: 16 bits/ 2 bytes (i16,u16)
// LONG: 32-64 bits/ 4-8 bytes (i32-i64, u32-u64)
// LONG LONG: 64 bits/ 8 bytes (i64, u64)
// ^^ additionally, note that signed and unsigned integers of the aforementioned integer value sizes will affect the size said integer occupies in memory 

// -----

// (2)-(3) FLOAT, DOUBLE, LONG DOUBLE
// when referring to numbers with decimal points in C++, we are able to represent said number with the following 3 data types, all of which take up varying amounts of space in memory
// ^^ when specifying which data type to use, note the PRECISION required, which is calculated by the number of decimal points + number of numbers before the decimal point (eg. the precision of 1.23456700001 is 12)
// *similar to integer modifiers, this finer level of control C++ gives us in allowing specificity when statically declaring data types can net COMPILE time errors should a data type be misdeclared during the assignment statement
// *additionally, values of the float, double or long double data types can be represented by scientific notation (eg. 'double number1 = 1.92400023e8') aside from the conventional decimal point notation

// (2.1) FLOAT: 32 bits/ 4 bytes, precision of 7
// *note that when declaring a float data type, we must suffix the value of said data type with 'f' to indicate to C++ that it is a float, otherwise the C++ compiler will automatically chop off a chunk of said float since the default decimal point data type is the double
// eg. "float number2 = 1.12345678901234567890f"

// (3.1) DOUBLE: 64 bits/ 8 bytes, precision of 15 
// the Double is the commonly used default by the C++ compiler for decimal point values
// *note that there is no need to suffix a double data type value with anything, since the C++ compiler takes such numbers as doubles by default
// eg."double number3 = 1.12345678901234567890"

// (3.2) LONG DOUBLE: 96 bits/ 12 bytes, precision longer than that of a double
// *note that when declaring a long double data type, we must suffix the value of said data type with 'L' to indicate to C++ that it is a long double, otherwise the C++ compiler will automatically chop off a chunk of said long double since the default decimal point data type is the double 
// eg. "long double number4 = 1.12345678901234567890L"

// -----

// (1)-(3) INTEGRAL TYPES
// integral types refer to data types of size less than 4 bytes/ 32 bits 
// eg. CHAR and SHORT INT (2 bytes/ 16 bits)
// ^^ numbers in integral types DON'T support simple arithmetic operations (+,-,*,/,%)
// *note that this same behaviour is observed in the bitwise shift operators (>> and <<)

// ----------

// (5) STRING
// print statements comprise of a 'cout' (console out) and an 'endl' (endline)
// *cout: prints the given statement to the terminal
// *endl: inserts a newline character ('\n') behind a given print statement, meaning it is possible to end a print statement with no 'endl' if you wish to print the next statement on the same line
// ^^ note that we can add '\n' inside of strings to forceably print the statement on a new line, similar to Python

// -----

// (5.1) STRING FUNCTIONS
// use string functions the same way you would with Python
// *google for more C++ string function documentation
// *for all examples below, take eg.'string Strig = "Giraffe"'

// (5.1.0) []
// eg. 'Strig[0]' would return 'G'
// string slicing utilising the index of said CHAR functions the same way as in Python, index starts count from 0

// (5.1.1) .length()
// eg. 'Strig.length()' would return 7
// returns the length of a given string as an INT type

// (5.1.2) .find()
// eg. 'Strig.find('G', 4)' would return 5
// '.find()' can take CHARs or STRINGs as parameters to search for within the given STRING
// ^^ it also takes an index from which to start searching for the given sequence within the STRING
// returns the index of the given CHAR/STRING as an INT type

// (5.1.3) .substr()
// eg. 'Strig.substr(1, 3)' would return "ira"
// the first parameter taken is the starting index from which the function should start its selection
// the second parameter taken is the number of CHARs the function should slice
// returns the sliced STRING/CHAAR depending on the number of characters sliced

// (5.1.4) stoi()
// eg. 'stoi(12)' would return 12, handling TYPE CONVERSION 
// the only parameter taken by the stoi() function is a string data type, which is then type converted to an integer data type
// extremely useful for text parsing, whenever numerical calculations are required

// -----------

// (10) USER INPUT in C++

// (10.1) to take in user inputted CHAR/numbers (INT, FLOAT, DOUBLE), we use "cin"
// *cin: console in, takes in user input that is entered directly into the console
// ^^ note that cin must be followed with ">>" to indicate the inflow of user input taken in by the program (opposite direction of "cout <<")
// user input must be stored within a variable, which is indicated after the cin >> command

// (10.2) to take in user inputted STRINGs, we use the getline() function
// getline() creates an entire line from which we can allocate user inputted strings to
// ^^ the getline() function takes in 2 parameters, "cin" (to indicate that we want to take in user input as one of the function's parameters), and the variable name that we want to store the user inputted string to

// -----------

// (11) ARRAYS
// functions the same as lists in Python, data type declared by the square brackets [], though the actual array is represented with {}
// must statically declare the data type of values and (number of said values (which is optional)) within the array the same way you would a variable, in the format eg. "int aray[5] = {1,2,3,4,5}"
// *an array contains elements of the same type!

// (11.0.1) 2-D ARRAYS
// effectively nested arrays, where an array is a value in another array
// *values can be retrieved in the same was as Python, where the first [] represents the index of the value (an array) within the larger array, and the second [] contains the index of the desired value within the prior chosen array
// ^^ remembering that C++ allows us to statically declare the number of elements within our arrays, this applies for 2-D arrays as well, where we can define the number of arrays within a large array, and the number of elements within each of those arrays, remembering that index of elements starts from 0
// eg. "int numberGrid[3][2]" would indicate that the 2-D array consists of 3 arrays, each of which contain 2 elements

// (11.0.2) SPECIAL characteristic of CHAR arrays
// we are able to directly print out arrays composed of CHARs without creating a for/while loop, since a STRING is simply an array of CHARacters (*note that this behaviour only applies for CHARs and not any other data type that is within an array data structure)
// eg. "char message[5] = {'H','E','L','L','O',\0};
//		cout << message << endl;" will print out the string "Hello" to the console
// *CAVEATS:
// 1. CHAR arrays that want to be printed to the console must be appended with the NULL TERMMINATION CHARACTER (\0) to indicate to cout that we have hit the end of the "String" (array of characters)
// 2. Notice that the NULL TERMINATION CHARACTER (\0) is not counted as a character when determining the size of a CHAR array 

// -----

// (11.1) ARRAY FUNCTIONS
// use array functions the same way you would with Python
// *google for more C++ array function documentation
// *for all examples below, take eg. "int aray[5] = {1,2,3,4,5}"

// (11.1.0) []
// eg 'aray[0]' would return 1
// accessing values within the array can be achieved the same way as in Python, with indexing using [] starting from index 0
// ^^ modifying values within an array can be achieved the same way via the assignment statement

// (11.1.1) [X]
// eg. "int aray[20] = {1,2,3,4,5}" would set the max number of values the array aray could hold as 20
// ^^ similarly, you are able to statically declare an array with a given size first, without assigning any values to store within it

// --------------------

// (1) FUNCTIONS in C++
// functions operate the same way as they do in Python, though C++ categorizes functions depending on their return value
// *"int main()" is an example of the main function which runs automatically upon runtime
// ^^ function parameters must also statically define their data types upon the function's creation, eg. "void sayHi (stringname)"

// (1.0) CALLING functions
// the most direct way to call a C++ function is to run it within the int main() function
// *also see: function stubs

// (1.1) FUNCTION STUBs
// should we want to reference a function in int main() function, we normally would have to declare and create said function above the main function
// ^^ this can be averted through function stubs
// *FUNCTION STUB: statically declaring a function and its parameter's data type above the main function first, and leaving the function's contents below the main function
// ^^ this allows for C++ to reference your function prior to you assigning it any value

// (1.1) VOID FUNCTIONS
// *"void" functions DO NOT RETURN any value, and only run code within said function
// eg. "void sayHi () { function components }"

// (1.2) RETURN type
// most functions in C++ barring void functions will return a value (INT, FLOAT, DOUBLE, CHAR, STRING, BOOL, ARRAY)
// ^^ the data type of the RETURN value of a function must be statically defined upon declaration, in the same way the void function is defined
// *note that even the main function "int main()" statically defines its return value of 0 (an integer) upon its declaration
// ^^ return statements at the END of every function operates the same way as in Python, where you must include a "return <variable>" statement at the end of said function, and the function will break upon hitting the return statement, ignoring all code after the return statement

// ----------

// (2) CONDIITONAL OPERATERS in C++
// used to define logic in C++, operates similarly as in Python

// (2.1) if
// if statements function similarly as in Python, though notice the syntax differences in how conditional statements are implemented
// eg."if (input given condition to check here) {}"

// (2.2) else
// else statements function similarly as in Python
// eg. "else {}

// (2.3) else if
// else if statements function similarly to elif in Python
// eg. "else if (input given condition to check given the above if/else if statements have yielded false) {}"

// (2.4) &&
// double ampersands represent the AND operator
// functions exactly the same as in Python, used to check whether multiple separate conditions fulfill the same criteria

// (2.5) ||
// double straight lines represent the OR operator
// functions exactly the same as in Python, used to check whether at least one of the conditions fulfills a given criteria

// (2.6) !
// exclamation mark represents the NEGATION (NOT EQUAL TO) operator
// functions the same as in Python, negating the condition that the negation operator is placed before
// eg. in Python, "!=" represents not equals to

// (2.7) > and >=
// relational operators functions the same way as in Python, bigger than and bigger than or equal to

// (2.8) < and <=
// relational operators functions the same way as in Python, smaller than and smaller than or equal to

// (2.9) == and !=
// relational operators functions the same way as in Python, equal and not equal to

// (2.10) switch()
// switch() statements obey the same syntax as if, else and else if statements in C++, and are used to simplify multiple if else statements
// ^^ switch() takes in a single parameter, the variable with a varying value
// *within switch(), "case" is a reserved word used to prefix the varying value you would like to compare, and below "case", normal conditional logic applies
// *within switch(), "default" is another reserved word used at the END of the switch statement to prefix what the program should run, should none of the above cases return true

// eg. " switch (dayNum) {
//       case 0: {
//          output = "Smells";
//          }
//          break;
//       case 1: {
//          output = "Smells2";
//          }
//          break;
//       etc...
//       default: {
//          dayname = "Invalid input";
//          }
//          }"
//
// (2.11) TERNARY OPERATOR
// ternary expressions can simplify complex conditional logic, resulting in terse C++ code
// for further documentation, read more at (https://cplusplus.com/articles/1AUq5Di1/) 
//
// TERNARY EXPRESSION
// eg. "result = (condition) ? option1 : option2;"
//
// EQUIVALENT IN NORMAL C++
// eg. "if (condition) {
//			result = option1;
//			} else {
//			result = option2;
//			}"
//
// *note that ternary expressions can also be used to initialise variables and their values, as seen below
//
// eg. "bool fast = false
//		int speed {fast ? 300 : 150};"
//
// ^^ this essentially means if fast == true, int speed = 300, else int speed = 150
// ----------

// (3) LOOPS
// conceptually, C++ loops operate the same way as in Python, and we mainly deal with the bottom 4types 
// 1. FOR loops (eg. "for (int i=0; i<20; i++)")
// 2. Range-based FOR loops (eg. "for (int i: array)")
// 3. WHILE loops (eg. "while (int x < 20)")
// 4. DO WHILE loops (eg. "do {rest of code} while (int x < 20)")
// 
// *for additional understanding, read up on the "size_t" unsigned integral data type at (https://cplusplus.com/reference/cstring/size_t/)

// (3.0) break
// break operates the same way as in Python, breaking the program out of the current loop/ iteration upon execution

// -----

// (3.1.1) WHILE loops
// functions similarly to while loops in Python, where the program continuously loops while a certain condition is met, and as such, while loops have the potential to become infinite loops if you're not careful (eg. "while (true) {rest of code}" will endlessly run and can only be ended by a braek statement)
// syntax is as follows, eg. "while (given condition) {rest of code}"

// (3.1.2) DO WHILE loops
// essentially a REVERSED while loop, where code is first run, and the while condition of the while loop is checked for at the end of said code being run
// syntax is as follows, eg. "do {rest of code} while (given condition)"
// *note that conventional while loops can accomplish anything that do while loops can

// -----

// (3.2) FOR loops
// functions somewhat similarly to for loops in Python, where the program iterates over multiple variables in an array from beginning to end, though C++ provides added flexibility in the way in which the itervar is modified across multiple iterations
// *google documentation for current C++ version's documentation on 'for' loop implementation, especially for string parsing purposes
// syntax is as follows, eg. "for (<itervar (specify itervar data type)>; <looping guard of when to break the loop>; <condition to change at the end of each loop>) {rest of code}"
// eg. "for (int i = 1; i <= 5; i++) {
//           cout << i << endl;       
//      }"

// (3.2.0) Range-based FOR loops
// *caveat: this is another way to iterate through for loops in C++
// eg. "vector <int> nums = {1,2,3,4,5};
//      for (int i: nums) {
//          cout << nums << endl;
//      }"
// this above method is much more similar to the for loops we are familiar with in Python, and in actuality, is easier to implement when looping through a set number of variables or across a known data structure
// *syntax wise, this C++ for loop of "for (<data type of itervar> <itervar> : <data structure to iterate through>)" is the equivelant of "for i in nums:" in Python
// ^^ note that the above iteration variable can be of the Strings, Chars, Int, Floats, Doubles etc.

// (3.2.1) ARRAY ITERATION
// applicable for both conventional arrays as well as iterating through a string (array of CHAR), we use the indexing capabilites of arrays to obtain each value in said array
// eg. "int nums[5] = {1,2,3,4,5};
//      for (int i=0; i < 5; i++) {
//          cout << nums[i] << endl;
//      }"

// (3.2.2) NESTED FOR LOOPS
// the logic behind C++ nested for loops is identical to that behind Python, though the syntax somewhat varies due to C++'s more robust for loop capabilities
// eg. "for (int i = 0; i < 3; i++) {
//          for (int q = 0; q < 2; q++) {
//              cout << nums[i][q];
//    }
//    cout << endl;
// }"

// ----------

// (4) POINTERS in C++
// all variables and values take up physical, tangible memory on your computer's RAM, and have a fixed memory address to uniquely identify said piece of data
// *POINTER: the memory address of a given value that exists within your program 
// documentation: https://youtu.be/2ybLD6_2gKM, https://youtu.be/q24-QTbKQS8

// (4.1) &<variable name> 
// eg. "cout << &Age;" will return 0x6afee0 (hexadecimal number indicating fixed memory address of Age value)
// ^^ used to access the POINTER of a stored value
// an ampersand placed in front of a variable indicates to C++ that you want to print out the memory address (pointer) where the value of the age variable is stored

// (4.2) <variable data type> *p<variable name> = &<variable name>
// eg. "int Age = 19;
//      int *pAge" = &Age;
//      cout << pAge;" will store the pointer value of Age within the pAge variable and print out said Age pointer value using "cout << pAge;"
// ^^ CREATES a POINTER VARIABLE (which assigns a pointer [memory address] to a variable)
// *note that the data type of the pointer variable must be the same as the variable it is pointing to (ie. applies for INT, FLOAT, DOUBLE, CHAR, STRING, BOOL)
// *also note that the aestricks dereferencer "*" prefixing the p<variable name> is a reserved keyword, and is only to be used in the assignment statement. As such, printing out the pointer value would be as follows, eg. "cout << pAge;"

// (4.3) *<pointer of a given variable>
// eg. "cout << *pAge;" will return 19
// ^^ DEREFERENCES a POINTER () and returns the value that was stored at the given pointer's memory address
// effectively the reverse of "&", which allows us to access the pointer of a given value, since "*" derefences a pointer value, allowing us to access the original value stored at said pointer location

// --------------------

// C++ OBJECT ORIENTED PROGRAMMING 
// fundamental concepts underlying OOP operate the same way as in Python, though the syntax might be slightly different
// class objects, instance variables, instance attributes, methods, and object inheritance all operate the same way, although there are some restrictions to be noted
// classes allows us to create and use different data types outside of the standard 6 from C++

// (1) creating a CLASS
// *note that the convention for creating classes is to capitalize them upon declaration, eg. "Book" and not "book"
// ^^ in contrast to Python, there is no need to declare "self" with () when creating a class, statically declaring instance variables and their type is sufficient

// (1.1) PUBLIC 
// public scope in C++ operates the same way as global in Python
// *any code, instance attributes, variables or values placed under the "public" scope in C++ classes can be accessed outside of the given C++ class from the public scope (public instance attributes are often a neccesity when working with OOP since we need to access instance attributes of multiple different objects at a given time)

// (1.2) PRIVATE
// private scope in C++ means that only code within the given C++ class is able to access and manipulate said private values
// *any code, instance attributes, variables or values placed under the "private" scope in C++ classes can ONLY be accessed from within the given C++ class, and cannot be referenced outside of said class from the public scope

class Book {

    private:
        double finrating;
        // this private instance attribute cannot be accessed from the public scope, and can only be accessed and referenced from within the Book class

    public:
        string title;
        string author;
        int pagenum;
        float bookprice;
        // these are all instance variables/attributes of the Book class
        
        Book(float bookprice, double rating) {
            cout << "Book has been instantiated";
            bookprice = bookprice;
            setrating(rating);
            // CONSTRUCTOR function which takes in a single parameter, 'bookprice' of float data type, and this constructor function returns the inputted parameter to the public variable bookprice, an instance attribute of the Book data class
    }
        
        bool bookpriceChecker() {
            if (bookprice >= 50.0) {
                return true;
            }
            return false;
            // CLASS METHOD of boolpriceChecker() which can be called for every instance object created off the Book class
        }

        void setrating (double rating) {
            if (rating == 1.0 || rating == 2.0 || rating == 3.0) {
                finrating = rating;
                cout << "Rating succesfully edited to " << finrating << endl;
                // validation of a given inputted rating where input is accepted
            } else {
            cout << "Invalid rating inputted. Please input 1.0, 2.0 or 3.0.";
            finrating = 0;
            // in the case of an invalid input
            }
            
        }
        // by funneling all attempts of modifying the rating through the setrating() function, we are able to implement logic to check and restrict certain input


        double getrating () {
            return finrating;
        }
};

// (2) instantiating an INSTANCE OBJECT
// instantiating instance objects functions the same way as in Python, though C++ makes said instantiation feel more natural due to the already practiced static type declaration
// *note that modifying instance attributes of a given object operate the same way as in Python, by following the object name with a ".<instance variable>"

void tempstorage () {
    // temporary function to demonstrate creation of a class object outside of the main function above
    Book book1(50.1, 1.2);
    book1.title = "Harry Pooper";
    book1.author = "JK shitface";
    book1.pagenum = 500;
    // modifying instance attributes of the book1 object

    cout << book1.bookprice;
    cout << book1.bookpriceChecker();

    book1.setrating (19.899);
    cout << book1.getrating ();

}

// (3) CONSTRUCTOR 
// a class method that is automatically called whenever an instance object is first instantiated, the equivalent of "def __init__ (self):" in Python
// **for C++, the constructor function is specified with the Class name, formatted as a class method
// ^^ as seen above, the class Book's constructor function is defined by "Book() {method contents}"
// *just like any other class method, the constructor function can take parameters which it can then assign to instance attributes OR can use within class methods, though note that we must statically declare data types of all the parameters taken in
// ^^ additonally, note that C++ is particular about which instance attributes are public and which are private, so we oftentimes have to assign and statically declare the data type of a public instance attribute, before we can receive it as a returned value from a constructor function

// (4) CLASS METHODS
// *functions that a given class can run, defined in the class declaration, operates similarly to class methods in Python, though the syntax is somewhat different
// eg. "turn_page (parameters to be inputted into said method if desired) {method contents}"
// ^^ note that for all class methods, the data types of any parameters taken in by the method, as well as the return type of the method itself, must be statically declared
// ^^ additonally, note that C++ is particular about which instance attributes are public and which are private, so we oftentimes have to assign and statically declare the data type of a public instance attribute, before we can receive it as a returned value from a class method
// void functions can be run as class methods as well!

// (5) GETTERS and SETTERS
// setting restrictions on parameters that CONSTRUCTORs and CLASS METHODs can receive, achieved through the use of the public and private scope
// ^^ seen above in line 410 and 422

// (6) OBJECT INHERITANCE
// where a child class is able to inherit all the ATTRIBUTES and METHODS of a parent class, while modifying certain attributes and methods as desired and adding new ones
// eg. "class <child class> : public <parent class>" when declaring the child class will allow for the child class to inherit all attributes and methods of the parent class
// ^^ to modify and redefine parent class methods or attributes in the CHILD class, simply reassign said attribute or method in the child class, under the public scope
// *worked example of parent and child class with object inheritance seen below 

// PARENT class
class Chef {
    public:
        void makeChicken() {
            cout << "Chef made chicken" << endl;
        }

        void makeSalad() {
            cout << "Chef made salad" << endl;
        }

        void makeSpecialDish() {
            cout << "Chef made special dish" << endl;
        }
};

// CHILD class
class ItalianChef: public Chef {
    // here, the child class has now inherited all the functionality (attributes and methods) of the parent class

    public:
        void makePasta() {
            cout << "Italian chef made pasta" << endl;
        }
    // additionally, the child class is able to expand on its parent class functionality with additional attributes and methods 

        void makeSpecialDish() {
            cout << "Chef made pee pee poo poo" << endl;
        }
    // here, the child class redefines and overrides the makeSpecialDish() method that was initially defined in the parent class, though the child class will continue to inherit the rest of its attributes and methods from the parent class defined in its declaration statement
};


void tempstorage2() {
    // temporary function to demonstrate creation of a class object outside of the main function above

    Chef chef1;
    chef1.makeChicken();
    ItalianChef italianchef1;
    italianchef1.makeChicken();
    italianchef1.makePasta();
    chef1.makeSpecialDish();
    italianchef1.makeSpecialDish();

}

// --------------------

// FILE HANDLING in C++
// (1) #include <fstream> library to allow for file reading and writing (in the same way we #include <iostream> to handle console input and output)  
// (2) fstream fhand; declare a variable (conventionally "fhand" for file handler) of the 'fstream' data type

// -----

// WRITING to a file [ios::out]
// (3.1) fhand.open("input.txt", ios::out); the .open() function takes in 2 parameters, the name of your file, and read, WRITE or append mode
// ^^ since this is write mode, the mode is set to output with "ios::out"
// *note that if the file does not exist, it will be created when opened in WRITE mode

// (3.1.1) .is_open() 
// eg. "if (fhand.is_open()) {};"
// used to determine whether a file has been succesfully opened, can accompany either writing, reading or appending to a text file

// actually WRITING to the file
// (3.1.2) fhand << "Hello World/n";
//         fhand << "Yes ok";
// ^^ notice that the syntax to write to a file is very similar to printing to the console with cout

// -----

// OVERWRITING file data [ios::app]
// (3.1.3) fhand.open("input.txt", ios::app); the .open() function takes in 2 parameters, the name of your file, and read, write or APPEND mode
// should you wish to override all previous data by writing to a file in a new instance, you would set the mode to "ios::out"
// should you wish to APPEND additional data to said file in a new instance, you would set the mode to "ios::app"
// ^^ appending data to the file is done the same way as writing to the file, with the "fhand << <desired text>"

// -----

// READING a file [ios::in]
// (3.2) fhand.open("input.txt", ios::in); the .open() function takes in 2 parameters, the name of your file, and READ, write or append mode
// ^^ since this is read mode, the mode is set to input with "ios::in"

// (3.2.1) .is_open() 
// eg. "if (fhand.is_open()) {};"
// used to determine whether a file has been succesfully opened, can accompany either writing, reading or appending to a text file

// Parsing data from a file
// (3.2.2) string line;
//         while (getline(fhand, line)) {
//                cout << line << endl;
//               }
// *a WHILE LOOP is used to allow for C++ to read through every single line of text within fhand, storing each line from fhand within the string variable
// ^^ the getline() function takes in 2 parameters, the file object (fhand) and the string variable to store each individual line it reads from the file object
// note that since C++ is a statically typed language, we must declare a string variable to store each line of text we want to read and manipulate

// -----

// CLOSING a file
// (3.3) fhand.close(); closes a file once you are done reading/writing/appending to it

// --------------------

// OUTPUT FORMATTING in C++
// refers to different functions in C++ that change the format of text printed to the console
// ^^ further documentation can be found at (https://en.cppreference.com/w/cpp/io/manip)

// (1.1) std::flush
// eg. 'cout << std::flush' will print any code executed in the program to the terminal immediately
// when we cout (console out) a statement in C++, it is temporarily stored in the output buffer, and only when the output buffer is full does it send the stored text to the terminal
// *"std:flush" tells C++ we would like to bypass the 'storing' capability of the output buffer, and immediately print what is inside the output buffer to the terminal

// (1.2) std::setw()
// eg. 'cout << std::setw(10) << "test text"' prints out text comprised of empty spaces, followed by the words "test text" to the terminal
// *"std:setw(X)" is used to set a fixed width between text that is printed out to the console, with X being the number used to determine the width to be inserted between elements
// ^^ with this function, we can set text in the terminal to be Right justified/ Internally justified

// (1.3) std::setfill()
// eg.'std::setfill(-)' will fill any empty spaces in the terminal with '-'
// *"std:setfill(X)" is relatively self-explanatory, and is used to fill a determined amount of space with the chosen CHAR, 'X'

// (1.4) std::boolalpha
// eg. declaring 'cout << std::boolalpha;' will cause 'cout << boolean_condition' to return true/false
// *"std:boolalpha" forces C++ to return the text-version of Boolean values (true, false) instead of 1 and 0 when printing boolean values to the terminal

// (1.5) std::showpos and std::noshowpos
// eg. 'int pos_num1 = 34; 
//      cout << std::showpos;
//      cout << pos_num1' will return +34, and swapping the command for 'cout << std::noshowpos' will return 34
// *"std:showpos" forces C++ to print out the otherwise hidden '+' positive sign in front of positive integers
// *"std:noshowpos" forces C++ to hide the '+' positive sign in front of positive integers

// (1.6) std::dec, std::oct, std::hex
// eg. 'int pos_num2 = 717171;
//      cout << std::dec << pos_num2;  will return 717171
//      cout << std::hex << pos_num2;  will return af173
//      cout << std::oct << pos_num2;' will return 2570563
// these manipulators format how numbers are represented according to different number systems (decimal, octo, hexadecimal)

// (1.7) std::showbase
// eg. 'int pos_num2 = 717171;
//      cout << std::showbase;
//      cout << std::dec << pos_num2;  will return 717171
//      cout << std::hex << pos_num2;  will return OXaf173
//      cout << std::oct << pos_num2;' will return 02570563
// *note that the "std::showbase" formatter alters how the same value is printed out in different number systems, by making it obvious which version is octo and hexadecimal as compared to the conventional decimal

// (1.8) std::uppercase
// related to (1.6) and (1.7), "std::uppercase" simply formats the letters in the hex and oct representations of the number to be uppercase, returning OXAF173 for the hexadecimal representation and 02570563 for the octo representation
// ^^ this tends to be useful when working with applications which dictate specific formats for number inputs

// (1.9) std::scientific and std::fixed
// *controls how floating point data (float, double, long double) is represented in the terminal
// ^^ by default, "cout << <floating point number>" will print out said float/double/long double in scientific notation, but including the line "cout << std::fixed" prior to printing out floating point numbers will format them with fixed notation instead

// (1.10) std::setprecision()
// *"std::setprecision(X)" will format all floating point numbers printed to the terminal according to the specified precision of value X

// (1.11) std::showpoint
// including this formatter prior to printing your numbers will format all numbers according to their statically declared data types, and show the decimal point and trailing zeros if required for their data type

// --------------------

// MATH FUNCTIONS in C++
// (1) #include <cmath> to include all the math functions which reside within said library
// *for further documentation on all functions, read (https://cplusplus.com/reference/cmath/?kw=cmath)

// --------------------
