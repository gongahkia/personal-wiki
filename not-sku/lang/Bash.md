# `Bash`

Bash is a ***dynamic interpreted*** programming language, and is also known as a **shell** (which wraps the operating system's kernel), whilst allowing for user interaction via abstraction through the command line.

*(Most likely though, you want to learn Bash so you can further pimp out your Arch-Neovim-Tmux setup. More power to you of course.)* 🫡

![](https://miro.medium.com/max/800/1*avk37KYCRMe8HRYcgdMJUA.jpeg)

----------

### Creating and running a Bash file

Bash files are indicated by suffixing the file name with the `.sh` file format

Note that the Bash `.sh` file will not be able to run due to its permissions (checked using the `ls -al` command)
* to ***change*** the permissions and make it a Bash ***executable***, we run `chmod +x {file name}`
* to run the Bash executable file, we run `./{file name}` (similar to C and C++)
* note that *once* the Bash executable file has been created, **any** changes made to the Bash `.sh` file will be immediately reflected, and there is **no need** for any 'recompiling' (unlike in C and C++)

----------

### Printing to the console

* printing to the console in Bash is handled by the `echo` command

```bash
echo "hello world!"
```

* formatted strings can be easily printed by prefixing the variable with a `$` dollarsign character (which is standard when referencing a variable anywhere outside of assignment in Bash)
```bash
num_buffalo_wings = 10
echo "here's your order: $num_buffalo_wings buffallo wings"
```

* note that variables alone can be printed directly using `echo` without need for the double quote marks

```bash
num_times_i_tried = 2
echo $num_times_i_tried
```

----------

### If && Elif && Else

* `if`, `elif` and `else` function the same in Bash as they do in any other programming language
* unlike in C++, C, Javascript and nearly any other language, the condition is surrounded by the `[]` square brackets

```bash
peepee = 10

if [$peepee -eq 10] 
then
    echo "peepee is equal to 10!"
else
    echo "peepee is not equal to 10!"
fi
```

As you will no doubt quickly realize from the above code, there are many differences in Bash's syntax as compared to curly braced languages, as follows:

* **condition scope** is denoted by indentation, and not `{}` curly braces
* the `if`/`elif`/`else` **conditional statements** are followed by `then`, before the rest of the enclosed code block
* the **code block** is closed with the `fi` finished statement (since Bash lacks curly braced indentation, and has to rely on *closing statements* to indicate scope)

and...

* ***equality*** is indicated by the `-eq` flag (equal) [or with the `==` equality operator]
* ***inequality*** is indicated by the `-ne` flag (not equal) [or with the `!=` inequality operator]
* ***value comparison*** is indicated with the `-gt` (greater than), `-lt` (less than), `-ge` (greater than or equal), `-le` (less than or equal) flags [or with the `>`,`<`,`<=`,`>=` operators, which are available]
* ***variables*** are referenced with the `$` dollarsign character in front of them (similar to *PHP*)

```bash
poopoo = 1600

if [$poopoo -ne 1500]
then 
    echo "aigh bet"
else
    echo "aight beghhh"
fi
```

### Case statements

* `case` statements are used to simplify multiple `if`, `elif`, `else` statements into a neater, more readable format, and are the equivalent of *switch statements* from other programming languages
* syntax-wise, `case` and `in` surround the given variable to be checked (such as *car*), and each condition is suffixed by the `)` close bracket character
* `;;` double semi-colons are used to indicate a ***break statement*** in `case` statements in Bash, and are used below to break out of the `case` statement should a given condition be found to be true (note that `break` statements **do** exist as their own thing in Bash)
* `*` an aestricks is used to indicate the **default** statement should all the other conditions be false

```bash
car = 10

case $car in

    "BMW" )
        echo "your kar is a BMW";;

    "Toyota" )
        echo "aigh bet";;

    "Honda" )
        echo "your car is a Hoonda";;

    "Toyota" )
        echo "car ni na";;

    * )
        echo "this is the default case";;
esac
```

* notice in the code above, that whenever referring to a variable in a statement, we prefix it with the `$` dollarsign character
* note that `case` statements **must** be ended with `esac`, which indicates to Bash that the `case` statement is done (the equivalent of `fi` for `if`/`elif`/`else` statements)

----------

### && and || and !

All the following logical operators work the same way in Bash as in any other programming language

* `&&`/`-a` is the **and** operator
* `||`/`-o` is the **or** operator
* `!` is the **not** operator

----------

### While loops

* `while` loops in Bash function the same way as in any other programming language, where the code chunk will run *while* the given condition is ***true*** and halt once the given condition is ***false*** (think `while true` in other languages)
* note that `do`, which is often *omitted* in other programming languages, is included in Bash, thus creating an explicit `while do` loop 

```bash
number = 1
while [ $number -lt 10 ]
do
    echo "$number"
    number = $((number+1))
done
```

* note that `done` is included at the end of the `while` loop to indicate the end of the code chunk (similar to how `fi` is used in **conditional** statements and how `esac` is used in **case** statements as explored earlier)

### Until loops

* `until` loops in Bash function nearly the same as `while` loops, except that the code chunk will run *while* the given condition is ***false*** and halt once the given condition is ***true*** (think `until true` in other languages)
* note that `do` and `done` are included in the exact same way as in the `while` loop

```bash
number = 1
until [ $number -ge 10 ]
do
    echo $number
    number $((number+1))
done
```

### For loops

* `for` loops in Bash function the same way as in any other programming language, though the syntax is somewhat bulkier
* note that `do` and `done` are still included, in the same way as in the `while` and `until` loops
* for reference, the code below would print out *"1","2","3","4","5"* to the console

```bash
for i in 1 2 3 4 5
do
    echo $i
done
```

* Bash allows the standard `for` loop to be expressed in a more compressed format, as seen in the code below
* including three numbers, separated by `..` two periods inside `{}` curly braces will indicate to Bash that you want to create a sequence of numbers, starting from the **first** number, ending at the **second number**, with an increment of the **third** number each time (in the format of `{start..ending..increment}`)
* for reference, the code below would print out *"0","2","4","6","8","10","12","14","16"* to the console

```bash
for i in {0..20..2}
do
    echo $i
done
```

* additionally, Bash allows for the conventional `for` loop we are more familiar with (as seen in languages like C++, C, Javascript and Rust)
* notice that for this type of `for` loop, the conditions are placed inside of `()` normal brackets, instead of the `[]` square brackets for `while` and `until` loops
* for reference, the code below would print out *"0","1","2","3","4"* to the console

```bash
for (( i=0; i<5; i++ )) 
do
    echo $i
done
```

----------

### Break and Continue

* `break` and `continue` statements in Bash function the same way as in any other programming language
* for reference, the code below would print out *"10","9","8","7","6","5","4","3","2","1"* to the console

```bash
for (( i=10; i<=0; i-- )) 
do
    if [$i -gt -100]
    then
        break
    fi
    
    elif [$i -eq 200]
    then
        continue
    fi

    echo $i 
done
```

----------

### Comments 

* **single-line** comments are prefixed with the `#` hash character
* **multi-line** comments are surrounded by the `: '` colon, space, single quote characters and the `'` single quote character

```bash
#here is a comment in Bash

: '
here 
is 
a 
multi-line
comment
in
Bash'
```

----------

### Reading User Input

Since Bash is a scripting language, it handles user input slightly differently as compared to conventional programming languages

* the closest we get to a conventional asking for user input *reserved word*, like `input("Enter a number:)` in Python or `cin << user_input << endl` in C++ is with the `read` reserved word in Bash

```bash
echo "Enter a number: "
read st1
```

* as seen above, `read` will allow for the user to enter an input, and will then **read** said input into the variable placed after the `read` in the program

However, Bash also allows for user input to be taken in *alongside* the Bash executable file name, as opposed to being explicitely entered in as a step run in the program.

Let the following code be in the file *test.sh*:
```bash
echo $arg1 $arg2 $arg3 
```

* should the user run the above code in the format *"test.sh shit ass uncle"*, the program will return *"shit ass uncle"*
* as previously established with the use of the `$` dollarsign character (in referencing a variable outside of its assignment and initialization), including a variable alongside the file executable will assign the given arguments to the variables in sequential order

```bash
args = ("$@")
args_len = ${#args[*]}

for ((q=0; q<args_len; q++))
do
    echo ${args[$q]}
done
```

* Bash also allows for taking in an **undetermined number** inputs using the `@` at character
* as seen above, we use the `@` at character to receive an undetermined number of inputs, which we assign to the **array** *args*, and then print out every element within the array to the console using a `for` loop
* for an explanation regarding [the method](https://stackoverflow.com/questions/1886374/how-to-find-the-length-of-an-array-in-shell) used to derive length of the array

```bash
args = ("$@")

echo $@
echo $#
```

* however, as seen in the above code, the same effect can be achieved by printing out all the inputs (an **undetermined number** of inputs) taken into the array using the `@` at character
* note that the `#` hash character will return the **length** of the array, with `echo $#` printing out the number of elements within the given array to the console *(this will be covered in greater depth under the **Arrays** section later)*

----------

### String manipulation

#### String comparison

* the below code is an example of combining the `read` function, the `==` equality operator, which were covered previously, and the conditional `if` and `else` statements, to check if two strings are **equal**

```bash
echo "enter one string: "
read string1

echo "enter another string: "
read string2

if [ "$string1" == "$string2" ]
then
    echo "the strings are identical"
else
    echo "the strings are not identical"
fi
```

* the below code is an example of how one would check whether two strings are **longer than each other**, or if they are **equal**, making use of the newly introduced `/>` and `/<` comparison operators

```bash
echo "enter string 1: "
read str1

echo "enter string 2: "
read str2

if ["$str1" \< "$str2" ]
then 
    echo "string 2 is larger than string 1"
elif ["$str1" \> "$str2" ]
then
    echo "string 1 is larger than string 2"
else
    echo "string 1 is equal to string 2"
fi
```

#### String concatenation

* string concatenation in Bash is achieved by simply placing the two string variables next to each other (note there is **no need** for the `+` addition operator as in other languages like Python)

```bash
string1 = "good morning people, "
string2 = "my life is new page, my, yootube channel"
c = $string1$string2
echo $c
```

#### Lowercase and Uppercase

* to convert our entire string to **lowercase** characters, we use the `^` single carrot operator inside of `{}` curly braces
* to convert our entire string to **uppercase** characters, we use the `^^` double carrot operators inside fo `{}` curly braces

```bash
string1 = "AIGHT BET"
string2 = "aight bet"

echo ${string1^}
echo ${string2^^}
```

* to **capitalize** or **uncapitalize** only the first letter of the string, we mention said letter in the format of `${string1^h}` or `${string2^^h}` assuming the string is "Hello world!"

----------

### Numbers and Arithmetic operations

All arithmetic operations used in other programming languages are valid in Bash:

`+` : **addition** operator  
`-` : **subtraction** operator  
`*` : **multiplication** operator  
`/` : **division** operator  
`%` : **modulo** operator  

* note that arithmetic operations in bash must be conducted within two sets of normal `()` brackets if the calculation is done directly in the `echo` statement

```bash
num1 = 4
num2 = 20
echo $(( num1 + num2 ))
```

----------

### Arrays

* the array data structure in Bash is surrounded by `()`, and **spaces** must seperate each value in an array (not commas)

#### [ ]

* to access *individual* values in an array by their **index**, we use `[]` square brackets (similar to Python, C and C++) 
* additionally, adding new values to the array uses the same syntax, with an assignment statement of `{array name}[index of new element] = "{new element}"`

```bash
cars = ("BMW" "Toyota" "Honda")
echo "${cars[1]}" #this would print out Toyota, the element within the array with an index of 1
cars[3] = "Tesla" #this would add the element of value "Tesla" to the cars array at index 3
```

#### @

* as previously mentioned, we use the `@` at character to access **all the elements** within the array 

```bash
cars = ("BMW" "Toyota" "Honda")
echo "${cars[@]}" #this would print out all the elements within the array
```

#### \#

* as previously mentioned, the `#` hash character returns the number of values within said array (**length of the array**)

```bash
cars = ("BMW" "Toyota" "Honda")
echo "${#cars[@]}" #this would print out the number 3
```

#### unset 

* to modify the array by **removing/deleting** values, we use `unset` in the format `unset {array name}[index of element to be removed]`

```bash
cars = ("BMW" "Toyota" "Honda")
unset cars[1] #removes the element within the array of index 1, "Toyota"
echo "${cars[@]}" #this would print out all elements within the new array, "BMW Honda", as "Toyota" has been removed from the array
```

----------

### Functions

* Bash functions operate the same way as functions in any other programming language (creating and calling a function)
* note that we prefix our function name with `function`, and the code is enclosed within `{}` curly braces

```bash
function test_function() 
{
    echo "This is a new function"
}
```

* this works the same way for functions that take **arguments**, although Bash allows for function calls to take arguments alongside it *(in the format `{function name} {arg 1} {arg 2} {arg 3} {arg 4}`)*
* the code below allows the *test_function()* function to take in 4 arguments alongside the call, which it will assign sequentially to the variables 1 to 4

```bash
function test_function()
{
    echo "Hello $1, $2, $3, $3"
}
```

----------

### Writing to local files

* writing to a file in Bash is *startlingly braindead*
* to write/ create a new local file, we use the `>` single angle bracket operator to indicate the file we want to write data to

```bash
echo "hello world!" > output.txt
```

The above code will write the text *"hello world"* to the *output.txt* file if it exists, and create a new file if it doesn't exist, writing *"hello world"* to it

* to take in user input via the command line and write it to a given file, we use the `cat` operator and combine it with the `>` single angle bracket operator 

```bash
cat > file.txt
```

![](https://media.tenor.com/tq67NQA8FD4AAAAC/cat-bash-cat.gif)

### Appending to local files

* to write **additional data** to the file on top of existing data, we use the `>>` double angle bracket operator in the same way as above

```bash 
echo "hello again world!" >> output.text
cat >> file.text
```

----------

### HereDoc delimiter

* the heredoc delimiter, indicated by `<<` double reverse angle brackets, effectively prints a given text to the console until the delimiter word is arrived at, after which the program breaks

```bash
cat << kreativ
this is hello creative text, add another line, and kreativ
```

The above code has set *"kreativ"* as the **delimiter**, and will thus print out the text *"this is hello creative text, add another line, and"* to the console

----------

### Streams

![](https://cdn.arstechnica.net/wp-content/uploads/2015/11/bob-ross-action.jpg)

To provide a bit of [context](https://riptutorial.com/bash/example/7602/stdin--stdout-and-stderr-explained) prior to this dive into *streams*, here is a breakdown of the terminology:

1. `stdin` : standard **input**
2. `stdout` : standard **output**
3. `stderr` : standard **error**

*(Note that for the scope of this introduction, we will be talking about `stdin`, `stdout` and `stderr` in the context of command line scripting)*

Every command in Bash has ***ONE*** input (`stdin`), and ***TWO*** possible outputs (`stdout` and `stderr`). We refer to these 3 as the **standard streams**.

1. `stdin`
* Generally, ***standard input*** is what the programmer/user uses to provide input to a program. It is what we often refer to as 'user input'.
* in the code below, we use the `read` builtin function to read a line from the ***standard input*** stream

```bash
gongahkia@PC:~/desktop/coding$ read
Type some text here
```

2. `stdout` 
* ***standard output*** is what is received as output in the console in response from a *command* entered by the programmer/user
* in the code below, the command `ls` to list all the files in the current file directory will *return* the names of the files within the current file directory, which are sent to the ***standard output*** stream

```bash
gongahkia@PC:~/desktop/coding$ ls 
file
```

3. `stderr`
* ***standard error*** is what is received as **error messages** to the console in response from a *command* entered by the programmer/user which resulted in an error 
* in the code below, since there is no file directory or file titled *anotherfile*, an **error message** is received by the ***standard error*** stream

```bash
gongahkia@PC:~/desktop/coding$ ls anotherfile
ls: cannot access 'anotherfile': No such file or directory
```

In Bash, these 3 standard streams are referred to by **number** *(as seen below)*, and while `stdin` is *bound* to the keyboard, we can *redirect* `stdout` and `stderr` (which normally appear in the terminal) to wherever we need.

0 = ***Standard input***  
1 = ***Standard output***   
2 = ***Standard error*** 

* in the code below, we only want to see output coming from the ***standard output*** stream, and don't want to see any error messages. As such, we *redirect* the `stderr` ***standard error*** stream to */dev/null*

```bash
gongahkia@PC:~/desktop/coding$ ls anotherfile 2>/dev/null
gongahkia@PC:~/$
```

----------

### stdin (0)

* `stdin` is the [standard input](https://www.tutorialspoint.com/understanding-stdin-stderr-and-stdout-in-linux#:~:text=stdin%20%E2%88%92%20It%20stands%20for%20standard,It%20stands%20for%20standard%20error.)
* to read a file within the same file directory as our Bash executable, we run the command in the format of `{bash executable file name} {name of file to be read}`, with the contents of said Bash executable seen below
* for now, just know that we use **stdin** to read the contents of a file within our local standard directory *(don't worry, I'm starting to get confused as well. This is part of a much larger discussion regarding [streams](https://www.middlewareinventory.com/blog/linux-stdout-stderr-bash/) and such, and I will come back and update this section when I have a better grasp of that.)* 😔

```bash
while read line
do
    echo "$line"
done < "${1:-/dev/stdin}"
```

### stdout (1) && stderr (2)

* `stdout` is the [standard output](https://www.computerhope.com/jargon/s/stdout.htm)
* `stderr` is the [standard error](https://www.tutorialspoint.com/understanding-stdin-stderr-and-stdout-in-linux)
* as previously established, *1* is for `stdout` and *2* is for `stderr`, and as seen in the code below, we ***redirect*** the `stdout` from the command **ls -al** to *file1.txt* and send any `stderr` to *file2.txt*

```bash
ls -al 1>file1.txt 2>file2.txt
```

----------

### Sending output from one script to another

* to export a variable/value from one Bash executable to another, we use the `export` function, which exports said variable to **all** other Bash files in the same file directory (essentially the equivalent of importing functions from other files in Python)
* seen below, we prefix Bash executable names with `./` (*exportedscript.sh* in the example below) when mentioning them within other Bash files, when we want to **run the referenced Bash file** from within our program

Code within *helloscript.sh*:
```bash
message = "good morning people"
export message
./exportedscript.sh
```

Code within *exportedscript.sh*:
```bash
echo "The message from helloscript is: $message"
```

* in the above example, running the Bash executable file *helloscript.sh* will result in the output *"The message from helloscript is: good morning people"* being printed to the console

----------

### Files and Directories

![](https://preview.redd.it/qjeywdkcvrq61.png?auto=webp&s=0c89b59d87b067790f0b89cbc00c531504bda813)

Since Bash is primarily a *scripting language*, most valid command line commands (`mkdir`, `ls`, `touch` etc) are valid Bash code that can be written within a .sh file. We can use these to interact with files and directories on our local device.

#### Directories

* the code below would create a **new local directory** with the name *testFolder* within the user's desktop when the Bash executable is run
* the `-p` flag prevents an error from occurring should the same Bash executable be run more than once

```bash
mkdir -p testFolder
```

* however, to properly check whether a given directory already exists, we need to do the following
* the `-d` flag checks whether the following **directory** listed *already exists*

```bash
echo "Enter your directory name: "
read directoryName

if [ -d "$directoryName" ]
then
    echo "$directoryName already exists!"
else
    echo "$directoryName does not exist!"
fi
```

#### Files

* as previously mentioned, we can use the `touch` command within our Bash executable to create a file
* **always remember** to use the `$` dollarsign when referencing variables outside of initialization and assignment in Bash

```bash
echo "Enter your file name: "
read fileName
touch $fileName
```

* in the same fashion as the aforementioned `-d` flag *(used to check if a directory exists)*, the `-f` flag is used here to check whether a **file** *already exists*

```bash
echo "Enter your file name to check if it exists: "
read fileName

if [ -f "$fileName" ]
then 
    echo "$fileName exists"
else
    echo "$fileName does not exist"
fi
```

#### Writing data to Files

* as previously mentioned, **writing** over new data to files is achieved with the `>` single angle bracket operator

```bash
echo "Enter the file name: "
read fileName

if [ -f "$fileName" ] 
then 
   echo "Enter the text you wish to write: "
   read fileText
   echo "$fileText" > $fileName
else
    echo "$fileName does not exist"
fi
```

#### Appending data to Files

* as previously mentioned, **appending** additional text to files is achieved with the `>>` double angle bracket operator

```bash
echo "Enter the file name: "
read fileName

if [ -f "$fileName" ] 
then 
   echo "Enter the text you wish to append: "
   read fileText
   echo "$fileText" >> $fileName
else
    echo "$fileName does not exist"
fi
```

#### Reading data from Files

A few things to note about this:

* to **read** a file *line-by-line*, we use a `while` loop to **read** the value of each line of the file into the variable *line*, which we then print out using `echo` 
* `-r` is the **reading flag**, used to *read* each line of a file into a specified variable
* `IFS` is used for accounting for *whitespaces* within the file 
* `<` reverse angle bracket is used behind the `done` of the `while` loop to indicate the file we want to **read** data from [since we are **reading** (`<`) and not writing/appending (`>`/`>>`) to the file, the direction of the angle bracket is opposite]

```bash
echo "Enter name of the file you want to read: "
read fileName

if [ -f "$fileName" ]
then
    while IFS = "" read -r line
    do
        echo "$line"
    done < $fileName
else
    echo "$fileName does not exist"
fi
```

#### Deleting files

* **deleting** files can simply be achieved with the use of the `rm` command, which is also used in the command line

```bash
echo "Enter the name of the file to be deleted: "
read fileName
if [ -f "$fileName" ]
then
    rm $fileName
else
    echo "$fileName does not exist!"
fi
```

### Curl

![](https://www.fitliferegime.com/wp-content/uploads/2022/02/Wide-Grip-Barbell-Curl.jpg)

For the uninitiated, a greatly oversimplified introduction to `curl` (client URL) has some of the same functionality as `sudo apt get`, `homebrew` and `sudo dnf` in that it alows a *local device* to **exchange data** with a *server*. Like the other commands explored before, `curl` is often used on the command line, though it can be run in a Bash executable file as well.

#### -O

* `curl` will begin the download of data from the remote server to our local device
* `-O` ***Big O flag*** indicates to Bash to allow the downloaded file to **inherit** the file name given to it by the remote server

```bash
url = "https://proof.ovh.net/files/1Mb.dat"
curl ${url} -O
```

#### -o

* `-o` ***small o flag*** indicates to Bash to **save** the downloaded file under the file name specified *(in this case, NewFileDownload)*, in the same way we indicate to the C++ compiler when we want to save the C++ executable under a different name *`g++ main.cpp -o hello`*

```bash
url = "https://proof.ovh.net/files/1Mb.dat"
curl ${url} -o NewFileDownload
```

#### >

* note that the code below, where we use the `>` single angle bracket *(which indicates reading into a file)* also achieves the same effect as the `-o` flag in this situation

```bash
url = "https://proof.ovh.net/files/1Mb.dat"
curl ${url} > NewFileDownload
```

#### -I

* the `-I` **Big I flag** allows us to obtain the **meta-data** of the file we are downloading using `curl`, allowing us to make decisions pertaining to the downloading of said file

```bash
url = "https://proof.ovh.net/files/1Mb.dat"
curl -I ${url}
```

----------

### Creating menus in Bash

* the `select` `in` loop automatically generates a ***selection*** from the elements given to it *(BMW Mercedes Tesla Rover Toyota)*, and allows the user to input their choice, which `select` `in` will then **assign** to the specified variable *(car)*

```bash
select car in BMW Mercedes Tesla Rover Toyota
do 
    echo "You have selected $car"
    break
```

----------

### Grep

![](https://www.meme-arsenal.com/memes/b01f18ca70a42a57f4c31934afd3bae7.jpg)

For the uninitiated, `grep` is the command line equivalent of *control-f* to **search for patterns and text** in a file, though `grep` allows for use of regular expressions. Just like any other command that can be run on the command line, `grep` is valid Bash code.

* as seen in the code below, `grep` is used in the format of `grep {search parameters} {filename of file to be searched}`, and will return every instance of the *search pattern* being **matched**
* `-i` **small i** flag removes *case sensitivity* for the `grep` search
* `-n` **small n** flag adds *line numbers* to each **matched pattern** returned by the `grep` search
* `-c` **small c** flag will return the **matched word** and the *number of instances* there is a **matched pattern**
* `-v` **small v** flag will return the *number of lines* that do ***not*** have a **matched pattern**

```bash
echo "Enter the filename: "
read fileName

if [ -f "$fileName" ]
then
    echo "Enter the search parameters: "
    read searchParam
    grep -i -n -c -v $searchParam $fileName
else
    echo "$fileName does not exist"
fi
```

----------

### Awk

Very briefly, `awk` is a *scripting language* used to **format data and reports** and **process text files**, its program files ending in *.awk*. Similar to `grep`, since `awk` is a valid command on the command line, it can be run as Bash code.

* note that any interaction with `awk` scripting from within Bash must occur within the set of *'single quotes'* suffixing `awk`, as seen below
* as seen in the code below, `awk` is simply being used to **print out the entire file** to the command line 

```bash
echo "Enter the filename to be printed from awk: "
read fileName

if [ -f "$fileName" ]
then
    awk '{print}' $fileName
else
    echo "$fileName does not exist"
fi
```

* `//` double slashes indicate to `awk` to only **print out** lines that **match the pattern** of the word enclosed by the `//` double slashes *(peepeepoopoo)*

```bash
echo "Enter the filename to be printed from awk: "
read fileName

if [ -f "$fileName" ]
then
    awk '/peepeepoopoo/ {print}' $fileName
else
    echo "$fileName does not exist"
fi
```

* `$` dollarsign character *(when suffixing the `awk` `print` statement)*, indicates to `awk` to only **print out** the Nth word *(with N being the number suffixing the `$`)* in lines that **match the pattern** of the word enclosed by the `//` double slashes
* the same effect can be used to print out multiple words from a line, in the format of `awk '/{word to be searched}/ {print $2, $4, $9}' ${name of File}`

```bash
echo "Enter the filename to be printed from awk: "
read fileName

if [ -f "$fileName" ]
then
    awk '/peepeepoopoo/ {print $2}' $fileName
else
    echo "$fileName does not exist"
fi
```

----------

### [Summary of Awk, Sed, Grep](https://www-users.york.ac.uk/~mijp1/teaching/2nd_year_Comp_Lab/guides/grep_awk_sed.pdf)
