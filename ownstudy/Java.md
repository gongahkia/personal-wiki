# `Java`

![](https://www.cnet.com/a/img/resize/efcac0a8d0f46692a6020d23a7ba5bee2b80fd95/hub/2012/04/19/8a430931-f0ec-11e2-8c7c-d4ae52e62bcc/20120419_Java_Duke_mascot_001.jpg?auto=webp&fit=crop&height=900&width=1200)

**Java programs** *(written in source code, end in `.java` file extension)* are compiled to **byte code** *(cross-platform intermediary code, end in `.class` file extension)*,  which is then compiled to **machine code** *(machine-specific)*.

> As such, Java is often known as the programming language that allows for the *"Write once, run anywhere"* philosophy.

---

### Compiling and Running Java code

![](https://preview.redd.it/pydopmz8ik151.png?auto=webp&s=f8b22d451089f15c87f6e7396b6f892789e7726b)

> ⚠️ *Note that these instructions are for setting up a Java program in VSCodium.*

1. Install the latest version of the [Java Development Kit](https://www.oracle.com/sg/java/technologies/downloads/).

2. Install the VSCodium Java extension pack.
    * Language Support for Java 
    * Debugger for Java
    * Maven for Java
    * Project Manager for Java
    * Test Runner for Java

3. Ensure that the *name of your Java program file* is the same as that of your ***main class***.

> *eg.* the below Java program file should have the name `shit.java`

```java
public class shit {
    
    public static void main(String[] args) {
        System.out.println("I gotta poo!");
    }

}
```

4. `javac` command **compiles** your Java program file into an executable `.class` file *(as byte code)* which can then be run on any machine as *machine code*.

> *eg.* taking the previous example, to compile our `shit.java` program, we do the following

```bash
javac shit.java
```

5. `java` is then used to **run** the compiled Java program in the Java Virtual Machine *(note that we do not include the `.class` file extension when we run the program)*.

```bash
java shit
```

> *To create a full-fledged Java project with VSCodium, refer to [this video](https://youtu.be/2WeYJrGbyIg).*

---

### Printing to the console

Similar to C and C++, Java requires us to enclose any code we want to run within the `main` method.

> *Note we refer to it as '`main` method' and **not** 'main function', as all Java program code is enclosed within the '`Main` public class' seen above.*

* `public static void main(String[] args)` is the neccesary boilerplate to create the **main method**

* `System.out.print()` **prints** the given text to the console *without* adding a newline character at the end

* `System.out.println()` **prints** the given text to the console *including* a newline character at the end

```java
public class Main {

    public static void main(String[] args) {
        System.out.print("Hello World!"); // prints to the console without a newline character
        System.out.println("Good morning ppl"); // prints to the console, and adds a newline character behind 
    }

}
```

* `//` create **single lined** comments
* `/*` and `*/` create **multi lined** comments

### Printf

![](https://media2.giphy.com/media/Y3HQJxujs34fnVd61n/200w.gif?cid=6c09b952r4jctmjmjly27jt6tyneer2f9qae2zabxmrfg9xq&rid=200w.gif&ct=g)

**Formatted strings** in Java are handled by the `printf()` method.

* `printf()`  
    * takes in 2 variables/ values *(String to be printed to the console, object/ variable/ value to be inserted into string)* as *arguments*, prints the **formatted string** to the console
    * `%` character is used to represent the **position** of the inserted object/ variable/ value in the string

Note that aside from the *`% character`*, there are 3 other parameters that can **format** the String.

> *% [flags] [precision] [width] [conversion-character]*

#### Conversion-character

* Each data type has a unique *conversion character* that accompanies the `%` character *(like in Bash)*.
    * `%b` : `boolean` value/ variable
    * `%c` : `char` value/ variable
    * `%s` : `string` value/ variable
    * `%d` : `int` value/ variable
    * `%f` : `float` and `double` value/ variable

*Here is an example of using `printf()` with the conversion character:*

```java
public class Main {

    public static void main(String[] args) {
        boolean myBool = true;
        char myChar = '@';
        String myString = "shit";
        int myInt = 200;
        double myDouble = 10.000;

        System.out.printf("shit, %b", myBool);
        System.out.printf("aight %c", myChar);
        System.out.printf("this is the string, %s", myString);
        System.out.printf("Good morning %d people", myInt);
        System.out.printf("float and double heaven with %f", myDouble);
        // all the above examples will print the formatted strings that include their relevant data types to the console
    }

}
```

#### Width

* An integer that indicates the **minimum number of characters** to be written as output *(inlcuding whitespaces)*.

*Here is an example of using `printf()` with the conversion character and width:*

```java
public class Main {

    public static void main(String[] args) {
        String myString = "poopoo";

        System.out.printf("Hello %10s", myString); // this would print out the String "Hello     poopoo" to the console, where poopoo (6 characters) is coupled with 4 other whitespace characters to make up the 10 characters specified in the width parameter
    }

}
```

#### Precision

* Number of digits of **precision** *(number of digits after decimal point)* when outputing `floats` and `doubles`.

*Here is an example of using `printf()` with the conversion character and precision:*

```java
public class Main {

    public static void main(String[] args) {
        double myDouble = 10.0000;

        System.out.printf("You have this much money: %.2f", myDouble); // this would print out the String "You have this much money: 10.00" to the console, since it indicates 2 degrees of precision, which is 2 digits to be shown after the decimal point
    }

}
```

#### Flags

* Adds **effects** to output based on the type of flag added to the *format specifier*.  
    * `-` : left-justify  
    * `+` : output `+` plus and `-` minus sign for numeric values
    * `0` : pad numeric values with zeros
    * `,` : commas to seperate groups of numbers exceeding 1000

*Here is an example of using `printf()` with the conversion character, width and flags:*

```java
public class Main {

    public static void main(String[] args) {
        float myFloat = 1.327;

        System.out.printf("You are worth this much on the black market: %010f", myFloat); // this would print out the String "You are worth this much on the black market: 000001.327" to the console, since the formatted float is automatically right justified with a width of 10 characters, and all whitespace characters have been padded by 0s
    }

}
```

---

### Variables

Java is a *statically typed* programming language, and variable declaration is the same as in C, C++ and Rust.

```java
public class Main {

    public static void main(String args[]) {
        int x = 123; // variable declaration && assignment (collectively known as variable initialization) can be done seperately
        System.out.println(x); // this would print the integer value of 123 to the console

        String veryImportantMessage; // variable declaration
        veryImportantMessage = "good morning people"; // variable assignment
        System.out.println(veryImportantMessage); // this would print out the string value of "good morning people" to the console
    }

}
```

### Final

`final` is the equivalent of a **constant** in Java, and is a variable whose value *cannot be changed* after assignment.

* **final variables** have their variable names *capitalized* as a naming convention

```java
public class Main {

    public static void main(String[] args) {
        final double PI = 3.14159; // attempting to reassign a value to the variable pi after initialization and assignment will result in a compilation error
    }

}
```

---

### Data types 

> Pay attention to the starred data types!

***Primitive data types:***

* ⭐ `boolean`: **true** or **false** *(1 bit in size)*
* `byte`: **-128** to **127** *(1 byte in size)*
* `short`: **-32,768** to **32,767** *(2 bytes in size)*
* ⭐ `int`: **-2 billion** to **2 billion** *(4 bytes in size)*
* `long`: **-9 quintillion** to **9 quintillion** *(8 bytes in size)*, character `L` to be appended after the long value
* `float`: floating point number up to **7 digits** *(4 bytes in size)*, character `f` to be appended after the float value
* ⭐ `double`: floating point number up to **15 digits** *(8 bytes in size)*
* ⭐ `char`: **single character**/**letter**/**ASCII value** *(2 bytes in size)*

***Reference data types:***

* ⭐ `String`: **sequence of characters** *(size varies)*

> A quick rundown of **Primitive** vs **Reference** data types:

| Primitive data type | Reference data type |
| :-------------------: | :-------------------: |
| 8 types *(mentioned above)* | unlimited number of types *(all user defined)* |
| stores data values | stores a memory address |
| stores 1 value | can store more than 1 value |
| less memory *(faster)* | more memory *(slower)* |

#### Swapping variables

To *switch the value* of 2 variables of the same data type in Java, we create a **temporary variable** to hold the value of one of them and reassign the value accordingly.

```java
public class Main {

    public static void main(string[] args) {
    string x = "water";
    string y = "piss";
    string temp;

    temp = x; // storing the value of x in string variable temp
    x = y; // now that string variable x has been freed up, we can store the value of y in it
    y = temp; // assigns the stored value of x to string variable y

    System.out.println(x); // this would show that x has swapped value with y
    System.out.println(y); // this would show that y has swapped value with x
    }

}
```

---

### Data structures

### Array

![](https://static.wikia.nocookie.net/evangelion/images/0/0c/Rei_Ayanami_Inside_Eva.png/revision/latest/scale-to-width-down/400?cb=20120325154541)

Stores **multiple values** of the **same data type** in a single variable *(similar to a list in Python)*.

* `[]` square brackets *append* the data type of the **array name**
* `{}` curly braces *surround* an array

```java
public class Main {

    public static void main(String[] args) {
        String[] peepee = {"poopoo", "Aight", "full metal alchemist"}; // this would create the array of Strings with the name peepee

        System.out.println(peepee[1]); // this would print out the element of index 1 to the console, "Aight"
    }

}
```

* Array elements can be *accessed* and *reassigned* by their **index** using `[]` square bracket notation

#### Array methods

* `.length()`  
    * returns the **length** of a given array

> Note that we can also create **Nested arrays** in Java *(2D arrays)*.

### ArrayList

> ArrayLists are just dynamic arrays  
> 
> ~ *Chantell Yu, 2023*

ArrayLists are **resizeable arrays**, allowing elements to be added and removed after **compile time**.  

* ArrayLists only store **reference data types**.

* import `java.util.ArrayList` class to use **ArrayList** objects in our program

* `ArrayList` declares the **ArrayList** data structure in the same way we would declare a *new class object*

* `<>` angle brackets contain the **reference data type** to be stored in the ArrayList

```java
import java.util.ArrayList;

public class Main {

    public static void main(String[] args) {
        ArrayList<String> food = new ArrayList<String>(); // creating a new ArrayList class object off the ArrayList class

        food.add("cheese");
        food.add("bayashi");
        food.add("hamburger");

        for (int i=0; i < food.size(), i++) {
            System.out.println(food.get(i));
        } // this will print out the elements within our ArrayList, "cheese", "bayashi", "hamburger"
    }

}
```

#### ArrayList methods

Since `ArrayList` is a class, we can call class methods on **ArrayList objects**.

* `.add()`
    * takes 1 reference data type variable/ value as an *argument*, **adds** it to the ArrayList

* `.get()`
    * takes 1 integer variable/ value of the index of the desired ArrayList element to be accessed as an *argument*, returns the **element** at said index

* `.set()`
    * takes 2 variables/ values *(index of element to be reassigned, new string variable/ value to be assigned to said index)* as *arguments*, and **assigns**/ **replaces** said value to the element at the given index of the ArrayList

* `.size()`
    * takes 0 *arguments*, returns the **length** of the ArrayList

* `.remove()`
    * takes 1 integer variable/ value of the index of the desired ArrayList element to be removed as an *argument*, **removes** said element

* `.clear()`
    * takes 0 *arguments*, **clears** every single item in the ArrayList

### 2D ArrayList

We can also create **Nested ArrayLists** in Java *(2D ArrayLists)*, for which we need to import `java.util.*`.

* `.get().get()`  
    * takes in 1 integer variable/ value each as an *argument* of the index of the respective elements within the ArrayList, returns the **element in a 2D ArrayList** at the given index *(similar to `[][]` in Python)*

```java
import java.util.*;

public class Main {

    public static void main(String[] args) {
        ArrayList<ArrayList<String>> overallList = new ArrayList(); // notice the syntax for the ArrayList of ArrayLists used here to create a nested ArrayList

        ArrayList<String> bakeryList = new ArrayList();
        bakeryList.add("carbs");
        bakeryList.add("shit bread");
        bakeryList.add("shitass");

        ArrayList<String> produceList = new ArrayList();
        produceList.add("keto");
        produceList.add("Hybrid Calisthenics");
        produceList.add("Chris Heira");

        ArrayList<String> DrinksList = new ArrayList();
        DrinksList.add("water");
        DrinksList.add("mountain pepper extra green");
        DrinksList.add("szechuan sauce");

        overallList.add(bakeryList);
        overallList.add(produceList);
        overallList.add(DrinksList);

        System.out.println(overallList); // this would print out all 3 nested arrays to the console, in the format of [ [], [], [] ]

        System.out.println(overallList.get(0).get(1)); // this would print out the string "shit bread" at the index of [0][1] from the 2D ArrayList to the console
    }

}
```

### HashMaps

**HashMaps** are Java's equivalent of Python-style dictionaries.

* To use a HashMap, we first have to import it with `import java.util.HashMap`.

> Note that similar to other data structures, we have to *statically define* the data types of the key-value pairs taken in by the HashMap.

Since `HashMap` is a class, we can call class methods on **HashMap objects**.

* `.put()`
    * takes 2 variables/ values, the **key-value** pair to be added to the HashMap.

* `.putIfAbsent()`
    * takes 2 variables/ values, the **key-value** pair to be added to the HashMap if the existing key does not already exist in the HashMap *(the value can be different)*.

* `.replace()`
    * takes 2 variables/ values, the **new key-value** pair that is to replace an existing key-value pair within the HashMap.

* `.get()`
    * takes 1 variable/ value, the **key** whose correponding value we want to retrieve from the HashMap.
    * returns *'null'* if an invalid key is supplied.

* `.getOrDefault()`
    * takes 2 variables/ values, the **key** whose corresponding value we want to retrieve from the HashMap, as well as a **default value** that is returned should said key not exist.
    * *similar to `.get()` in Python dictionaries*

* `.remove()`
    * takes 1 variable/ value, the **key** whose corresponding key-value pair we want to remove from the HashMap.

* `.containsKey()`
    * takes in 1 variable/ value, the **key** whose corresponding key-value pair you want to check whether exists in the HashMap, returns a boolean value. 

* `.containsValue()`
    * takes in 1 variable/ value, the **value** whose corresponding key-value pair you want to check whether exists in the HashMap, returns a boolean value. 

* `.clear()` 
    * takes no arguments, **clears and empties out** the entire HashMap.

* `.size()`
    * takes no arguments, returns the **number of key-value pairs** stored in the HashMap.

* `.isEmpty()`
    * takes no arguments, returns a boolean value depending on whether the HashMap is empty or not.

* `.forEach()` 
    * takes 2 variables/ values, the **data types** of the key-value pair stored within the HashMap, allowing us to iterate over every single key-value pair.

```java
import java.util.HashMap;

public class Main {

    public static void main(String[] args) {
        HashMap<String, Integer> examScores = new HashMap<String, Integer>; // HashMaps are instantiated like this
        }

}
```

---

### String methods

As previously covered, the `String` is a **reference data type** that stores *1 or more characters*. 

> As such, we can call the following methods on **all** string objects.

* `.equals()`  
    * takes in 1 string variable/ value as an *argument*, returns a **boolean value** if the argument is the same as the string object
    * **case-sensitive** when checking for equality

* `.equalsIgnoreCase()`
    * takes in 1 string variable/ value as an *argument*, returns a **boolean value** if the argument is the same as the string object
    * **not case-sensitive** when checking for equality

* `.length()`
    * takes in no *arguments*, returns the **length** of the string object

* `.charAt()`
    * takes in 1 integer variable/ value of the index of the desired character as an *argument*, returns the **character** at the given index

* `.indexOf()`
    * takes in 1 character variable/ value as an *argument*, returns the **index** of the given character within the string object

* `.isEmpty()`
    * takes in no *arguments*, returns a **boolean value** if the string object is empty

* `.toUpperCase()`
    * takes in no *arguments*, returns the string object with all the characters converted to **uppercase**

* `.toLowerCase()`
    * takes in no *arguments*, returns the string object with all the characters converted to **lowercase**

* `.trim()`
    * takes in no *arguments*, returns the string object with any **whitespace** to the left and right of the string **removed** *(similar to .strip() in Python)*

* `.replace()`
    * takes in 2 character variables/ values *(old character to be replaced, new character)* as *arguments*, returns the string object with the desired characters **replaced** by the new characters

> Additional String methods can be found [here](https://www.w3schools.com/java/java_ref_string.asp).

---

### User input

To accept user input, we have to import the `java.util.Scanner` class.

* `Scanner` class allows us to create a **scanner object** that *receives user input*  

    * `System.in` is the argument taken in by the **scanner object** to indicate to Java to take in user input from the standard system input *(the console)*  
    * `.nextLine()` method is called on the **scanner object** to *read the user input* entered into the console until it hits a newline character 
    * `.nextInt()` method is called on the **scanner object** to *only read user input* from the console that is an **integer**
    * `.next()` method is called on the **scanner object** to *only read user input* from the console that is a **character**
    * `.close()` method is called on the **scanner object** to *close the scanner* after all user input has been read

```java
import java.util.Scanner;

public class Main {

    public static void main(String[] args) {
        Scanner s1 = new Scanner(System.in); // created the s1 scanner object, which scans System.in for user input

        System.out.println("Please input your name: ");
        String name = s1.nextLine(); // takes in user input from the console until the newline character is read
        System.out.println("Please input your age: ");
        int age = s1.nextInt(); // indicates to the scanner object to only accept an integer as input
        s1.nextLine(); // we call this method one more time to clear the newline character left behind from the .nextInt() method, as that method only reads the inputted integer and does not accept any input that is a non-integer data type (newline character)

        System.out.println("Hello " + name + " !");
        System.out.prinln("You are " + age + " years old!");
        s1.close(); // to close the scanner class object
    }

}
```

---

## File handling

Like everything else in Java, *file handling* is done via **classes**, and in this case, we import `java.io.File` to use the `File` class.

* `File()`
    * used to instantiate a file instance object off the `File` class 
    * takes in 1 argument, the target **file name** *(including the **relative file path** to the target file as required)*
    * note the file extension (eg. `.txt`/ `.json`) should be included in the file name

We can call several *methods* on the newly created file object.

* `.exists()`
    * takes in no arguments
    * returns a **boolean** value depending on whether the given file object *can be found* and *exists* or not

* `.getPath()`
    * takes in no arguments
    * returns a copy of the target **file name** *(and relative file path from the Java program)* that was included when the `File` instance object was instantiated

* `.getAbsolutePath()`
    * takes in no arguments
    * returns the **absolute file path** *(full file path)* of the target file on the local machine

* `.isFile()`
    * takes in no arguments
    * returns a **boolean** value depending on whether the given file object is a *file* or a *file directory*/ *folder*

* `.delete()`
    * takes in no arguments
    * **deletes** the target file specified in the `File` instance object that was instantiated at the beginning

```java
import java.io.File;

public class Main {

    public static void main(String[] args){
        File fhand = new File("input.txt");

        if (fhand.exists()) {
            System.out.println("Huat ah");
            System.out.println(fhand.getPath()); // returns the relative file path of the target file from the Java program
            System.out.println(fhand.getAbsolutePath()); // returns the absoluet file path of the target file on the local machine, for Windows, it would likely return something like "C:\Users\gong\Desktop\coding\projects\study-notes\Java\java.md"
            System.out.println(fhand.isFile());
            System.out.println(fhand.delete()); // will delete the target file specified in the File instance object's instantiation
        } else {
            System.out.println("Ohh shit");
        }
    }

}
```

> Refer [here](https://www.w3schools.com/java/java_files.asp) for additional file handling methods.

### Writing to Files

To **write** to files, we have to import a whole other `java.io.FileWriter` class.

* `FileWriter()`
    * used to instantiate a file writer instance object off the `FileWriter` class
    * takes in 1 argument, the desired **file name** *(including the **relative file path** to where the target file is/ where we want the new file to be created)*
    * note the file extension (eg. `.txt`/ `.json`) should be included in the file name
    * note that we must surround *instantiation* of the file writer instance object within `try` and `catch` blocks by **default** *(in the format seen below)*
        * we also have to import the `jave.io.IOException` class to handle the exception within the `try` and `catch` block

We can call several *methods* on the newly created file writer object.

* `.write()`
    * takes in 1 String argument, the text to be *written* to the target file
    * note that `.write()` **writes over** existing text in the target file, and `.append()` should be used to add onto existing text

* `.append()`
    * takes in 1 String argument, the text to be *appended* to the target file
    * note that `.append()` **adds on** text to the existing text in the target file

* `.close()`
    * takes in no arguments
    * **closes** target file

```java
import java.io.Filewriter;
import java.io.IOException;

public class Main {

    public static void main(String[] args) {
        try {
            FileWriter fwrite = new FileWriter("testing.txt");
            fwrite.write("ah shit");
            fwrite.append("\nhere we go again");
            fwrite.close();
        } catch (IOException e) { // default try and catch block to include when creating a new file writer object
            e.printStackTrace();
        }
    }

}
```

### Reading Files

Similarly, to **read** a file, we have to import the `java.io.FileReader` class.

* `FileReader()`
    * used to instantiate a file reader instance object off the `FileReader` class
    * takes in 1 argument, the target **file name** that is to be read *(including the **relative file path** to the target file)*
    * note the file extension (eg. `.txt`/ `.json`) should be included in the file name
        * the file reader instance object reads the contents of a file as a *stream of characters*, **one by one**
    * note that we must surround *instantiation* of the file reader instance object within `try` and `catch` blocks by **default** *(in the format seen below)*
        * we also have to import the `jave.io.FileNotFoundException` and `java.io.IOException` classes to handle the exceptions within the `try` and `catch` block

We can call several *methods* on the newly created file reader object.

* `.read()`
    * takes in no arguments
    * returns an **int** value that contains the *byte value* of the character currently being read *(which we can then cast to a character)*
        * `(char)` typecasting casts *(converts)* an int value to a **char value** 
        * there is **no text left to be read** once `.read()` returns -1 int value

* `.close()`
    * takes in no arguments 
    * **closes** the target file

```java
import java.io.FileReader;
import java.io.FileNotFoundException;
import java.io.IOException;

public class Main {

    public static void main(String[] args) {
        try {
            FileReader fread = new FileReader("input.txt");
            int data = fread.read(); // to store the integer value being returned from the .read() method

            while (data != -1) { // handles reading till the end of a file
                System.out.print((char)data); // we use System.out.print() so as to not print a newline character \n after every character we print out
                data = fread.read();
            }
            fread.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
```

---

### Java GUI libraries 

![](https://images.wondershare.com/filmora/article-images/green-screen-app-chromavid.jpg)

> 📢 *This section will continue to be updated as I learn more about creating GUIs in Java.*

For all of these **Graphical User Interface** *(GUI)* libraries, we have to `import` them in at the beginning of the program.

1. `javax.swing.JOptionPane`
    * Brings up small dialog boxes that also accept user input and display system messages.
  
2. `javax.sound.sampled.*`
    * Allows us to play `.wav` files *(music)* from our Java program.

> Many of the core components we will use to build GUI come from the `javax.swing` package.

3. `javax.swing.JFrame`
    * Creates a Java frame that we can then populate with different elements.
        * `.setDefaultCloseOperation()`, `.setSize()`, `.setLayout()`, `.setVisible()` are all default methods we normally call on a JFrame object upon initialization.
    * We can also create a **parent class** off the `JFrame` class with specific default configurations, and instantiate *child classes* off the parent.
        * `.pack()` method dynamically resizes the JFrame object based on the components stored within it.
    * We can also **launch new GUI windows** by creating another class off the `JFrame` class.
        * Opens up a new GUI launch window, which we can then place components within *(similar to a `JFrame`)*.
        * `.dispose()` method called on `JFrame` object to **close** the specified frame.

4. `javax.swing.ImageIcon`
    * Allows for import of image assets (`.jpg`/`.png` format) into project.

5. `javax.swing.border.Border`
    * Allows us to create a **border** around our JFrame label.

6. `javax.swing.JLabel`
    * Allows us to display text and images inside our JFrame, and customize their positions and color accordingly.

7. `javax.swing.JPanel`
    * House different UI elements *(JLabels, JButtons etc)* while still being able to divide the entire JFrame into different sections.

8. `javax.swing.JButton`
    * Implements a **clickable** button within the JFrame or JPanel.

9. `javax.swing.JLayeredPane`
    * Similar to *Jpanels*, but we can **layer** *JLayers* to create a 3D effect.
        * By default, the first component added is placed on the *top-most* level, and the last component added is placed at the *bottom-most* level.
            * Each layer in `JLayeredPane` has a unique name, that can be referenced using **dot notation** to specify the exact layer we want to add a component to.
            * Each layer also has a **corresponding Integer value**, higher value rises to the **top layer**, lower value is at the **bottom layer**.

10. `java.awt.Color`
    * Allows for usage of different colors in your Java project.

11. `java.awt.Font`
    * Allows for usage of different fonts in your Java project.

12. `java.awt.Dimension`
    * Required to call specific dimensions within the `.setPreferredSize()` method *(alongside other methods)*

***Layout managers*** define the layout of UI components within a Java program.

> `.setLayout()` method is called on **JFrame** object to specify the desired layout manager.

13. `java.awt.BorderLayout`
    * Segments a screen by **formatting** JPanels and JLabels accordingly, useful for governing the relative position of UI elements on screen.
        * Called within the `.setLayout()` method of the `JFrame` class *(as one of the possible layout managers)*.

14. `java.awt.FlowLayout`
    * Arranges components in a *row* at their preferred size, and shifts components to the next row if the *horizontal container space* is too small.

15. `java.awt.GridLayout`
    * Arranges components in a *grid* of the specified size *(by default, all components in a single-row grid)*.
        * `GridLayout()` takes in 4 *arguments*, number of **rows**, number of **columns**, **length** of border spaces, **height** of border spaces.

Additional functionality can be found within the `javax.swing` library.

16. `java.swing.JOptionPane`
    * Creates a **standard dialog box** to *inform the user of something* or to *prompt for user input*.
        * `.showMessageDialog()` method is called on the `JOptionPane` object, and a *varying number of arguments* can be taken in to customize the dialog box.
            * `.PLAIN_MESSAGE' produces a notification prompt with **no icon**.
            * `.INFORMATION_MESSAGE` produces a information prompt with an **'i' icon**.
            * `.QUESTION_MESSAGE` produces a question prompt with a **'?' icon**.
            * `.WARNING_MESSAGE` produces a warning prompt with a **hazard icon**.
            * `.ERROR_MESSAGE` produces an error prompt with an **'x' icon**.
        * `.showConfirmDialog()` method is called on the `JOptionPane` object that allows for **user input**, and it returns an int value from -1 to 2 depending on user input. 
            * `.YES_NO_CANCEL_OPTION` creates the **three** specified buttons that users can interact with.
            * `.YES_NO_OPTION` creates the **two** specified buttons.
            * `.YES_OPTION` creates the **one** specified button.
        * `.showInputDialog()` method is called on the `JOptionPane` object, allowing for user to **type** in their input into a text field, which then returns us that user input.
        * `.showOptionDialog()` method is called on the `JOptionPane` object, and it **combines all the three previous types of prompts**, with [each unique argument](https://docs.oracle.com/javase/7/docs/api/javax/swing/JOptionPane.html) customizing the look and function of the prompt.
    
18. `javax.swing.JTextField`
    * Creates a GUI **textbox** component that a user can input *a single line of text* into.
    * `ActionListener` or `KeyListener` has to be added to the `JTextField` to **detect user input**.

19. `javax.swing.JTextArea`
    * Functions similarly to a `JTextField`, except that a user can input *multiple lines of text* into it.
    * `ActionListener` or `KeyListener` has to be added to the `JTextArea` to **detect user input**.

20. `javax.swing.JCheckBox`
    * Creates a **clickable** check box that the user can interact with.
        * Functions similarly to any other `javax.swing` GUI component, where we create an object off the `JCheckBox` class, and add functionality from there.
        * `.isSelected()` method called on the `JCheckBox` object, and it returns a **boolean value** depending on whether the check box is currently selected or not.

21. `javax.swing.JRadioButton`
    * Creates a set of **radio buttons** *(familiar to those acquinated with HTML radio buttons)* that the user can interact with.
        * Every new radio button must be instantiated as its own `JRadioButton` class object.
    * To only allow for selection of **one** of the radio buttons, we add the radio buttons to the same *group*.
        * `ButtonGroup` class used to create `ButtonGroup` objects, which we then call the `.add()` method on to add **all desired radio buttons** to the same group. 

22. `javax.swing.JComboBox`
    * Creates a **dropdown field** from which the user can choose their desired input.
        * Takes in an **array** of arguments during *initialization*, which it recieves as *options* for the **dropdown field**.
        * `.getSelectedItem()` method can be called on `JComboBox` object to receive the chosen user input.
        * `.getSelectedIndex()` method can be called on `JComboBox` object to receive the **index** of the chosen user input.
            * There are also a [bunch of other methods](https://docs.oracle.com/javase/tutorial/uiswing/components/combobox.html) that can be called on `JComboBox` objects to manipulate the given options and display.

23. `javax.swing.JSlider`
    * Creates an **adjustable slider** that users can interact with to *input a given value* to the program.
    * Takes in 3 int values, the *minimum* value, *maximum value*, and *starting point* of the knob on the `JSlider`.
        * There are a [bunch of methods](https://docs.oracle.com/en/java/javase/12/docs/api/java.desktop/javax/swing/JSlider.html) that can be called on the `JSlider` object.

24. `javax.swing.JProgressBar`
    * Creates a **progress bar** that ticks down as time passes to reflect flow of time in the GUI.
        * Note that to do things like **filling the progress bar** *(which we implement using `.setValue()` method)*, we can create another public method called `fill()` to do this for us *(seen in the example project below)*.
        * There are also a [bunch of methods](https://docs.oracle.com/javase/tutorial/uiswing/components/progress.html) that can be called on the `JProgressBar` object.

25. `javax.swing.JMenuBar`, `javax.swing.JMenu`, `javax.swing.JMenuItem`
    * Creates a **menu bar** that the user can interact with.
        * To add a `JMenu` to the `JMenuBar` object, we first assign each *menu* to a `JMenu` object, then call the `.add()` method on the `JMenuBar` object and add each *menu* individually.
            * To add `JMenuItem` to the `JMenu` object, we first assign each *menu item* to a `JMenuItem` object, then call the `.add()` method on the `JMenu` object and add each *menu item* individually.
                * Note that to detect user input via `ActionListener` interface, we must call the `.addActionListener()` method on **every single** `JMenuItem` object within our project *(since each of them qualify as a button)*.
        * To add a ***keybind*** to a given `JMenu` or `JMenuItem`, we call the `.setMnemonic()` method on the `JMenu` or `JMenuItem` object respectively.

26. `javax.swing.JFileChooser`
    * Creates a GUI that allows the user to choose a selected file from the local machine's file directory.
        * *Also see:* `javax.swing.filechooser.FileSystemView` class.

27. `javax.swing.JColorChooser`
    * Creates a GUI that allows the user to pick a color from a given palette offered by Java, and assign said color value to a given variable.

For every instance of **user input** that we want to detect in a Java program, we have to use the following.

28. `java.awt.event.KeyListener`
    * Used to detect any input from the keyboard *(`JTextArea`, `JTextField`), often paired together with the `java.awt.event.KeyEvent` library.
        * Always remember to **implement** the `KeyListener` interface and make all desired input fields visible to the `KeyListener` object. 
        * `.addKeyListener()` method to be called on the object field we want to **detect key input** with our `KeyListener` object from.

29. `java.awt.event.ActionListener`
    * Used to detect any input from a button *(clickable fields)*, often paired together with the `java.awt.event.ActionEvent` library.
        * Always remember to **implement** the `ActionListener` interface and make all desired input fields visible to the `ActionListener` object. 
        * `.addActionListener()` method to be called on the object field we want to **detect action input** with our `ActionListener` object from.
            * *Also see:* **Keybindings** are offered as an alternative possibility in Java for easier game development with `javax.swing` components.

30. `java.awt.event.MouseListener`
    * Used to detect any input from the mouse, often paired together with the `java.awt.event.MouseEvent` library.
        * Always remember to **implement** the `MouseListener` interface and make all desired input fields visible to the `MouseListener` object.
        * `.addMouseListener()` method to be called on the object field we want to **detect mouse input** with our `MouseListener` object from.
            * *Also see:* `ClickListener`, `DragListener`, `addMouseMotionListener`, `MouseAdapter`, `MouseMotionAdapter`.

Additionally, we are able to paint 2-D graphics and implement rudimentary animation in our JFrames.

31. `paint(Graphics g)`
    * Every **Java component** *(most `javax.swing` components)* has a built-in `paint()` method, that takes in the Graphics2D object.
        * There are a whole bunch of methods that can be called on a `Graphics2D` object, which can be found [here](https://docs.oracle.com/javase/7/docs/api/java/awt/Graphics2D.html).
    * Using the `java.awt.event.ActionListener` interface, we can implement basic animation of graphics moving across a screen.

---

### Expressions

Expressions comprise the **operands** and **operators**.

![](https://d1lss44hh2trtw.cloudfront.net/assets/article/2019/08/18/rainbowsixsiege-bg_feature.jpg)

* **Operands** *(anything that an arithmetic operator can be called on)*
    * Variables
    * Values
    * Numbers
    * Quantity

* **Operators** *(think arithmetic operators)*
    * `+` addition  
        * `+=` is a valid operator
        * `++` increments by one

    * `-` subtraction
        * `-=` is a valid operator
        * `--` decrements by one

    * `*` multiplication
        * `*=` is a valid operator

    * `/` floor division
        * `/=` is a valid operator

    * `%` modulus *(returns the remainder of a division operation)*
        * `%=` is a valid operator

---

### More methods of the `Math` class

![](https://quizizz.com/media/resource/gs/quizizz-media/quizzes/714aa5c6-fc63-4edc-a5cf-db32ee90e782)

* `Math.max()`  
    * takes in 2 variables/ values as *arguments*, returns the **larger** of the 2 arguments
    * is able to take in 2 numbers of ***different data types***

* `Math.min()`  
    * takes in 2 variables/ values as *arguments*, returns the **smaller** of the 2 arguments
    * is able to take in 2 numbers of ***different data types***

* `Math.abs()`  
    * takes in 1 variable/ value as an *argument*, returns the **absolute value** of the argument

* `Math.sqrt()`
    * takes in 1 variable/ value as an *argument*, returns the **squarerooted value** of the argument

* `Math.round()`
    * takes in 1 variable/ value as an *argument*, returns the **rounded up or down value** of the argument

* `Math.ceil()` 
    * takes in 1 variable/ value as an *argument*, returns the **rounded up value** of the argument

* `Math.floor()`
    * takes in 1 variable/ value as an *argument*, returns the **rounded down value** of the argument

---

### Conditional statements

Conditional statements in Java function the same way as in any other programming language.

* `if` statement
* `else if` statement
* `else` statement

```java
public class Main {

    public static void main(String[] args) {
       int age = 75;
       if (age >= 75) {
            System.out.prinln("aight bet");
       } else if (age == 18) {
            System.out.println("this rose is very nice");
       } else {
            System.out.println("You are an adult!");
       }
    }   // this conditional statement chain will see "aight bet" printed out to the console    

}
```

#### Switch statements

Similarly, `switch` statements in Java function the same way as in other programming languages *(alongside the use of `case`)*.

* `break` statement is required to **break out** of the `switch` statement should the given condition be met

* `default` keyword creates a **default case** to execute should no prior case be met

```java
public class Main {

    public static void main(String[] args) {
        string Day = "Monday";

        switch(Day) {
            case "Sunday": System.out.println("Today is Sunday!");
            break;
            
            case "Monday": System.out.println("Today is Monday!");
            break;

            case "Tuesday": System.out.println("Today is Tuesday!");
            break;

            default: System.out.println("Ohh shittt");
        }
    }   // the above switch statement would result in "Today is Monday!" being printed out to the console

}
```

### Logical operators

* `&&` and 
* `||` or
* `!` not

---

### Loops

The logic governing **loops** in Java are essentially the same as in other programming languages, such as C, C++, Javascript and Rust.

#### While loops

`while` loops in Java can execute an *infinite number of times*.

* `while` loop
* `do while` loop

```java
public class Main {

    public static void main(String[] args) {
        String yes_or_no = "yes";
        do {
            System.out.println("Eh ok");
        } while (yes_or_no == "yes"); // note that this do while loop will endlessly run since there is no set break condition

        while (true) {
            System.out.println("I'm trapped in an endless loop!");
        } // a conventional while loop, though the while true has created an infinite loop
    }

}
```

#### For loops

`for` loops in Java will execute a *limited number of times*.

* `for` loop

```java
public class Main {

    public static void main(String[] args) {
        for (int i=0; i <= 10; i++) {
            System.out.println(i); // this would print out numbers starting from 0 to 10 to the console, incrementing by 1 each time
        }
    }

}
```

#### For-each loops

Additionally, Java also has `for-each` loops, which **iterate over a given data set** a limited number of times. *(These are closer to what we're more familiar with in Python and C++.)*

* `for` is still used to declare the `for-each` loop

* `:` colon indicates the *iteration variable*, as well as the *data set to be iterated over*

```java
public class Main {

    public static void main(String[] args) {
        String[] animals = {"cat", "dog", "bird", "elephant"};

        for (String animal: animals) {
            System.out.println(animal); // this would iterate over every element in the String Array animals, assigning each element to the String variable animal, and printing said variable to the console
        }
    }

}
```

> Note that `for-each` loops can be called on **Arrays**, **ArrayLists**, and other data structures.

---

### `try`, `catch()` and `finally`

Exception handling in Java is settled by the `try` and `catch()` keywords.

* `try`  
    * surround any **potentially dangerous code** in a `try` block *(similar to try and except in Python)*

        * if the `try` block *run smoothly*, **ignore** the `catch()` block

        * else if the `try` block *hits an exception*, ignore the `try` block and **run** the `catch()` block

* `catch()`  
    * takes exact **Java error log name** *(seen in the console)* of data type ***Exception***, and a **variable name** *(by convention, character 'e')* as *arguments*
    * there can be **multiple** `catch()` statements to account for different exception cases

* `finally`  
    * `finally` block **always executes**, regardless of whether we catch an exception or not
    * always placed *at the end* of our `try` and `catch()` blocks  
        * often used to close any *files* or *scanners* we previously opened

```java
import java.util.scanner;

public class Main {

    public static void main(String[] args) {
        try {
            Scanner scanner = new Scanner(System.in);

            System.out.println("Enter a whole number to divide: ");
            int x = scanner.nextInt();

            System.out.println("Enter a whole number to divide by: ");
            int y = scanner.nextInt();

            int z = x/y;

            System.out.println("result: " + z);
        }
    
        catch(ArithmeticExpression e) { // to handle the exception generated by the Java error log in the console, "java.lang.ArithmeticExpression"
            System.out.println("Stop being clown please I beg");
        }

        catch(InputMismatchException e) { // to handle the exception generated by the Java error log in the console, "java.lang.InputMismatchException"
            System.out.println("Enter a number you idiot!");
        }

        finally {
            System.out.println("This will alwyas print!");
            scanner.close();
        }
    }

}
```

> Note that it is possible to **catch all exceptions** with `catch(Exception e)`. However, since *`Exception`* is the data type for all exceptions, this is considered lazy and should be avoided. Instead, handle each major exception case as seen above.

---

### Wrapper classes

![](https://oyster.ignimgs.com/mediawiki/apis.ign.com/apex-legends/2/2e/Loottick.jpg)

Wrapper classes allow *primitive data types* to be used as **reference data types**.

> This is useful as **reference data types** can ...

* have *methods* called on them
* be used in collections *(eg. Arraylist)*

**Some definitions:**

* *autoboxing*  
    * Java compiler automatically **detects** that we are creating wrapper class objects off a primitive data type and **converts** the primitive data types to its corresponding **wrapper class objects**
    * we are now able to *call* class methods on our **wrapper class objects** 👍

* *unboxing*
    * the reverse of **autoboxing**, the Java compiler automatically converts wrapper class objects to **primitive data type**

*These are the naming conventions for wrapper classes:*

| Primitive data type | Wrapper class |
| :---: | :---: |
| boolean | Boolean |
| char| Character |
| int | Integer |
| double | Double |

> Since String is **already** a reference data type, its first character is capitalized!

Below is an example of creating wrapper class objects off the 4 main primitive data types via **autoboxing**:

```java
public class Main {

    public static void main(String[] args) {
        Boolean a = true; // autoboxing, where the Java compiler automatically detects the boolean value and the object wrapper class "Boolean" that has been assigned to it
        Character b = '@'; // same for char value to object wrapper class "Character"
        Integer c = 123; // same for int value to object wrapper class "Integer"
        Double d = 3.14; // same for double value to object wrapper class "Double"
        String e = "Bro"; // String is already a reference data type

        if (a == true) {
            System.out.println("wow this rose, it smell very good"); // unboxing, as the wrapper class object a of reference type "Boolean" still retains its primitive data type of boolean for the value true, which the Java compiler automatically unboxes and allows for 
        }
    }

}
```

---

### Methods

Since all code in a Java program is enclosed within the `public class Main` (*Main class*), what would be referred to as a function in other programming language is called a **method** in Java.

The naming convention for methods is to **begin with a lowercase character**.

*Here is an example of a method:*

```java
public class Main {

    public static void main(String[] args) {
        hello(); // notice that the hello method can be called even though it is mentioned after the main method

        int a = 10;
        int b = 200;

        addition(a,b); // this would return a value of 210
    }

    static void hello() { 
        System.out.println("Hello!");
    } // note that this void method must be preceded with the static declaration, to allow for the main method to call it 

    static int addition(int x, int y) {
        return x + y; // this is an int method that takes in 2 arguments that takes in 2 arguments of type int, and returns an int value
    }

}
```

* `return` keyword is used to return a value from a **method**

Similar to other *statically typed* languages like C++ and C, **methods** must declare their **argument type** and ***return type***.

* `void`
* `boolean`
* `int`
* `float`
* `double`
* `char`
* `String`

### Overloaded methods

Similar to overloaded functions, **overloaded methods** are methods that *share the same name* but take in *different parameters* **(number of arguments, data type of arguments)**.

> This is possible as each overloaded method has a different ***method signature*** *(method name + method parameters)*.

*Here is an example of an overloaded method:*

```java
public class Main {

    public static void main(String[] args) {
        int a = 10;
        int b = 20;
        int c = 6.31249;
        int d = 70;

        addition(a, b); // this returns the integer value of 30
        addition(a, b, c, d); // this returns the integer value of 106 (since the addition method returns an integer value)
    }

    static int addition(int a, int b) {
        return a + b;
    }
    
    static int addition(int a, int b, double c, int d) {
        return a + b + c + d;
    } // an example of an overloaded method, the integer method addition is able to take in 2 variations of arguments, which have a varying number of parameters

}
```

---

## Object Oriented Programming 

![](https://pbs.twimg.com/media/EUJSyWmUMAARlov.jpg)

Java encourages **verbose** *object-oriented programming*, and shouldn't be too hard to grasp for those familiar with C++, Javascript and Python's OOP patterns.

As such, I'll be going over the ***similarities*** and ***differences*** between Java's object oriented patterns and those from other programming languages.

---

### They're the same thing 🫡

![](https://i.kym-cdn.com/photos/images/facebook/001/449/511/a9f.jpg)

* `class` keyword **creates** a new class *(which has its own methods and attributes)*

* `new` keyword **initializes** a new instance object, created off the *class* that has been declared previously
    * multiple objects can be *instantiated* off the same class, each with their own attributes and methods as defined by the program

* Object **attributes** and **methods** can be accessed and called respectively via `.` *dot notation*

* **Constructor** method is given the same name as the class, with *no need* for a method return type
    * *constructor* is a special method that is **automatically called** when an instance object is instantiated off a class 
    * *constructor* can take in arguments *(whose data types must be declared)*
    * `this.` is the equivalent of *`self.`* in Python, and **assigns** arguments taken in by the constructor as an object's *instance attributes*, which can be referenced by the object and its **methods**

* **Local** and **Global** scope *(aka variable scope)* 	🌎
    * **Local** scope  
        * Variables declared *inside a method*
        * Visible to and can only be referenced *within that method*
        * Local variables can enter the **global scope** via *constructor* methods (`this.`) and the `return` keyword

    * **Global** scope  
        * Variables declared *outside a method*, but *within a class*
        * Visible to and accessible by *all parts of a class*

* Passing **instance objects** as *arguments* to methods  
    * treat it the same way as passing any other argument to a method   
    * **declare the data type** of the *instance object* ***(its class)*** when passing the instance object as an argument to the method
    * the method can now access all **instance attributes** and **methods** that can be called from the *instance object*

* **Inheritance** between *parent* and *child* classes  
    * `extends` keyword indicates which *parent class* the child class is **inheriting** its attributes and methods from
    * **method overriding** in the *child class* to redefine an existing method from the *parent class* with a new implementation *(by using the same method name when declaring the method)*  
        * it is convention to include the `@Override` annotation above any method in a *child class* that has been **overridden**

* **Encapsulation** for greater security, preventing unwanted access to sensitive object attributes
    * attributes of a class are *private* by default
    * instance attributes are accessed and modified via **getters** and **setters**  
        * *public* methods that we have to define within the class, similar to `toString()`

* **Polymorphism**, where an object can identify as its *own data type* and its *parent class' data type* 🙊
    * access instance **attributes** and **methods** of an object from its *parent class* when many objects from the same *parent class* are bundled together
    * [**Dynamic polymorphism**](https://www.javatpoint.com/dynamic-polymorphism-in-java)

```java
public class Main {

    public static void main(String[] args) {
        Streamer Emiru = new Streamer("water", 10, 20.12); // initialized an instance object off the streamer class called Emiru with some arguments for the constructor, and the String "Streamer has been instantiated!" is printed to the console
        System.out.println(Emiru.type); // this would print out the String value "Adin Ross" to the console, an instance attribute of the class object Streamer
        System.out.println(Emiru.price); // this would print out the double value 1.28843 to the console, an instance attribute of the class object Streamer

        Emiru.missTate(); // this would call the missTate() void class method on the Emiru instance object, printing the String "Man, Andrew I miss you so much man" to the console

        Homies central_c = new Homies();

        Homies.hug(Emiru); // this would print out the String "Where are all my homies at? Eh yo water" to the console

        Entertainer pyrocynical = new Entertainer; // child class instantiated
        pyrocynical.emote(); // this will print out the string "Its 202020203" to the console, an inherited method that the child Entertainer class inherited from the parent Streamer class
        System.out.println(pyrocyncial.shits_to_give); // this will print out the integer 20, an instance attribute that is unique to the child Entertainer class

        pyrocycnical.missTate(); // this will print the String "Man, I have nothing against the guy" to the console, as we have overriden the missTate() void method from the parent Streamer class in the Entertainer child class
    }

}

public class Streamer {

    // global scope within the Streamer class 
    String type = "Adin Ross";
    int year = 2023;
    double price = 1.28843;

    // an object's instance attributes for the constructor, remember to statically declare them first!
    String name;
    int age;
    float weight;

    Streamer(String name, int age, float weight){
        System.out.println("Streamer has been instantiated!");
        this.name = name;
        this.age = age;
        this.weight = weight;
        // technically, the name, age and weight arguments taken in by the constructor are local scoped, but because we assign them to the object's instance attributes with "this", they become global scoped
    }
    
    void emote() {
        System.out.println("Its 202020203");
    }

    void missTate() {
        System.out.println("Man, Andrew I miss you so much man");
    }

    public class Homies {

        void hug(Streamer streamer_obj) { // note that the data type of the void method called on the Homies class object here is Streamer, which is the class of the object taken in as an argument by this method
            System.out.println("Where all my homies at? Eh yo " + streamer_obj.name);
        }

    }

    public class Entertainer extends Streamer { // child class Entertainer inheriting the attributes and methods of the Streamer parent class

        int shits_to_give = 20; // unique instance attribute that is globally scoped

        @Override
        void missTate() { // method overriding to redefine the inherited missTate() void method from the Streamer parent class in the Entertainer child class
            System.out.println("Man, I have nothing against the guy");
        }
    }

}
```

> Notice that the Streamer class is defined outside the Main class. In larger projects, it is **convention** to create a seperate file for every new class introduced, so as to prevent *bloat*.

### Super

* `super`  
    * keyword that allows the **child class** to access and customize its inherited *attributes* and *methods* from the **parent class** *(super class)* without ***method overriding*** *(completely redefining a new method under the same name)*
        * similar to the `this.` keyword, but instead of referencing an instance object's members, `super` references the parent class' members *(attributes, methods)*

    * `super()` when referencing parent class' *attributes*

    * `super.` when referencing parent class' *methods*

```java
public class Main {

    public static void main(String[] args) {
        Hero hero1 = new Hero("spongebob", 42, "$$$"); // we are still able to initialize our instance object hero1 with the constructor values from our child class Hero and the constructor values from our parent class person

        System.out.println(hero1.name); // this would print out the String "spongebob" to the console
        System.out.println(hero1.age); // this would print out the integer 42 to the console
        System.out.println(hero1.power); // this would print out the String "$$$" to the console
        System.out.println(hero1.toString); // this works due to the super keyword, which allows the child Hero class to access the toString method of the parent person class and copy its existing method, only modifying it by adding additional behaviours as desired
    }

}

public class person {

    String name;
    int age;

    person(String name, int age){
        this.name = name;
        this.age = age;
    }

    public String toString() {
        return this.name + "\n" + this.age + "\n";
    }

}

public class Hero extends person {

    String power;

    Hero(String name, int age, String power){
        super(name, age); // input the variable names of the parent class's constructor values as arguments to the super keyword, allowing the Hero constructor to be called using the same constructor values as the parent class and more
        this.power = power;
    }

    public String toString() {
        return super.toString() + this.power;
    }

}
```

### Abstract

![](https://i.redd.it/x2lyjb7bdwz41.jpg)

* `abstract`  
    * both **classes** and **methods** can be declared with the `abstract` keyword 

        * `abstract` classes cannot be *instantiated*, but their **child classes** *(subclasses)* can 👪
            * **prevents** objects from being instantiated off *parent classes*

                > an example of `abstract` classes in action:

            ```java
            public abstract class vehicle { // we now cannot instantiate a vehicle class object, and can only extend a child class off of the vehicle parent class and instantiate instance objects off of the child class
                // rest of class code
            }
            ```

        * `abstract` methods are declared without an *implementation*, there is **no code executed** when the method is called  
            * **forces** *child classes* to implement the method via ***method overriding*** without actually implementing said method in the *parent class*

                > an example of `abstract` methods in action:
            
            ```java
            public class transformer {
                abstract void transform(); // there is no implementation for the abstract void transform method in the parent transformer class, but every child class extended off this parent class is forced to implement said transform method via method overriding
            }
            ```

---

### *You know its not the same as it was* 😭

![](https://i.ytimg.com/vi/H5v3kku4y6Q/maxresdefault.jpg)

* **Overloaded constructors** are created the way you would expect *(the same as an overloaded method)*  
    * defining *multiple constructor statements* within a class, that take in a **variable** number of arguments of **different data types**

* `.toString()`  
    * a **method** that can be called on *any object* that is instantiated, and returns the **memory address** of the instance object in the computer's memory *(the same as if we printed out an instance object)*

        > However, we can override the `.toString()` method by doing the following.

    * **redefine** the `.toString()` method within our class *(to return all the attributes of our instance object that we want to know about as a String)*  
        > *eg. there is an instance object named p1 and `.toString()` method has been overriden*

        * can then be called **explicitly** by printing out the object with the `.toString()` method called 
            * `System.out.println(p1.toString())`
        
        * can also be called **implicitly** by printing out the object itself
            * `System.out.println(p1)`

```java
public class Main {

    public static void main(String[] args) {
        Pizza p1 = new Pizza("pita", "tomato");
        Pizza p2 = new Pizza("wholemeal", "cabonara", "mozzerela");
        Pizza p3 = new Pizza("White", "nutella", "american", "bannana");
        // instantiating multiple objects off the same class with overloaded constructors
        
        System.out.println(p1.toString()); // calling the toString() method explicitly
        System.out.println(p1); // calling the toString() method implictly
        
    }

}

public class Pizza {

    String bread;
    String sauce;
    String cheese;
    String topping;

    Pizza(String bread, String sauce) {
        this.bread = bread;
        this.sauce = sauce;
    }

    Pizza(String bread, String sauce, String cheese) {
        this.bread = bread;
        this.sauce = sauce;
        this.cheese = cheese;
    }

    Pizza(String bread, String sauce, String cheese, String topping) {
        this.bread = bread;
        this.sauce = sauce;
        this.cheese = cheese;
        this.topping = topping;
    }
    // above is an example of an overloaded constructor

    public String toString() {
        String myString = this.bread + "\n" this.sauce;
        return myString;
    } // we have to create our own toString() method to then call it explicitly or implicitly

}
```

### Array of objects

While creating an array *(not ArrayList)* of objects is mostly similar to what we're used to, we have to **declare** the *object array* as its own data type with the `new` keyword and the **size of the array**.

```java
public class Main {

    public static void main(String[] args) {
        Food[] fridge = new Food[3]; // note that we must statically declare the data type of the array (class of Food, which is its own data type), and the length of said Food array during assignment and initialization

        Food food1 = new Food("spinach");
        Food food2 = new Food("lettuce");
        Food food3 = new Food("apple");

        fridge[0] = food1;
        fridge[1] = food2;
        fridge[2] = food3;

        // Food[] fridge = {food1, food2, food3};
        // if I were to include this above line, I would no longer need to declare the fridge object at the beginning of the main method, as well as the individual assignment of objects as elements to my object array as seen above, since the above line does initialization, declaration and assignment all at once

        System.out.println(fridge[1].name); // prints out "lettuce" to the console
    }

    public class Food {
        String name; // global scoped variable

        // constructor method
        Food(String name) {
            this.name = name;
        }
    }

}
```

### Interface ⭐

![](https://i.ytimg.com/vi/b_V-VJQT6pM/maxresdefault.jpg)

Interfaces are *templates* that are applied to a class.

* specifies a class' **attributes** and **methods** 

    * there is **no method body** specified in the interface, just the name and return type *(similar to an abstract method)*

> With that, let's go into the important keywords.

* `interface` keyword **declares** and **creates** a new interface
* `implements` keyword indicates which interface a class is choosing to **adopt**
* a class can apply **more than 1** interface, seperated by a `,` comma

```java
public interface Prey {

    void flee(); // this indicates that all classes implemented off the Prey interface must have the flee() void method

}

public interface Predator {

    void hunt(); // another interface method

}

public class Rabbit implements Prey{

    @Override
    public void flee() {
        System.out.println("Rabbit fleeing noises");
    }

}

public class Hawk implements Predator{

    @Override 
    public void hunt() {
        System.out.println("Hawk hunting noises");
    }

}

public class Fish implements Prey, Predator{ // here, the Fish class has implemented 2 interfaces, both the Prey and Predator interface

    @Override 
    public void hunt() {
        System.out.println("Fish hunting noises");
    }

    @Override
    public void flee() {
        System.out.println("Fish fleeing noises");
    }

}
```

---

## Why the boilerplate? 🧠

![](https://www.maran-pro.com/upload/iblock/078/07894424d34b2f7ba52602a5aed66c36.jpg)

Java is known for being a ***verbose programming language***. 

Let's breakdown the most notorious line of Java code `public static void main(String[] args)` to better understand what's going on under the hood.

---

### Access modifiers

![](https://media.cnn.com/api/v1/images/stellar/prod/200619190852-public-restroom-coronavirus.jpg?q=w_1600,h_1082,x_0,y_0,c_fill)

***Definitions:***  
* *Class* : collection of code, normally saved in a file by itself
* *Package* : collection of classes
* *Project* : often a collection of packages

1. Default access modifer *(none specified)*
2. `public`
3. `protected`
4. `private`

### Default access modifier

By default, only *variables* and *methods* of a class within the **same package** are able to access and reference each other.

### `public`

`public` keyword makes the *variables* and *methods* of a class ***visible*** to any other class within the **same project** folder.

```java
public class C {

    String defaultMessage = "This is the default";
    public String publicMessage = "This is public";

}
```

### `protected`

`protected` keyword makes the *variables* and *methods* of a class ***visible*** to any other class, as long as it is a **child class**/ **sub class** of the parent class that contains a `protected` member.  
* child class can be in a **different package**

```java
public class D {

    String defaultMessage = "This is the default";
    protected String protectedMessage = "This is protected";

}
```

### `private`

`private` keyword makes the *variables* and *methods* of a class ***visible*** only to **itself**.

```java
public class E {

    String defaultMessage = "This is the default";
    private String privateMessage = "This is private";

}
```

---

### Static

![](https://i.kym-cdn.com/photos/images/facebook/001/582/715/5d6.jpg)

> What's a static?

Static *variables* and *methods* are referred to as **static members** *(basically Class attributes/ methods)*.

* `static`  

    * keyword *modifier* that can be applied to a **variable**, **method** or **class**
        * equivalent of a **Class attribute**/ **Class method** in [Python](https://builtin.com/software-engineering-perspectives/python-attributes)
    * **class** owns the *static variable*/ *static method*, and there is ***a single copy*** of the static member that is created and shared
    * as such, we normally call a *static variable*/ *static method* from the **class** itself, not from any instance object *(though it is possible, it is still not advised)*

```java
public class Main {

    public static void main(String[] args) {
        Friend f1 = new Friend("Markos");

        System.out.println(Friend.numberOfFriends); // calling the static variable attribute from the Friend class, this will print out the integer 1 to the console

        Friend.displayFriendCount(); // calling the static method of the Friend class, that will print out the String "You have 1 friends!" to the console
    }

}

public class Friend {

    String name;
    static int numberOfFriends; // since there is only one copy, all instance objects of the Friend class are forced to share this one static variable (class variable)

    Friend(String name) {
        this.name = name;
        numberOfFriends++; // global variable numberOfFriends is incremented whenever the constructor statement is called and a new friend is added 
    }

    static void displayFriendCount() {
        System.out.println("You have " + numberOfFriend + " friends!"); 
    } // static method of the Friend class

}
```

---

### Useful Java libraries

> 📢 *This section will continue to be updated as I learn more about the different Java libraries available.*

As with the ***Java Gui libraries*** above, we have to `import` these libraries to use them in our program.

1. `java.util.Random`  

    * imports the **Random class**, which we use to create **random objects** that we can call methods on  
        * `.nextInt()` generates a ***random integer***
        * `.nextDouble()` generates a ***random double***
        * `.nextBoolean()` generates a ***random boolean***

2. `java.util.Scanner`

    * as discussed above, imports the **Scanner class**, from which we can call methods on **scanner objects** to take in user input

---

### Other advanced Java concepts to look into:

> 📢 *This section will continue to be updated as I learn more about Java in general.*

1. [**Generics**](https://www.baeldung.com/java-generics): offered as an alternative to function overloading. 
    * [Differences](https://dev.to/pffigueiredo/the-typescript-functions-mental-model-1301) covered here.
2. [**Serialization**](https://www.baeldung.com/java-serialization): Process of converting an object to a byte stream *(saved as a platform-independent `.ser` file / byte stream can be sent over a network)*, saving the object even after the program exits.
3. [**Deserialization**](https://www.geeksforgeeks.org/serialization-in-java/): Reverse of Serialization, converting a byte stream into an object.
4. [**Threads**](https://www.w3schools.com/java/java_threads.asp): A thread of execution in a program (similar to tasks on a CPU) that execute parts of your code alongside the **main thread**.
    * Each thread has a *priority*, with those of higher priority being executed first.
    * **JVM** *(Java Virtual Machine)* executes threads until...
        * `exit` method of class Runtime is called
        * all user threads have died
5. [**Mulitthreading**](https://www.tutorialspoint.com/java/java_multithreading.htm): Process of executing multiple independent threads simultaneously.
6. [**Packages**](https://www.w3schools.com/java/java_packages.asp): Important to learn about user-defined packages for project management of larger projects in Java.
7. [**`.jar` file**](https://www.baeldung.com/java-create-jar): Important to learn about to create Java executable files in the future.
