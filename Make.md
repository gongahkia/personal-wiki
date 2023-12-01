# Make 

A `Makefile` is a text file which is universally recognised on all platforms, and can be used to run default commands for your project's build system.  

There are other alternatives such as *(Make, Rake, Ant, Maven)*.   

The general format of Makefiles are as follows.

```Makefile
command: file dependancy
    actual command you would normally type out
```

To invoke the command, you would simply type "make command".   

Notice how this functions like a rich man's bash alias since it works on virtually any platform.  

Here are some other special things Makefiles can do.

* declare variables to reuse text with `=` assignment operator, reference variables with `$()`
* `all` keyword will run the command universally when "make" is inputted into the console.
* its a good idea to have a `clean` rule to automate restoring a clean build 

```Makefile
CC=clang

all: hello

hello: hello.c test.o
    $(CC) -o hello hello.c

test.o: test.c
    $(CC) -c test.c -o test.o

clean:
    rm hello test.o
```

> [Reference](https://makefiletutorial.com/)
