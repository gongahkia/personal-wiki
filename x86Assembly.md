# `x86 assembly`

> Add expressive description here and make it succint

## Comments

```asm
; ----- COMMENT -----

; this is a single-line comment

; there is no built-in implementation
; for multi-line comments in x86 assembly
; but the same effect can be achieved as 
; seen here
```

## Quickstart

```asm
; ----- QUICKSTART -----
    ; note that there are multiple kinds of assembly languages and each is tailored and optimised to their specific CPU or microcontroller architecture
    ; each processor architecture has its own instruction set with varying syntax and file extensions, denoted as the ISA (instruction set architecture)
    ; the more popular ISAs are as follows 
        ; x86 assembly => used for Intel and AMD processors
        ; ARM assembly => used for ARM processors in mobile devices and embedded systems
        ; MIPS assembly => used for MIPS processors in academic settings and specialized devices
        ; PowerPC assembly => used for PowerPC processors in older Apple computers and certain embedded systems
        ; SPARC assembly => used for SPARC processors developed by Sun Microsystems
        ; AVR assembly => used for AVR microcontrollers in Arduino systems
        ; PIC assembly => used for PIC microcontrollers in embedded systems

; ----- SYNTAX OVERVIEW -----
    ; section => declares a section with an x86 assembly program, where different sections in the program serve different purposes
        ; .data => data section stores variable and constant declaration 
        ; .text => text section stores all code instructions
    ; _start => label marking the beginning of all execution code within the program, the equivalent of the main function or method in other programming languages inheriting from the C family
    ; global => brings a specified label or call into the global scope of the x86 assembly program
    ; <valueOrConstantIdentifier> db <valueToBeAssigned> => define byte, used to assign a specified value to the specified variable or constant identifier 
    ; mov <register> , <valueOrVariableOrConstant> => moves the specified value literal, variable or constant identifier into the specified register

; ----- REGISTER -----
    ; small, fast storage locations within the CPU that store data and their respective memory addresses
    ; often used for performance of quick operations within x86 assembly programs
    ; the types of registers are as follows
        ; general-purpose register
        ; segment register
        ; control register
        ; special-purpose register

; --- GENERAL-PURPOSE REGISTERS ---
    ; EAX => accumulator register, used for arithmetic operations, IO operations, and as a register that stores function return values
    ; EDX => data register, used for IO operations, arithmetic operations as a 64-bit extension of EAX
    ; EBX => base register, used to hold store memory addresses for memory access operations
    ; ECX => counter register, used as a loop counter and for shift/rotate operations
    ; ESI => source index, used to store pointers for string and array operations
    ; EDI => destination index, also used to store pointers for string and array operations similar to ESI
    ; EBP => base pointer, used to store the pointer which points to the bottom of the stack
    ; ESP => stack pointer, used to store the pointer which points to the top of the stack

; --- SEGMENT REGISTERS ---
    ; CS => code segment, storing the pointer that points to the segment containing current program code
    ; DS => data segment, storing the pointer that points to the segment containing data
    ; ES => extra segment, acting as an additional data segment register on top of the existing DS
    ; SS => stack segment, storing the pointer that points to the segment containing the stack
    ; FS => additional segment, used for thread-local storage
    ; GS => another additional segment, used for thread-local storage

; --- CONTROL REGISTERS ---
    ; CR0 => contains flags that control the CPU's operating mode
    ; CR2 => contains the page fault linear address
    ; CR3 => contains the physical address of the page directory required for paging
    ; CR4 => contains flags that control additional CPU features

; --- SPECIAL-PURPOSE REGISTERS ---
    ; EIP => instruction pointer, storing the pointer that points to the next instruction to be executed
    ; EFLAGS => flags register, containing status, control, and system flags

; ----- PRINTING -----
    ; printing in x86 assembly involves system calls to interact with the local machine's operating system

section .data
    hello db 'Hello, world!', 0 ; defines a null terminated string and assigns it to the variable hello

section .text
    global _start ; places the _start label within the program's global scope

_start:
    mov eax, 4 
    mov ebx, 1 
    mov ecx, hello 
    mov edx, 13 
    int 0x80 ; triggers a software interrupt to invoke a syetem call to the kernel
    mov eax, 1 
    xor ebx, ebx 
    int 0x80 ; triggers the same software interrupt as above
```

## Types

```asm
; ----- TYPE -----
    ; In x86 assembly, data types are usually specified using assembler directives. Common data types include:

section .data
    myByte db 0x01        ; Define a byte
    myWord dw 0x1234      ; Define a word (2 bytes)
    myDword dd 0x12345678 ; Define a double word (4 bytes)
    myQword dq 0x123456789ABCDEF0 ; Define a quad word (8 bytes)
```

## Operators

```asm
; ----- OPERATOR -----


section .text
    global _start

_start:
    ; Arithmetic operators
    mov eax, 5      ; Move 5 into eax
    add eax, 2      ; Add 2 to eax, eax now contains 7
    sub eax, 1      ; Subtract 1 from eax, eax now contains 6
    imul eax, 3     ; Multiply eax by 3, eax now contains 18
    idiv ebx        ; Divide eax by ebx (requires edx:eax setup for dividend)

    ; Logical operators
    and eax, 0xFF   ; Perform bitwise AND with 0xFF
    or eax, 0x01    ; Perform bitwise OR with 0x01
    xor eax, eax    ; Perform bitwise XOR (clears eax to 0)
    not eax         ; Perform bitwise NOT (flips all bits in eax)
    shl eax, 1      ; Shift eax left by 1 bit
    shr eax, 1      ; Shift eax right by 1 bit
```

## Control structures

```asm
; ----- CONTROL STRUCTURE -----
```

### Loop
```asm
section .text
    global _start

_start:
    mov ecx, 10      ; Set loop counter

loop_start:
    ; Your loop code here
    dec ecx          ; Decrement counter
    jnz loop_start   ; Jump to loop_start if ecx is not zero
```

### Conditional Statements
```asm
section .text
    global _start

_start:
    mov eax, 5
    cmp eax, 5       ; Compare eax with 5
    je equal         ; Jump to equal if eax is 5

not_equal:
    ; Code if not equal
    jmp end

equal:
    ; Code if equal

end:
    ; Continue here
```

### Jumps
```asm
section .text
    global _start

_start:
    jmp label    ; Unconditional jump

label:
    ; Code to execute after jump
```

## Data structures

```asm
; ----- DATA STRUCTURE -----
```


While assembly language does not have high-level data structures, you can create arrays, structures, and other data formats manually.

### Array
```asm
section .data
    myArray db 1, 2, 3, 4, 5  ; Define an array of bytes

section .text
    global _start

_start:
    lea esi, [myArray]   ; Load address of myArray into esi
    mov ecx, 5           ; Set counter to 5 (number of elements)

array_loop:
    mov al, [esi]        ; Load byte from array into al
    inc esi              ; Move to next element
    loop array_loop      ; Loop until ecx is zero
```

### Structure
```asm
section .data
    myStruct:
        db 1           ; Byte
        dw 2           ; Word
        dd 3           ; Double word

section .text
    global _start

_start:
    lea esi, [myStruct]  ; Load address of myStruct into esi
    mov al, [esi]        ; Access first byte
    mov ax, [esi + 1]    ; Access word
    mov eax, [esi + 3]   ; Access double word
```

## Functions

```asm
; ----- FUNCTION -----
    ;

section .text
    global _start

_start:
    call myFunction
    jmp end

myFunction:
    ; Function code here
    ret

end:
    ; Continue here
```

```

## More on

* [x86 assembly documentation](https://www.cs.virginia.edu/~evans/cs216/guides/x86.html)
* [learn MIPS assembly in y minutes](https://learnxinyminutes.com/docs/mips/)
