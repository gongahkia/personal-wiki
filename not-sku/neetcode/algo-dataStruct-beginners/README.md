# Core of these notes 

These notes are taken from neetcode.io 🚀

## Introduction

Topics covered...
* Arrays
* Linked lists
* Recursion
* Sorting
* Binary search
* Trees
* Backtracking
* Heap / Priority queue
* Hashing
* Graphs
* Dynamic programming
* Bit manipulation

## Size conversion

* 1 bit == `1` or `0` in binary
* 1 byte == 8 bits
* Computers these days have RAM of at least 8GB (10^9 bytes)

Each character commonly occupies 1 byte (8 bits) in memory.  
Integers commonly occupy 4 bytes (32 bits) in memory.

## RAM

Before we dive into arrays, we first need to understand what a data structure even is in relation to computer memory.

Data structures are a way to store data efficiently in the computer's **RAM** (random access memory). An example is using an arrray (`[]`) to store an ordered group of integers in RAM, like `[1,3,5]`.

A `memory address` (distinct place in memory) and `value` are associated with each integer upon storing it in RAM. Each value is stored *contiguously* in RAM, similar to an array.

As such, each integer's memory address is 32 bits == 4 bytes apart. Similarly, each character's memory address is 8 bits == 1 byte apart.

The size of the type of each value stored in memory does not matter, as long as the memory address is incremented in accordance with the type of value being stored in the array.

## Arrays

Arrays are a way to store data contiguously.

### Static arrays

In strictly typed languages like Java, C, C++, C# etc, arrays are **static**, and have a fixed allocated size when initialized, which is immutable and cannot be changed after initialization.

Many data structures and algorithms rely on arrays in their underlying implementation.

Common operations on an array...

#### 1. Reading

##### Indexing 

Array elements are directly accessed through their index (most languages are zero-indexed). 

As long as index of array element is known, the access is **instant**, as each element has a specific `memory address` which is calculated based on *memory address of start of array* + *size of on element of specified data type of array (eg. integer 8 bytes)* * *index of element*.

> That's also why most languages are zero-indexed, since the first element of the array also shared the same memory address as the start of the array.

```Python
myArray:list = [1,2,3] # initializing the array
print(myArray[1]) # accessing the array element at index 1, second element
```

Regardless of array size, if the desired element's index is known, time taken to access the element will always be `O(1)` in terms of time complexity.

> `O(1)` means number of operations to read an array element is **constant** relative to the input size, in this case the size of the array.

##### Traversing 

Traversing by iterating through each element of an array is another way to access array elements. This is achieved through using a loop.

```Python
for i in range(len(myArray)):
    print(myArray[i])

# OR 

i = 0
while i < len(myArray):
    print(myArray[i])
    i += 1

# OR

for element in myArray: # here, python handles the implicit indexing call for you, but what's really happening under the hood is the first option
    print(element)
```

Since there is a loop, time complexity for traversing through an array of size `n` is `O(n)`, meaning number of operations to traverse an array is **linear** to `n`.

> In other words, if the size of the array (`n`) were to double, number of operations for traversal and the time said operations would take would also double.

#### 2. Deletion

##### Deleting from the end of an array

In strictly typed languages, all array indices are filled with `0s` (or some other default value) upon initialization to denote an empty array.

As such, removing an element from the last index of an array is as simple as restoring the last element's value to `0` or `null` or `-1`, since this **overwriting** effectively denotes an empty index. Length of the array is also recorded to been reduced by 1.

```Python
# removes element from the last position of the array if the array is not empty (length is non-zero)
def removeEnd(array:list, length:int) -> None:
    if length > 0:
        array[length - 1] = 0 
        # overwrites value of last element of the array with another default value
        # we will also consider the length of the array to be decreased by 1
```

Since this just uses simple indexing, it is quite clear that time complexity here would be `O(1)`, since as long as deletion of last element of the array CAN occur, number of operations to delete the last element of an array would be **constant** relative to input size.

##### Deleting at an `ith` index

Deleting an element at `ith` index of an array is more complicated, as it requires us to iterate from elements of index `i+1` onwards until end of the array, shifting each element `1` position to the left. This could mean shifting ALL elements to the left if `i` is `0`.

```Python
def removeMiddle(array:list, index:int, length:int) -> None:
    for q in range(index + 1, length): # creating range to shift array elements beginning from i + 1 to end
        array[q - 1] = array[q] # 'removal' of array element [i] occurs with the shift and reassignment
```

Time complexity here is `O(n)`, with number of operations being **linear** relative to input size of the array since a loop is used, and `n-1` shift operations are required if the index of the element to be removed is `0` (begining of the array) in the worst-case scenario.

#### 3. Insertion

##### Inserting at the end of the array

Since we can always access the last index of an array, inserting an element at the end of an array has a time complexity of `O(1)`, since number of operations is **constant** relative to size of input (size of array `n`).

```Python
# Inserts value into array at the next open position
# Since this is a static array and size is immutable, length is the number of actual values in the array, and capacity is the fixed size of the array (memory allocated for the fixed size array)
def insertEnd(array:list, value, length:int, capacity:int):
    if length < capacity:
        array[length] = value
```

##### Inserting at the `ith` index

Similar to deleting an array element of `ith` index, inserting an element at `ith` index requires us to shift all array elements of index `i-1` 1 position to the right. Shifting occurs prior to insertion to ensure no values are overwritten and lost.

```Python
# Insert value into array at index after shifting all subsequent elements to the right, assuming index is a valid index and array is not full
def insertMiddle(array:list, index:int, value, length:int):
    for index in range(length-1, i-1, -1): # iterating in decreasing range
        array[index + 1] = array[index]
    array[index] = value # final reassignment
```

Since this operation involves loops, time complexity is `O(n)`, with number of operations being **linear** relative to size of the input (size of the array). In the worse case scenario, we could be asked to insert a value at the index `0` of an array, in which case we would have to loop over every other value in the array to shift it one to the right via reassignment, prior to assigning `array[0] = value`.

### Dynamic arrays

### Stacks
