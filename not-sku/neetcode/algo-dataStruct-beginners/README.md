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
