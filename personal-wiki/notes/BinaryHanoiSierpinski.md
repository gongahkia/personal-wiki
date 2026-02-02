# `Binary, Hanoi and Sierpinski`

Notes on the 3Blue1Brown video exploring the surprising and beautiful connection between the Tower of Hanoi puzzle, binary numbers, and the Sierpinski triangle.

## Introduction

This is a story about finding deep connections between seemingly unrelated topics. We will explore the Tower of Hanoi puzzle and see how its solution is related to counting in binary, and how the structure of the problem itself gives rise to the famous Sierpinski triangle fractal.

## The Tower of Hanoi

The Tower of Hanoi is a classic puzzle with simple rules:
1.  There are three rods and a number of disks of different sizes which can slide onto any rod.
2.  The puzzle starts with the disks in a neat stack in ascending order of size on one rod, the smallest at the top.
3.  The objective is to move the entire stack to another rod, obeying the following rules:
    *   Only one disk can be moved at a time.
    *   A larger disk may not be placed on top of a smaller disk.

### The Recursive Solution

The solution to the puzzle is naturally recursive. To move a stack of $n$ disks from a source rod (A) to a destination rod (C), using an auxiliary rod (B):
1.  Move the top $n-1$ disks from A to B.
2.  Move the largest disk (the $n$-th disk) from A to C.
3.  Move the $n-1$ disks from B to C.

This recursive algorithm leads to the fact that moving $n$ disks requires $2^n - 1$ moves.

## The Connection to Binary Numbers

The sequence of moves in the optimal solution to the Tower of Hanoi puzzle is directly related to binary counting.

Consider the move number, starting from 1. If you write the move number in binary, the position of the least significant bit that is a '1' tells you which disk to move. For example, in a 3-disk puzzle:
*   **Move 1 (001):** The 1st bit is 1. Move disk 1.
*   **Move 2 (010):** The 2nd bit is 1. Move disk 2.
*   **Move 3 (011):** The 1st bit is 1. Move disk 1.
*   **Move 4 (100):** The 3rd bit is 1. Move disk 3.

This provides a non-recursive way to solve the puzzle.

## Visualizing the Problem Space

Let's imagine a graph where every possible legal arrangement of disks is a node, and a move from one arrangement to another is an edge.
*   For a 1-disk puzzle, there are 3 possible positions, forming a triangle.
*   For a 2-disk puzzle, the graph of all possible states also forms a triangle.
*   For a 3-disk puzzle, the graph of all states is a larger triangle, composed of three smaller triangular graphs of the 2-disk problem.

This structure, where a shape is composed of smaller copies of itself, is the hallmark of a **fractal**.

## The Sierpinski Triangle

The fractal that emerges from the Tower of Hanoi problem is the **Sierpinski triangle**. This is a fractal that can be constructed by:
1.  Starting with an equilateral triangle.
2.  Dividing it into four smaller congruent equilateral triangles and removing the central one.
3.  Repeating step 2 for each of the remaining smaller triangles indefinitely.

### Sierpinski, Pascal, and Binary

The Sierpinski triangle also appears in other areas of mathematics. For example, if you take Pascal's triangle and color all the odd numbers black and all the even numbers white, you get a Sierpinski triangle. This is because the parity of the numbers in Pascal's triangle is related to binary representations and powers of 2, which also govern the Tower of Hanoi.

The deep connection is that the recursive structure of the Tower of Hanoi solution ($T_n = 2T_{n-1} + 1$) is mirrored in the geometric construction of the Sierpinski triangle.

## More on

* [Binary, Hanoi and Sierpinski: 3Blue1Brown](https://www.youtube.com/watch?v=2SUvWfNJSsM)
* [The Tower of Hanoi on Wikipedia](https://en.wikipedia.org/wiki/Tower_of_Hanoi)
* [The Sierpinski Triangle on Wikipedia](https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle)
