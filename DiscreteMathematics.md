> [!NOTE] 
> Continue from Chapter 2.

# `Discrete mathematics`

**Read** and **write** mathematical proofs.

Covers...

* Basic mathematical objects, notation and techniques
* Propositional and Predicate logic, Sets, Functions, Relations, Modular Arithmetic, Counting, Graphs, Trees
* Algorithm analysis and complexity, Automata theory, Computability

## Chapter 1: Math Review

### Definitions

1. $\mathbb{Z}$ denotes the set of *all* negative and positive integers
    * $\mathbb{Z} = \{\ldots, -3, -2, -1, 0, 1, 2, 3, \ldots\}$
2. $\mathbb{N}$ denotes the set of *all* natural numbers (aka. non-negative integers)
    * non-negative integers: positive integers and zero
    * $\mathbb{N} = \{0, 1, 2, 3, \ldots\}$
3. $\mathbb{Z}^+$ or $\mathbb{N}^+$ denotes the set of *all* positive integers
    * positive integer: integer greater than zero
    * $\mathbb{Z}^+ = \{1, 2, 3, \ldots\}$
    * $\mathbb{N}^+ = \{1, 2, 3, \ldots\}$
4. $\mathbb{Q}$ denotes the set of *all* rational numbers
    * rational numbers: fractions which can be represented by $\frac{p}{q}$ where $q$ can't be zero **and** two fractions are the same number if they are the same when reduced to lowest terms
5. $\mathbb{R}$ denotes the set of *all* real numbers
    * real numbers: rational numbers **and** irrational numbers
    * irrational numbers: numbers which can be represented by $x$ assuming that $\sqrt{x}$ only returns the positive square root of $x$, **such as** $\sqrt{2}$, $\pi$, $e$
6. $\mathbb{C}$ denotes the set of *all* complex numbers
    * complex numbers: numbers of the form $a + bi$ where $a$ and $b$ are real numbers **and** $i=\sqrt{-1}$
7. Infinity ($\infty$) is not a number in standard mathematics.
8. $\in$ denotes membership to a set
    * eg. $x \in \mathbb{R}$ denotes $x$ is a real number
9. $\notin$ denotes absence from a set
    * eg. $y \notin \mathbb{Z}$ denotes $y$ is not an integer

To select a limited range of real numbers, we use an interval of the real line.

10. $[$ $]$ denotes a closed interval that is **inclusive** of its upper and lower bound
    * eg. $[a, b]$ denotes a set of all real numbers from $a$ to $b$, including $a$ and $b$
11. $($ $)$ denotes an open interval that is **exclusive** of its upper and lower bound
    * eg. $(a, b)$ denotes a set of all real numbers from $a$ to $b$, not including $a$ and $b$
12. $[$ $)$ and $($ $]$ both denote half-open intervals that are **inclusive** of one bound and **exclusive** of the other bound
    * $[a, b)$ and $(a, b]$ denote sets of real numbers from $a$ to $b$ that include either $a$ or $b$
13. $\mathbb{R}^2$ denotes the set of all **pairs** of real numbers
    * eg. $\mathbb{R}^2$ contains pairs like -2.3, 4.7
14. $\mathbb{R}^3$ denotes the set of all **triples** of real numbers
    * eg. $\mathbb{R}^3$ contains triples like 8, 7.3, -9
15. Exponentials
    * $b^0 = 1$ 
    * $b^{0.5} = \sqrt{b}$
    * $b^{-1} = \frac{1}{b}$
    * $b^xb^y = b^{x+y}$
    * $a^xb^x = (ab)^x$
    * $(b^x)^y = b^{xy}$
    * $b^{(x^y)}\neq(b^x)^y$
16. Logs
    * $y = b^x$ is $x = \log_b{y}$ **where** $b > 1$
    * $b^{\log_b(x)} = x$
    * $\log_b(xy) = \log_b{x} + \log_b{y}$
    * $\log_b{(x^y)} = y\log_b{x}$
    * $\log_b{x} = \log_a{x}\log_b{a}$ by applying change of base formula
    * $\log{x}$ with **no explicit base** taken to be $\log_2{x}$ since computer algorithms make heavy use of base-2 numbers and powers of 2
17. Factorial function
    * $k! = 1\cdot2\cdot3\cdot\ldots\cdot(k-1)\cdot{k}$
        * eg. $5! = 1\cdot2\cdot3\cdot4\cdot5 = 120$
    * $0! = 1$
18. Permutations and Combinations
    * set $S$ containing $n$ objects (all unique)
    * $n!$ permutations of these objects
        * permutation: number of ways to **arrange** objects in a particular order
    * $\frac{n!}{k!(n-k)!}$ ways to **choose** $k$ (unordered) elements from set $S$
        * $\frac{n!}{k!(n-k)!}$ can be abbreviated as $n \choose k$ (read as "$n$ choose $k$")
19. Others
    * $\lfloor$ $\rfloor$ denotes the **floor** function, which takes a real number $x$ and returns an integer no bigger than $x$ (rounds down)
        * eg. $\lfloor{3.75}\rfloor = 3$
        * eg. $\lfloor{3}\rfloor = 3$
        * eg. $\lfloor{-3.75}\rfloor = -4$
    * $\lceil$ $\rceil$ denotes the **ceiling** function, which takes a real number $x$ and returns an integer no smaller than $x$ (rounds up)
        * eg. $\lceil{3.75}\rceil = 4$
        * eg. $\lceil{3}\rceil = 3$
        * eg. $\lceil{-3.75}\rceil = -3$

## More on

* [Building Blocks for Theoretical Computer Science](https://mfleck.cs.illinois.edu/building-blocks/index-sp2020.html) by Margaret M Fleck