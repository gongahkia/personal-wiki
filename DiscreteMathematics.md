> [!IMPORTANT] 
> Continue from Chapter 3 Proofs.

# `Discrete mathematics`

**Read** and **write** mathematical proofs and apply them in Computer Science.

Covers...

* Basic mathematical objects, notation and techniques
* Propositional and Predicate logic, Sets, Functions, Relations, Modular Arithmetic, Counting, Graphs, Trees
* Algorithm analysis and complexity, Automata theory, Computability

## Chapter 1: Math Review

* Refresher on general concepts

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
        * $\frac{n!}{k!(n-k)!}$ can be abbreviated as $n \choose k$ (read as $n$ choose $k$)
19. Floor and Ceiling
    * $\lfloor$ $\rfloor$ denotes the **floor** function, which takes a real number $x$ and returns an integer no bigger than $x$ (rounds down)
        * eg. $\lfloor{3.75}\rfloor = 3$
        * eg. $\lfloor{3}\rfloor = 3$
        * eg. $\lfloor{-3.75}\rfloor = -4$
    * $\lceil$ $\rceil$ denotes the **ceiling** function, which takes a real number $x$ and returns an integer no smaller than $x$ (rounds up)
        * eg. $\lceil{3.75}\rceil = 4$
        * eg. $\lceil{3}\rceil = 3$
        * eg. $\lceil{-3.75}\rceil = -3$
20. Summation
    * assuming $a_i$ is a formula that depends on $i$, then
    $$\sum_{i=1}^{n}a_i = a_1 + a_2 + a_3 + \ldots + a_n$$
        * eg.
    $$\sum_{i=1}^{n}\frac{1}{2^i} = \frac{1}{2} + \frac{1}{4} + \frac{1}{8} + \ldots + \frac{1}{2^n}$$
    * **closed form** notation is employed for certain sums
        * general formula
    $$\sum_{k=0}^{n}{r^k} = \frac{r^{n+1}-1}{r-1}$$
        * geometric series pattern
    $$\sum_{i=1}^{n}\frac{1}{2^i} = 1 - \frac{1}{2^n}$$
        * also this one
    $$\sum_{i=1}^{n}{i} = \frac{n(n+1)}{2}$$
    * products are written with the same notation
        * eg. 
    $$\prod_{k=1}^{n}\frac{1}{k} = \frac{1}{1}\cdot\frac{1}{2}\cdot\frac{1}{3}\cdot\ldots\cdot\frac{1}{n}$$
21. $\epsilon$ denotes a string of length 0 containing no characters
22. $\alpha\beta$ denotes the concatenation of the 2 strings $\alpha$ and $\beta$
    * eg. where $\alpha = \text{water}$ and $\beta = \text{melon}$ then $\alpha\beta = \text{watermelon}$
    * eg. where $\alpha = \text{shit}$ then $\alpha\text{s} = \text{shits}$
23. A bit string is a string consisting of the $0$ and $1$ characters.
24. Supposing $A$ is a set of characters, then $A^*$ is a set of all finite-length strings containing characters from $A$.
    * eg. where $A$ contains all lower-case alphabets then $A^*$ contains strings like $\text{onion}$, $\text{e}$, $\text{kkkkmmmmmmbb}$ **and** the empty string $\epsilon$
25. Rudimentary regex support
    * $\text{a}\mid\text{b}$ means either the character $a$ or $b$
    * $\text{a}^*$ means zero or more copies of the character $\text{a}$
    * $($ $)$ show grouping
        * eg. $\text{ab}^*$ specifies all strings that start with $\text{a}$ followed by zero or more $\text{b}$'s
        * eg. $\text{c}(\text{a}\mid\text{b})^*\text{c}$ specifies all strings that start with a $\text{c}$, followed by zero or more characters that are either $\text{a}$ or $\text{b}$, followed by one $\text{c}$

## Chapter 2: Logic

* Propositional logic, Predicate logic

### Definitions

#### Propositional logic

1. $0$: False
2. $1$: True
3. $\neg$: Not
4. $\land$: And
5. $\lor$: Inclusive Or
6. $\oplus$: Exclusive Or
7. $\rightarrow$: Implies
8. $\leftrightarrow$: Biconditional
9. $\equiv$: Logical Equivalence
10. $($ $)$: Groups logically related propositions together

#### Predicate logic

11. $\forall$: Universal quantifier
12. $\exists$: Existental quantifier
13. $\exists!$: Unique Existence quantifier

> [!NOTE]  
> Moving forward, all truth tables will be represented assuming $A$, $B$ and $C$ are all propositions.

### Propositions

* Proposition: statement which is either true or false 
    * states a claim
    * cannot be a question
    * cannot contain variables *(eg. $x\leq9$ is not a proposition)*

#### Complex propositions

* Propositions can be chained together with $\neg$, $\land$ and $\lor$ to create complex propositions.
* Complex propositions are represented by truth tables.

**And truth table**

$$\begin{array}{|c|c|c|}
\hline
A & B & A \land B \\
\hline
0 & 0 & 0 \\
0 & 1 & 0 \\
1 & 0 & 0 \\
1 & 1 & 1 \\
\hline
\end{array}$$

**Inclusive Or truth table**

$$\begin{array}{|c|c|c|}
\hline
A & B & A \lor B \\
\hline
0 & 0 & 0 \\
0 & 1 & 1 \\
1 & 0 & 1 \\
1 & 1 & 1 \\
\hline
\end{array}$$

**Exclusive Or truth table**

$$\begin{array}{|c|c|c|}
\hline
A & B & A \oplus B \\
\hline
0 & 0 & 0 \\
0 & 1 & 1 \\
1 & 0 & 1 \\
1 & 1 & 0 \\
\hline
\end{array}$$

#### Implication

* Two propositions can be joined into a conditional statement in the format of "if *Proposition 1*, then *Proposition 2*".
* Can also be phrased as...
    * "*Proposition 1* implies *Proposition 2*"
    * "*Proposition 2* follows *Proposition 1*"
* *Proposition 1* is called the **hypothesis**.
* *Proposition 2* is called the **conclusion**.

**Implication truth table**

$$\begin{array}{|c|c|c|}
\hline
A & B & A \rightarrow B \\
\hline
0 & 0 & 1 \\
0 & 1 & 1 \\
1 & 0 & 0 \\
1 & 1 & 1 \\
\hline
\end{array}$$

#### Converse

* The converse of $A \rightarrow B$ is $B \rightarrow A$.
* Note that implications frequently only apply in one direction.
    * Therefore, there is **no** guarantee that since $A \rightarrow B$, then $B \rightarrow A$.

**Converse truth table**

$$\begin{array}{|c|c|c|}
\hline
A & B & B \rightarrow A \\
\hline
0 & 0 & 1 \\
0 & 1 & 0 \\
1 & 0 & 1 \\
1 & 1 & 1 \\
\hline
\end{array}$$

#### Biconditional

* Biconditional covers a situation where $A \rightarrow B$ and $B \rightarrow A$.
* $A$ and $B$ are true under exactly the same conditions.

**Biconditional truth table**

$$\begin{array}{|c|c|c|}
\hline
A & B & A \leftrightarrow B \\
\hline
0 & 0 & 1 \\
0 & 1 & 0 \\
1 & 0 & 0 \\
1 & 1 & 1 \\
\hline
\end{array}$$

#### Contrapositive

* Contrapositive just means to negate both the hypothesis and conclusion.
    * eg. the contrapositive of $A \rightarrow B$ would be $\neg B \rightarrow \neg A$

**Contrapositive truth table**

$$\begin{array}{|c|c|c|c|c|}
\hline
A & B & \neg A & \neg B & \neg B \rightarrow \neg A \\
\hline
0 & 0 & 1 & 1 & 1 \\
0 & 1 & 1 & 0 & 1 \\
1 & 0 & 0 & 1 & 0 \\
1 & 1 & 0 & 0 & 1 \\
\hline
\end{array}$$

##### Rules for complex statements

Follow these rules when constructing truth tables.

1. Apply $\neg$ (not operator) first.
2. Apply $\land$ (and) and $\lor$ (inclusive or) operators.
3. Apply $\rightarrow$ (implication) and $\leftrightarrow$ (biconditional) operators.
4. Use $($ $)$ (parantheses) to group logically related propositions together.

#### Logical Equivalence

* 2 propositions are logically equivalent when they are true for the same input values.
    * eg. $A \rightarrow B$ is logically equivalent to $\neg A \lor B$, so it can be said that $A \rightarrow B \equiv \neg A \lor B$

The proof is given that

**$A \rightarrow B$ truth table**

$$\begin{array}{|c|c|c|}
\hline
A & B & A \rightarrow B \\
\hline
0 & 0 & 1 \\
0 & 1 & 1 \\
1 & 0 & 0 \\
1 & 1 & 1 \\
\hline
\end{array}$$

and 

**$\neg A \lor B$ truth table**

$$\begin{array}{|c|c|c|c|}
\hline
A & B & \neg A & \neg A \lor B \\
\hline
0 & 0 & 1 & 1\\
0 & 1 & 1 & 1\\
1 & 0 & 0 & 0\\
1 & 1 & 0 & 1\\
\hline
\end{array}$$

it can be said that $A \rightarrow B$ and $\neg A \lor B$ are logically equivalent.

* Other well-known logical equivalences are...
    1. De Morgan's Laws
        * $\neg (A \land B) \equiv \neg A \lor \neg B$
        * $\neg(A \lor B) \equiv \neg A \land \neg B$
    2. Some useful ones
        * $A \land \neg A \equiv 0$
        * $A \land B \equiv B \land A$
        * $A \lor (B \land C) \equiv (A \lor B) \land (A \lor C)$
        * $A \land (B \lor C) \equiv (A \land B) \lor (A \land C)$
        * $\neg (\neg A) \equiv A$
        * $\neg(A \rightarrow B) \equiv A \land \neg B$
    3. [Actual cheatsheet](https://courses.cs.washington.edu/courses/cse311/24wi/resources/reference-logical_equiv.pdf)

#### Applying propositional logic

Below is a worked example of applying propositional logic by way of negation.

> *eg. Suppose the claim "If M is regular, then M is paracompact or M is not Lindelof."*

1. Convert the claim to shorthand.

> Let $A$ be "M is regular"  
> Let $B$ be "M is paracompact"  
> Let $C$ be "M is Lindelof"  
>  
> So the claim is now $A \rightarrow (B \lor \neg C)$

2. Negate the claim.

> Negation of $A \rightarrow (B \lor \neg C)$ would be $\neg(A \rightarrow (B \lor \neg C))$

3. Simplify by applying known logical equivalences.

> Applying the following...  
> $\neg (\neg A) \equiv A$  
> $\neg(A \land B) \equiv \neg A \lor \neg B$  
> $\neg(A \lor B) \equiv \neg A \land \neg B$  
> $\neg(A \rightarrow B) \equiv A \land \neg B$   
>  
> Therefore...  
> $\neg(A \rightarrow (B \lor \neg C)) \equiv A \land \neg(B \lor \neg C) \equiv A \land \neg B \land \neg\neg C \equiv A \land \neg B \land C$

4. Convert the shorthand back to a claim.

> Therefore, the negation of our original claim is "M is regular and M is not paracompact and M is Lindelof."

> [!NOTE]  
> Moving forward, assume that $P()$, $Q()$ and $R()$ are all predicates.

### Predicates

* Predicate: statement that becomes either true or false when its variables are substituted for values
    * eg. $P(x)$ is a predicate where $x^2 \geq 10$
    * eg. $Q(y)$ is a predicate where "the weather is $y$"
    * eg. $R(z)$ is a predicate where "you are smelly when $z$"
* Predicates are useful for making **general statements** about what happens when we subtitute a variety of values for variables.
    * eg. "$P(x)$ is true for every $x$"

#### Quantifiers

1. Universal quantifier: *"for all"*
    * **all** objects have such properties
    * eg. "for all integers $x$, $x^2 \geq 10$"
2. Existential quantifier: *"there exists"*
    * **at least one** object have such properties
    * eg. "there exists an integer $x$ such that $5 \lt x \lt 100$"
3. Unique Existence quantifier: *"there is"*
    * **only one** object have such properties
    * eg. "there is a unique integer $x$ such that $x^2 = 0$"

#### Quantifier notation 

The quantifiers are represented as...

* Universal quantifier: $\forall$
* Existential quantifier: $\exists$
* Unique Existence quantifier: $\exists!$

Below are some worked examples of understanding quantifier notation.

*eg.* $\forall x \in \mathbb{R}, x^2 + 3 \geq 0$

1. $\forall$ is the quantifier
2. $x \in \mathbb{R}$ specifies the variable and the set it belongs to 
    * $x$ is the variable
    * $\in$ denotes membership
    * $\mathbb{R}$ is the set of all real numbers
3. $x^2 + 3 \geq 0$ is the predicate we are asserting
4. Therefore this reads as "For all real numbers $x$, $x^2 + 3 \geq 0$."

*eg.* $\exists y \in \mathbb{R}, y = \sqrt{2}$

1. $\exists$ is the quantifier
2. $y \in \mathbb{R}$ specifies the variable and the set it belongs to
    * $y$ is the variable
    * $\in$ denotes membership
    * $\mathbb{R}$ is the set of all real numbers
3. $y = \sqrt{2}$ is the predicate we are asserting
4. Therefore this reads as "There exists a real number $y$ such that $y = \sqrt{2}$."

*eg.* $\exists!x \in \mathbb{R}, x^2 = 0$

1. $\exists!$ is the quantifier
2. $x \in \mathbb{R}$ specifies the variable and the set it belongs to
    * $x$ is the variable
    * $\in$ denotes membership
    * $\mathbb{R}$ is the set of all real numbers
3. $x^2 = 0$ is the predicate we are asserting
4. Therefore this reads as "There is a unique real number $x$ such that $x^2 = 0$."

> [!IMPORTANT]  
> Multiple quantifiers can be chained together as in the following shorthand.  
>   
> $\forall x \in \mathbb{R}, \forall y \in \mathbb{R}, x + y \geq x$  
>  
> can be shortened to become 
>   
> $\forall x, y \in \mathbb{R}, x + y \geq x$
>   
> and both retain the meaning of "for all real numbers $x$ and $y$, $x + y \geq x$". (Note the predicate itself isn't true, but that's not important here.)

#### Quantifiers and logical equivalences

Here are some logical equivalences derived from the negation of quantifiers.

* $\neg (\forall x, P(x)) \equiv \exists x, \neg P(x)$
* $\neg(\exists x, P(x)) \equiv \forall x, \neg P(x)$
* $\neg (\forall x, P(x) \rightarrow (Q(x) \land R(x))) \equiv \exists x, P(x) \land (\neg Q(x) \lor \neg R(x))$

## Chapter 3: Proofs

## More on

* [Building Blocks for Theoretical Computer Science](https://mfleck.cs.illinois.edu/building-blocks/index-sp2020.html) by Margaret M Fleck