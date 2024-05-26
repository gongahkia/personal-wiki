> [!IMPORTANT] 
> Continue from Chapter 4 Number Theory.

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

> *eg.* Suppose the claim "If M is regular, then M is paracompact or M is not Lindelof."

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

Mathematical proofs adhere to certain outlines for proving. 

1. Direct proof
2. Examples, Counter-examples
3. Proof by contrapositive

### Summary

An overview of proving methods for the given claim.

1. **Proving** Universal claim
    * Need to prove universality of claim for all proposed values within the given set
    * Construct general argument with variables (eg. $x$, $y$, $z$) in proof
2. **Disproving** Universal claim
    * Only need prove existential statement of claim failing
    * Find the specific counterexample 
3. **Proving** Existential claim
    * Only need prove existential statement
    * Find the specific example
4. **Disproving** Existential claim
    * Need to prove universality of counterclaim for all proposed values within the given set
    * Construct general argument with variables (eg. $x$, $y$, $z$) in proof

In table form...

| | Prove | Disprove |
| :---: | :---: | :---: |
| **Universal claim** | general argument | specific counter-example |
| **Existential claim** | specific example | general argument |

* Both proofs start by picking an element $x$ from the domain of quantification.
    * *General argument*: $x$ is a random element whose identity isn't uniquely specified
    * *Specific example* or *counter-example*: $x$ is a specific concrete value

### Direct Proof

* Start from **known** information *(variable declarations, definitions, hypothesis)*
* Move to information that has to be **proved**

##### Example 1

> *eg.* Prove the claim "For every rational number $q$, $2q$ is rational."

1. **Identify** the claim.

> This is a universal claim.
>   
> This claim is of the form $\forall x \in A, P(x)$.

2. **Definition(s)** within the claim.

> A real number $r$ is rational if there are integers $m$ and $n$ where $n \neq 0$, such that $r = \frac{m}{n}$.

3. **Substitution** of value(s) into definition.

> Let $q$ be any rational number. From the definition of "rational", we know that $q = \frac{m}{n}$ where $m$ and $n$ are integers and $n \neq 0$. As such, $2q = 2\frac{m}{n} = \frac{2m}{n}$. Since $m$ is an integer, so is $2m$. As such, $2q$ is a ratio of two integers, fulfilling the definition of "rational".

##### Example 2

> *eg.* Prove the claim "For any integer $k$, if $k$ is odd then $k^2$ is odd."

1. **Identify** the claim.

> This is a universal claim.
>   
>  This claim is of the form $\forall x \in \mathbb{Z}, P(x) \rightarrow Q(x)$.

2. **Definition(s)** within the claim.

> An integer $n$ is even if there is an integer $m$ such that $n = 2m$.
>  
> An integer $n$ is odd if there is an integer $m$ such that $n = 2m + 1$.

3. **Substitution** of value(s) into definition.

> Let $k$ be any integer and suppose that $k$ is odd. We now need to prove that $k^2$ is odd. Since $k$ is odd, there exists an integer $j$ such that $k = 2j + 1$.
> 
> Therefore, $k^2 = (2j + 1)^2 = 4j^2 + 4j + 1 = 2(2j^2 + 2j) + 1$.
> 
> Since $j$ is an integer, $2j^2 + 2j$ is also an integer that we can call $p$.
>
> Therefore, $k^2 = 2p + 1$, so by the definition of odd, $k^2$ is odd.

##### Example 3

> *eg.* Prove the claim "There is an integer $k$ such that $k^2 = 0$".

1. **Identify** the claim.

> This is an existential claim.
> 
>  This claim is of the form $\exists x \in A, P(x)$.

2. **Choose a concrete value** that proves the point.

> $0$ is such an integer. So the statement is true.

##### Example 4

> *eg.* Disprove the claim "Every rational number $q$ has a multiplicative inverse."

1. **Identify** the claim.

> This is a universal claim.
> 
>  This claim is of the form $\forall x \in A, P(x)$.

2. **Choose a concrete value** that disproves the point.

> Defining what a multiplicative inverse is, if $p$ and $r$ are real numbers, $r$ is a multiplicative inverse for $q$ if $qr = 1$.
>  
> Given that $0$ has no inverse, the claim is false.

##### Example 5

> *eg.* Disprove the claim "There is an integer $k$ such that $k^2 + 2k + 1 \lt 0$".

1. **Identify** the claim.

> This is an existential claim.
>  
> This claim is of the form $\exists x \in A, P(x)$.

2. **Identify** the counterclaim we need to prove.

> Therefore, we need to prove that "For every integer $k$, $k^2 + 2k + 1 \geq 0$".

3. **Substitution** of value(s) into counterclaim.

> Let $k$ be an integer.
>  
> $k^2 + 2k + 1$ can be factorised to be $(k + 1)^2$.
>  
> $(k + 1)^2 \geq 0$ since the square of any real number is non-negative, so $k^2 + 2k + 1 \geq 0$.
>   
> Therefore the original claim is false.

##### Example 6

> *eg.* Prove the claim "For any integers $m$ and $n$, if $m$ and $n$ are perfect squares, then so is $mn$".

1. **Identify** the claim.

> This is a universal claim.
>  
> This claim is of the form $\forall x,y \in A, P(x,y)$.

2. **Definition(s)** within the claim.

> An integer $n$ is a perfect square if $n = k^2$ for some integer $k$.

3. **Substitution** of value(s) into definition.

> Let $m$ and $n$ be integers and suppose that $m$ and $n$ are perfect squares.
>  
> By the definition of "perfect square", $m = k^2$ and $n = j^2$, so $mn = k^2j^2$, which can be factorised to $(kj)^2$.
>  
> Since $k$ and $j$ are both integers, so is $kj$. 
>  
> Since $mn$ is the square of the integer $kj$, $mn$ is a perfect square and the claim is true.

##### Example 7

> *eg.* Prove the claim "For all integers $j$ and $k$, if $j$ and $k$ are odd, then $jk$ is odd".

1. **Identify** the claim.

> This is a universal claim.
>  
> This claim is of the form $\forall x,y \in A, P(x,y)$.

2. **Definition(s)** within the claim.

> An integer $n$ is odd if there is an integer $m$ such that $n = 2m + 1$.

3. **Substitution** of value(s) into definition.

> Let $j$ and $k$ be integers and suppose they are both odd. 
>   
> Because $j$ is odd, there is an integer $p$ such that $j = 2p + 1$. Similarly, there is an integer $q$ such that $k = 2q + 1$.
>   
> Simplifying the expression, $jk = (2p + 1)(2q + 1) = 4pq + 2p + 2q + 1 = 2(2pq + p + q) + 1$. 
>  
> Given that both $p$ and $q$ are integers, $2pq + p + q$ is also an integer which we call $m$.
>  
> As such, $jk = 2m + 1$, so $jk$ is odd and the claim is true.

#### Proof by cases

* Used when given information results in *two or more* separate possibilities
* Do part of the proof *two or more times*, once for **each of the possibilities**

> [!TIP]  
> It is okay if the cases overlap, but the cases must together cover all possibilities.

##### Example 1

> *eg.* Prove the claim "For all integers $j$ and $k$, if $j$ is even or $k$ is even, then $jk$ is even".

1. **Identify** the claim.

> This is a universal claim.
>  
> This claim is of the form $\forall x \in A \lor \forall y \in A, P(x, y)$.

2. **Definition(s)** within the claim.

> An integer $n$ is even if there is an integer $m$ such that $n = 2m$.

3. **Handle** each possible case.

> Let $j$ and $k$ be integers and suppose that $j$ is even or $k$ is even. 
>  
> There are two cases.
>  
> Case 1: $j$ is even. Then $j = 2m$, where $m$ is an integer. As such, the expression simplifies to $jk = (2m)k = 2(mk)$. Since $m$ and $k$ are integers, so is $mk$. Therefore, $jk$ must be even.
>  
> Case 2: $k$ is even. Then $k = 2n$, where $n$ is an integer. As such, the expression simplifies to $jk = j(2n) = 2(nj)$. Since $n$ and $j$ are integers, so is $nj$. Therefore, $jk$ must be even.

4. **Summarise**

> So $jk$ is even in both cases, and the claim is true.

### Rephrasing claims

* Used when given claim is not in a *good form* for direct proof
* Rephrase the claim to be **readable**
* Rephrase the claim using **logical equivalence**
    * DeMorgan's laws
    * Proof by contrapositive

##### Example 1

> *eg.* Prove the claim "There is no integer $k$ such that $k$ is odd and $k^2$ is even".

1. **Rephrase** the claim to be readable

> The claim is really saying "For every integer $k$, it is not the case that $k$ is odd and $k^2$ is even".

2. **Rephrase** the claim using logical equivalence

> Applying DeMorgan's laws, this is also equivalent to "For every integer $k$, $k$ is not odd or $k^2$ is not even".
>   
> This is effectively the same as "For every integer $k$, $k$ is not odd or $k^2$ is odd".
>  
> Applying the logical equivalence that $\neg p \lor q \equiv p \rightarrow q$, this can be restated as "For every integer $k$, if $k$ is odd then $k^2$ is odd."

The following steps merely involve direct proof of a universal claim as covered above.

#### Proof by contrapositive

* Contrapositive of the claim $\forall x, P(x) \rightarrow Q(x)$ is $\forall x, \neg Q(x) \rightarrow \neg P(x)$
* Proving the contrapositive of a claim allows the inference  that the *original claim* is true

##### Example 1

> *eg.* Prove the claim "For any integers $a$ and $b$, if $a + b \geq 15$, then $a \geq 8$ or $b \geq 8$".

1. **Rephrase** the claim using logical equivalence by way of Proof by contrapositive

> The contrapositive of the claim is "For any integers $a$ and $b$, if it's not the case that $a \geq 8$ or $b \geq 8$, then it's not the case that $a + b \geq 15$".
>  
> Simplifying that further with logical equivalence, that is equivalent to "For any integers $a$ and $b$, if $a \lt 8$ and $b \lt 8$, then $a + b \lt 15$".

2. **Prove** the contrapositive

> We will now prove the contrapositive of the claim. That is, that for any integers $a$ and $b$, if $a \lt 8$ and $b \lt 8$, then $a + b \lt 15$.
>  
> Suppose that $a$ and $b$ are integers such that $a \lt 8$ and $b \lt 8$, this implies that $a \leq 7$ and $b \leq 7$.
>  
> Therefore, $a + b \leq 14$. This implies that $a + b \lt 15$, proving the contrapositive, so the original claim is true.

##### Example 2

> *eg.* Prove the claim "For any integer $k$, if $3k + 1$ is even, then $k$ is odd".

1. **Rephrase** the claim using logical equivalence by way of Proof by contrapositive

> Rephrasing the claim as the contrapositive returns "For any integer $k$, if $k$ is even, $3k + 1$ is odd".

2. **Prove** the contrapositive

> We will now prove the contrapositive of the claim. That is, that for any integer $k$, if $k$ is even, $3k + 1$ is odd.
>   
> An integer $x$ is even if there is an integer $y$ such that $x = 2y$.
>  
> An integer $x$ is odd if there is an integer $y$ such that $x = 2y + 1$.
> 
> Suppose that $k$ is an integer and $k$ is even, then $k = 2m$ where $m$ is an integer. Simplifying the expression, $3k + 1 = 3(2m) + 1 = 2(3m) + 1$. Since $m$ is an integer, so is $3m$.
>  
> Therefore, $3k + 1$ must be odd, proving the contrapositive so the original claim is true.

## Chapter 4: Number Theory

## More on

* [Building Blocks for Theoretical Computer Science](https://mfleck.cs.illinois.edu/building-blocks/index-sp2020.html) by Margaret M Fleck
