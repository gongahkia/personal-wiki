# `Category theory`

The branch of mathematics dealing with **abstract structures** and the **relationships between them**.

Covers...

* Categories, Functors, and Natural Transformations
* Monoids, Product and Sum Types
* Limits and Colimits
* Functor Categories and Adjunctions
* Monads 
* Initial and Terminal Objects
* Equivalence of Categories
* Yoneda Lemma
* Representable Functors

## Definitions

1. **Category**
    * A category $\mathcal{C}$ consists of:
        * **Objects**: Denoted by symbols such as $A, B, C, \ldots$
        * **Morphisms** (or **Arrows**): Denoted by $f: A \to B$, representing a relationship from object $A$ to object $B$
        * **Composition**: For morphisms $f: A \to B$ and $g: B \to C$, the composition $g \circ f: A \to C$
        * **Identity Morphisms**: For each object $A$, there is an identity morphism $id_A: A \to A$ such that $id_A \circ f = f$ and $g \circ id_A = g$ for all $f$ and $g$ involving $A$
        * **Associativity**: Composition of morphisms is associative: $(h \circ g) \circ f = h \circ (g \circ f)$

2. **Functor**
    * A functor $F: \mathcal{C} \to \mathcal{D}$ between categories $\mathcal{C}$ and $\mathcal{D}$ consists of:
        * A mapping of objects: For each object $A \in \mathcal{C}$, there is an object $F(A) \in \mathcal{D}$
        * A mapping of morphisms: For each morphism $f: A \to B$ in $\mathcal{C}$, there is a morphism $F(f): F(A) \to F(B)$ in $\mathcal{D}$
        * Preservation of composition and identity: $F(g \circ f) = F(g) \circ F(f)$ and $F(id_A) = id_{F(A)}$

3. **Natural Transformation**
    * A natural transformation $\eta$ between functors $F, G: \mathcal{C} \to \mathcal{D}$ consists of:
        * A collection of morphisms $\eta_A: F(A) \to G(A)$ for each object $A \in \mathcal{C}$
        * Such that for any morphism $f: A \to B$ in $\mathcal{C}$, the following diagram commutes:
          $$
          \begin{array}{ccc}
          F(A) & \xrightarrow{\eta_A} & G(A) \\
          \downarrow{F(f)} & & \downarrow{G(f)} \\
          F(B) & \xrightarrow{\eta_B} & G(B)
          \end{array}
          $$

4. **Monoid**
    * A monoid $(M, \cdot, e)$ is a set $M$ equipped with a binary operation $\cdot: M \times M \to M$ and an identity element $e \in M$ such that:
        * **Associativity**: $(a \cdot b) \cdot c = a \cdot (b \cdot c)$ for all $a, b, c \in M$
        * **Identity**: $a \cdot e = a$ and $e \cdot a = a$ for all $a \in M$

5. **Product and Sum Types**
    * **Product Types**: Denoted by $A \times B$, represent pairs of elements from sets $A$ and $B$.
    * **Sum Types**: Denoted by $A + B$, represent elements that belong to either set $A$ or set $B$.

6. **Limits and Colimits**
    * **Limit**: The limit of a diagram (a functor from an index category to $\mathcal{C}$) is an object that captures the idea of the “best approximation” to the diagram.
    * **Colimit**: Dual to limits, capturing the “coarsest” structure that can be mapped to from the diagram.

7. **Functor Categories**
    * The category **Fun**$(\mathcal{C}, \mathcal{D})$ consists of all functors from $\mathcal{C}$ to $\mathcal{D}$ and natural transformations between them.

8. **Adjunctions**
    * An adjunction between categories $\mathcal{C}$ and $\mathcal{D}$ consists of a pair of functors $F: \mathcal{C} \to \mathcal{D}$ and $G: \mathcal{D} \to \mathcal{C}$ such that:
        * For each pair of objects $A \in \mathcal{C}$ and $B \in \mathcal{D}$, there is a natural isomorphism between $\text{Hom}_{\mathcal{D}}(F(A), B)$ and $\text{Hom}_{\mathcal{C}}(A, G(B))$.

9. **Monads**
    * A monad on a category $\mathcal{C}$ consists of:
        * A functor $T: \mathcal{C} \to \mathcal{C}$
        * Two natural transformations: $\eta: 1_{\mathcal{C}} \to T$ (unit) and $\mu: T^2 \to T$ (multiplication) satisfying certain coherence conditions.

10. **Initial and Terminal Objects**
    * **Initial Object**: An object $I$ in a category $\mathcal{C}$ such that for every object $A \in \mathcal{C}$, there exists a unique morphism from $I$ to $A$.
    * **Terminal Object**: An object $T$ in a category $\mathcal{C}$ such that for every object $A \in \mathcal{C}$, there exists a unique morphism from $A$ to $T$.

11. **Equivalence of Categories**
    * Two categories $\mathcal{C}$ and $\mathcal{D}$ are said to be **equivalent** if there exist functors $F: \mathcal{C} \to \mathcal{D}$ and $G: \mathcal{D} \to \mathcal{C}$ such that $G \circ F$ is naturally isomorphic to the identity functor on $\mathcal{C}$, and $F \circ G$ is naturally isomorphic to the identity functor on $\mathcal{D}$.

12. **Yoneda Lemma**
    * The Yoneda Lemma states that for any category $\mathcal{C}$, object $A \in \mathcal{C}$, and functor $F: \mathcal{C} \to \textbf{Set}$, there is a natural isomorphism:
      $$
      \text{Nat}(\text{Hom}_{\mathcal{C}}(-, A), F) \cong F(A)
      $$
      where:
      * $\text{Hom}_{\mathcal{C}}(-, A)$ is the functor that maps an object $X$ to the set of morphisms $\text{Hom}_{\mathcal{C}}(X, A)$.
      * $\text{Nat}(\text{Hom}_{\mathcal{C}}(-, A), F)$ denotes the set of natural transformations from $\text{Hom}_{\mathcal{C}}(-, A)$ to $F$.

13. **Representable Functors**
    * A functor $F: \mathcal{C} \to \textbf{Set}$ is called **representable** if it is naturally isomorphic to the hom-functor $\text{Hom}_{\mathcal{C}}(C, -)$ for some object $C \in \mathcal{C}$.

## More on

* [Category theory for Programmers](https://unglueit-files.s3.amazonaws.com/ebf/e90890f0a6ea420c9825657d6f3a851d.pdf) by Bartosz Milewski
* [Abstract Algebra in 3 Hours](https://www.slideshare.net/slideshow/abstract-algebra-in-3-hours/70763563) by Ashwin Rao
* [Category theory made easy with (ugly) pictures](https://www.slideshare.net/slideshow/category-theory-made-easy-with-ugly-pictures-73745930/73745930) by Ashwin Rao
* [Category theory in Context](https://www.maths.ox.ac.uk/system/files/attachments/Category%20Theory%20in%20Context.pdf) by Emily Riehl and Charles A. Stewart
* [A Gentle Introduction to Category theory (the calculational approach)](https://maartenfokkinga.github.io/utwente/mmf92b.pdf) by Maarten M Fokkinga
* [What is Category theory Anyway?](https://www.math3ma.com/blog/what-is-category-theory-anyway) by math3m
* [Basic Category theory](https://arxiv.org/pdf/1612.09375) by Tom Leinster
* [Category theory for Dummies](https://homepages.inf.ed.ac.uk/jcheney/presentations/ct4d1.pdf) by James Cheney
* [Good Introduction to Category theory?](https://www.reddit.com/r/math/comments/r4yoc0/good_introduction_to_category_theory/) by r/math
* [An Introduction to the Language of Category theory](http://www.ms.lt/derlius/StevenRoman-AnIntroductionToTheLanguageOfCategoryTheory.pdf) by Steven Roman
* [Introduction to Category theory and Categorical logic](https://www2.mathematik.tu-darmstadt.de/~streicher/CTCL.pdf) by Thomas Streicher
