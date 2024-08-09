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
    * A category ![C](https://latex.codecogs.com/png.latex?%5Cmathcal%7BC%7D) consists of:
        * **Objects**: Denoted by symbols such as ![A](https://latex.codecogs.com/png.latex?A), ![B](https://latex.codecogs.com/png.latex?B), ![C](https://latex.codecogs.com/png.latex?C), $\ldots$
        * **Morphisms** (or **Arrows**): Denoted by ![f: A \to B](https://latex.codecogs.com/png.latex?f%3A%20A%20%5Cto%20B), representing a relationship from object ![A](https://latex.codecogs.com/png.latex?A) to object ![B](https://latex.codecogs.com/png.latex?B)
        * **Composition**: For morphisms ![f: A \to B](https://latex.codecogs.com/png.latex?f%3A%20A%20%5Cto%20B) and ![g: B \to C](https://latex.codecogs.com/png.latex?g%3A%20B%20%5Cto%20C), the composition ![g \circ f: A \to C](https://latex.codecogs.com/png.latex?g%20%5Ccirc%20f%3A%20A%20%5Cto%20C)
        * **Identity Morphisms**: For each object ![A](https://latex.codecogs.com/png.latex?A), there is an identity morphism ![id_A: A \to A](https://latex.codecogs.com/png.latex?id_A%3A%20A%20%5Cto%20A) such that ![id_A \circ f = f](https://latex.codecogs.com/png.latex?id_A%20%5Ccirc%20f%20%3D%20f) and ![g \circ id_A = g](https://latex.codecogs.com/png.latex?g%20%5Ccirc%20id_A%20%3D%20g) for all ![f](https://latex.codecogs.com/png.latex?f) and ![g](https://latex.codecogs.com/png.latex?g) involving ![A](https://latex.codecogs.com/png.latex?A)
        * **Associativity**: Composition of morphisms is associative: ![(h \circ g) \circ f = h \circ (g \circ f)](https://latex.codecogs.com/png.latex?%28h%20%5Ccirc%20g%29%20%5Ccirc%20f%20%3D%20h%20%5Ccirc%20%28g%20%5Ccirc%20f%29)

2. **Functor**
    * A functor ![F: \mathcal{C} \to \mathcal{D}](https://latex.codecogs.com/png.latex?F%3A%20%5Cmathcal%7BC%7D%20%5Cto%20%5Cmathcal%7BD%7D) between categories ![C](https://latex.codecogs.com/png.latex?%5Cmathcal%7BC%7D) and ![D](https://latex.codecogs.com/png.latex?%5Cmathcal%7BD%7D) consists of:
        * A mapping of objects: For each object ![A \in \mathcal{C}](https://latex.codecogs.com/png.latex?A%20%5Cin%20%5Cmathcal%7BC%7D), there is an object ![F(A) \in \mathcal{D}](https://latex.codecogs.com/png.latex?F%28A%29%20%5Cin%20%5Cmathcal%7BD%7D)
        * A mapping of morphisms: For each morphism ![f: A \to B](https://latex.codecogs.com/png.latex?f%3A%20A%20%5Cto%20B) in ![C](https://latex.codecogs.com/png.latex?%5Cmathcal%7BC%7D), there is a morphism ![F(f): F(A) \to F(B)](https://latex.codecogs.com/png.latex?F%28f%29%3A%20F%28A%29%20%5Cto%20F%28B%29) in ![D](https://latex.codecogs.com/png.latex?%5Cmathcal%7BD%7D)
        * Preservation of composition and identity: ![F(g \circ f) = F(g) \circ F(f)](https://latex.codecogs.com/png.latex?F%28g%20%5Ccirc%20f%29%20%3D%20F%28g%29%20%5Ccirc%20F%28f%29) and ![F(id_A) = id_{F(A)}](https://latex.codecogs.com/png.latex?F%28id_A%29%20%3D%20id_%7BF%28A%29%7D)

3. **Natural Transformation**
    * A natural transformation ![\eta](https://latex.codecogs.com/png.latex?%5Ceta) between functors ![F](https://latex.codecogs.com/png.latex?F), ![G: \mathcal{C} \to \mathcal{D}](https://latex.codecogs.com/png.latex?G%3A%20%5Cmathcal%7BC%7D%20%5Cto%20%5Cmathcal%7BD%7D) consists of:
        * A collection of morphisms ![\eta_A: F(A) \to G(A)](https://latex.codecogs.com/png.latex?%5Ceta_A%3A%20F%28A%29%20%5Cto%20G%28A%29) for each object ![A \in \mathcal{C}](https://latex.codecogs.com/png.latex?A%20%5Cin%20%5Cmathcal%7BC%7D)
        * Such that for any morphism ![f: A \to B](https://latex.codecogs.com/png.latex?f%3A%20A%20%5Cto%20B) in ![C](https://latex.codecogs.com/png.latex?%5Cmathcal%7BC%7D), the following diagram commutes:
          ![Diagram](https://latex.codecogs.com/png.latex?%5Cbegin%7Barray%7D%7Bccc%7D%0AF%28A%29%20%26%20%5Cxrightarrow%7B%5Ceta_A%7D%20%26%20G%28A%29%20%5C%5C%0A%5Cdownarrow%7BF%28f%29%7D%20%26%20%26%20%5Cdownarrow%7BG%28f%29%7D%20%5C%5C%0AF%28B%29%20%26%20%5Cxrightarrow%7B%5Ceta_B%7D%20%26%20G%28B%29%0A%5Cend%7Barray%7D)

4. **Monoid**
    * A monoid ![(M, \cdot, e)](https://latex.codecogs.com/png.latex?%28M%2C%20%5Ccdot%2C%20e%29) is a set ![M](https://latex.codecogs.com/png.latex?M) equipped with a binary operation ![\cdot: M \times M \to M](https://latex.codecogs.com/png.latex?%5Ccdot%3A%20M%20%5Ctimes%20M%20%5Cto%20M) and an identity element ![e \in M](https://latex.codecogs.com/png.latex?e%20%5Cin%20M) such that:
        * **Associativity**: ![(a \cdot b) \cdot c = a \cdot (b \cdot c)](https://latex.codecogs.com/png.latex?%28a%20%5Ccdot%20b%29%20%5Ccdot%20c%20%3D%20a%20%5Ccdot%20%28b%20%5Ccdot%20c%29) for all ![a, b, c \in M](https://latex.codecogs.com/png.latex?a%2C%20b%2C%20c%20%5Cin%20M)
        * **Identity**: ![a \cdot e = e \cdot a = a](https://latex.codecogs.com/png.latex?a%20%5Ccdot%20e%20%3D%20e%20%5Ccdot%20a%20%3D%20a) for all ![a \in M](https://latex.codecogs.com/png.latex?a%20%5Cin%20M)

5. **Product and Sum Types**
    * **Product Types**: Denoted by ![A \times B](https://latex.codecogs.com/png.latex?A%20%5Ctimes%20B), represent pairs of elements from sets ![A](https://latex.codecogs.com/png.latex?A) and ![B](https://latex.codecogs.com/png.latex?B).
    * **Sum Types**: Denoted by ![A + B](https://latex.codecogs.com/png.latex?A%20%2B%20B), represent elements that belong to either set ![A](https://latex.codecogs.com/png.latex?A) or set ![B](https://latex.codecogs.com/png.latex?B).

6. **Limits and Colimits**
    * **Limit**: The limit of a diagram (a functor from an index category to ![\mathcal{C}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BC%7D)) is an object that captures the idea of the “best approximation” to the diagram.
    * **Colimit**: Dual to limits, capturing the “coarsest” structure that can be mapped to from the diagram.

7. **Functor Categories**
    * The category **Fun** ![](https://latex.codecogs.com/png.latex?%5CFun%28%5Cmathcal%7BC%7D%2C%20%5Cmathcal%7BD%7D%29) consists of all functors from ![\mathcal{C}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BC%7D) to ![\mathcal{D}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BD%7D) and natural transformations between them.

8. **Adjunctions**
    * An adjunction between categories ![\mathcal{C}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BC%7D) and ![\mathcal{D}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BD%7D) consists of a pair of functors ![F: \mathcal{C} \to \mathcal{D}](https://latex.codecogs.com/png.latex?F%3A%20%5Cmathcal%7BC%7D%20%5Cto%20%5Cmathcal%7BD%7D) and ![G: \mathcal{D} \to \mathcal{C}](https://latex.codecogs.com/png.latex?G%3A%20%5Cmathcal%7BD%7D%20%5Cto%20%5Cmathcal%7BC%7D) such that:
        * For each pair of objects ![A \in \mathcal{C}](https://latex.codecogs.com/png.latex?A%20%5Cin%20%5Cmathcal%7BC%7D) and ![B \in \mathcal{D}](https://latex.codecogs.com/png.latex?B%20%5Cin%20%5Cmathcal%7BD%7D), there is a natural isomorphism between ![\text{Hom}_{\mathcal{D}}(F(A), B)](https://latex.codecogs.com/png.latex?%5Ctext%7BHom%7D_%7B%5Cmathcal%7BD%7D%7D%28F%28A%29%2C%20B%29) and ![\text{Hom}_{\mathcal{C}}(A, G(B))](https://latex.codecogs.com/png.latex?%5Ctext%7BHom%7D_%7B%5Cmathcal%7BC%7D%7D%28A%2C%20G%28B%29%29).

9. **Monads**
    * A monad on a category ![\mathcal{C}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BC%7D) consists of:
        * A functor ![T: \mathcal{C} \to \mathcal{C}](https://latex.codecogs.com/png.latex?T%3A%20%5Cmathcal%7BC%7D%20%5Cto%20%5Cmathcal%7BC%7D)
        * Two natural transformations: ![\eta: 1_{\mathcal{C}} \to T](https://latex.codecogs.com/png.latex?%5Ceta%3A%201_%7B%5Cmathcal%7BC%7D%7D%20%5Cto%20T) (unit) and ![\mu: T^2 \to T](https://latex.codecogs.com/png.latex?%5Cmu%3A%20T%5E2%20%5Cto%20T) (multiplication) satisfying certain coherence conditions.

10. **Initial and Terminal Objects**
    * **Initial Object**: An object ![I](https://latex.codecogs.com/png.latex?I) in a category ![\mathcal{C}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BC%7D) such that for every object ![A \in \mathcal{C}](https://latex.codecogs.com/png.latex?A%20%5Cin%20%5Cmathcal%7BC%7D), there exists a unique morphism from ![I](https://latex.codecogs.com/png.latex?I) to ![A](https://latex.codecogs.com/png.latex?A).
    * **Terminal Object**: An object ![T](https://latex.codecogs.com/png.latex?T) in a category ![\mathcal{C}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BC%7D) such that for every object ![A \in \mathcal{C}](https://latex.codecogs.com/png.latex?A%20%5Cin%20%5Cmathcal%7BC%7D), there exists a unique morphism from ![A](https://latex.codecogs.com/png.latex?A) to ![T](https://latex.codecogs.com/png.latex?T).

11. **Equivalence of Categories**
    * Two categories ![\mathcal{C}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BC%7D) and ![\mathcal{D}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BD%7D) are said to be **equivalent** if there exist functors ![F: \mathcal{C} \to \mathcal{D}](https://latex.codecogs.com/png.latex?F%3A%20%5Cmathcal%7BC%7D%20%5Cto%20%5Cmathcal%7BD%7D) and ![G: \mathcal{D} \to \mathcal{C}](https://latex.codecogs.com/png.latex?G%3A%20%5Cmathcal%7BD%7D%20%5Cto%20%5Cmathcal%7BC%7D) such that ![G \circ F](https://latex.codecogs.com/png.latex?G%20%5Ccirc%20F) is naturally isomorphic to the identity functor on ![\mathcal{C}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BC%7D), and ![F \circ G](https://latex.codecogs.com/png.latex?F%20%5Ccirc%20G) is naturally isomorphic to the identity functor on ![\mathcal{D}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BD%7D).

12. **Yoneda Lemma**
    * The Yoneda Lemma states that for any category ![\mathcal{C}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BC%7D), object ![A \in \mathcal{C}](https://latex.codecogs.com/png.latex?A%20%5Cin%20%5Cmathcal%7BC%7D), and functor ![F: \mathcal{C} \to \textbf{Set}](https://latex.codecogs.com/png.latex?F%3A%20%5Cmathcal%7BC%7D%20%5Cto%20%5Ctextbf%7BSet%7D), there is a natural isomorphism:
      ![ \text{Nat}(\text{Hom}_{\mathcal{C}}(-, A), F) \cong F(A) ](https://latex.codecogs.com/png.latex?%5Ctext%7BNat%7D%28%5Ctext%7BHom%7D_%7B%5Cmathcal%7BC%7D%7D%28-%2C%20A%29%2C%20F%29%20%5Ccong%20F%28A%29)
      where:
      * ![\text{Hom}_{\mathcal{C}}(-, A)](https://latex.codecogs.com/png.latex?%5Ctext%7BHom%7D_%7B%5Cmathcal%7BC%7D%7D%28-%2C%20A%29) is the functor that maps an object ![X](https://latex.codecogs.com/png.latex?X) to the set of morphisms ![\text{Hom}_{\mathcal{C}}(X, A)](https://latex.codecogs.com/png.latex?%5Ctext%7BHom%7D_%7B%5Cmathcal%7BC%7D%7D%28X%2C%20A%29).
      * ![\text{Nat}(\text{Hom}_{\mathcal{C}}(-, A), F)](https://latex.codecogs.com/png.latex?%5Ctext%7BNat%7D%28%5Ctext%7BHom%7D_%7B%5Cmathcal%7BC%7D%7D%28-%2C%20A%29%2C%20F%29) denotes the set of natural transformations from ![\text{Hom}_{\mathcal{C}}(-, A)](https://latex.codecogs.com/png.latex?%5Ctext%7BHom%7D_%7B%5Cmathcal%7BC%7D%7D%28-%2C%20A%29) to ![F](https://latex.codecogs.com/png.latex?F).

13. **Representable Functors**
    * A functor ![F: \mathcal{C} \to \textbf{Set}](https://latex.codecogs.com/png.latex?F%3A%20%5Cmathcal%7BC%7D%20%5Cto%20%5Ctextbf%7BSet%7D) is called **representable** if it is naturally isomorphic to the hom-functor ![\text{Hom}_{\mathcal{C}}(C, -)](https://latex.codecogs.com/png.latex?%5Ctext%7BHom%7D_%7B%5Cmathcal%7BC%7D%7D%28C%2C%20-%29) for some object ![C \in \mathcal{C}](https://latex.codecogs.com/png.latex?C%20%5Cin%20%5Cmathcal%7BC%7D).

## More on

* [Category theory for Programmers](https://unglueit-files.s3.amazonaws.com/ebf/e90890f0a6ea420c9825657d6f3a851d.pdf) by Bartosz Milewski
* [Abstract Algebra in 3 Hours](https://www.slideshare.net/slideshow/abstract-algebra-in-3-hours/70763563) by Ashwin Rao
* [Category theory made easy with (ugly) pictures](https://www.slideshare.net/slideshow/category-theory-made-easy-with-ugly-pictures-73745930/73745930) by Ashwin Rao
* [A Gentle Introduction to Category theory (the calculational approach)](https://maartenfokkinga.github.io/utwente/mmf92b.pdf) by Maarten M Fokkinga
* [What is Category theory Anyway?](https://www.math3ma.com/blog/what-is-category-theory-anyway) by math3m
* [Basic Category theory](https://arxiv.org/pdf/1612.09375) by Tom Leinster
* [Category theory for Dummies](https://homepages.inf.ed.ac.uk/jcheney/presentations/ct4d1.pdf) by James Cheney
* [Good Introduction to Category theory?](https://www.reddit.com/r/math/comments/r4yoc0/good_introduction_to_category_theory/) by r/math
* [An Introduction to the Language of Category theory](http://www.ms.lt/derlius/StevenRoman-AnIntroductionToTheLanguageOfCategoryTheory.pdf) by Steven Roman
* [Introduction to Category theory and Categorical logic](https://www2.mathematik.tu-darmstadt.de/~streicher/CTCL.pdf) by Thomas Streicher
