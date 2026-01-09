# `Essence of Linear Algebra`

Notes on the 3Blue1Brown series, focusing on the geometric intuition behind linear algebra.

## 1. Vectors

Vectors can be understood from three perspectives:
*   **Physics:** Arrows with a certain length and direction.
*   **Computer Science:** Ordered lists of numbers.
*   **Mathematics:** Objects that can be added together and scaled.

The core idea is that these are all different ways of looking at the same thing. A vector in 2D space can be represented by a pair of numbers, which are its coordinates.

*   **Vector Addition:** Geometrically, adding two vectors $\vec{v}$ and $\vec{w}$ is like placing the tail of $\vec{w}$ at the head of $\vec{v}$. The sum is the vector from the origin to the new position of $\vec{w}$'s head. Numerically, this is adding the corresponding components:
    $$
    \begin{bmatrix} v_1 \\ v_2 \\ \end{bmatrix} + \begin{bmatrix} w_1 \\ w_2 \\ \end{bmatrix} = \begin{bmatrix} v_1 + w_1 \\ v_2 + w_2 \\ \end{bmatrix}
    $$
*   **Scalar Multiplication:** Multiplying a vector by a scalar (a number) stretches or squishes the vector. A negative scalar flips its direction.
    $$
    c \begin{bmatrix} v_1 \\ v_2 \\ \end{bmatrix} = \begin{bmatrix} c v_1 \\ c v_2 \\ \end{bmatrix}
    $$

## 2. Linear Combinations, Span, and Basis Vectors

*   **Linear Combination:** A linear combination of two vectors $\vec{v}$ and $\vec{w}$ is any vector of the form $a\vec{v} + b\vec{w}$, where $a$ and $b$ are scalars.
*   **Span:** The span of a set of vectors is the set of all possible linear combinations of those vectors.
    *   The span of a single vector is a line.
    *   The span of two non-collinear vectors is a plane.
*   **Basis Vectors:** A set of vectors that can be used to create any other vector in a given space through linear combinations. In 2D, the standard basis vectors are $\hat{i}$ (the unit vector in the x-direction) and $\hat{j}$ (the unit vector in the y-direction).
    $$
    \hat{i} = \begin{bmatrix} 1 \\ 0 \\ \end{bmatrix}, \quad \hat{j} = \begin{bmatrix} 0 \\ 1 \\ \end{bmatrix}
    $$
    A vector $\begin{bmatrix} x \\ y \\ \end{bmatrix}$ is just a linear combination of these basis vectors: $x\hat{i} + y\hat{j}$.

## 3. Linear Transformations and Matrices

A **linear transformation** is a function that takes a vector as input and outputs another vector, satisfying two properties:
1.  Lines remain lines (no curving).
2.  The origin remains fixed.

A linear transformation is completely determined by where it sends the basis vectors. A 2x2 matrix is a way to describe a 2D linear transformation. The columns of the matrix are the coordinates of where the basis vectors $\hat{i}$ and $\hat{j}$ land after the transformation.

If a transformation sends $\hat{i}$ to $\begin{bmatrix} a \\ c \\ \end{bmatrix}$ and $\hat{j}$ to $\begin{bmatrix} b \\ d \\ \end{bmatrix}$, the matrix of the transformation is:
$$
\begin{bmatrix} a & b \\ c & d \\ \end{bmatrix}
$$

To find where any vector $\vec{v} = \begin{bmatrix} x \\ y \\ \end{bmatrix}$ lands, we can use a matrix-vector product:
$$
\begin{bmatrix} a & b \\ c & d \\ \end{bmatrix} \begin{bmatrix} x \\ y \\ \end{bmatrix} = x \begin{bmatrix} a \\ c \\ \end{bmatrix} + y \begin{bmatrix} b \\ d \\ \end{bmatrix} = \begin{bmatrix} ax + by \\ cx + dy \\ \end{bmatrix}
$$

## 4. Matrix Multiplication as Composition

If you apply one linear transformation and then another, the result is a single, composite transformation. The matrix of this composite transformation is the product of the individual transformation matrices.

If $T_1$ is represented by matrix $M_1$ and $T_2$ by $M_2$, the composite transformation $T_2 \circ T_1$ is represented by the matrix product $M_2 M_1$. Note the order: transformations are applied from right to left.

## 5. The Determinant

The determinant of a matrix represents the factor by which the area of a region changes after the transformation.
*   If the determinant is positive, the orientation of space is preserved.
*   If the determinant is negative, the orientation is flipped.
*   If the determinant is 0, the transformation squishes all of space onto a line or a point.

For a 2x2 matrix $\begin{bmatrix} a & b \\ c & d \\ \end{bmatrix}$, the determinant is $ad - bc$.

## 6. Inverse Matrices, Column Space, and Null Space

*   **Inverse Matrix:** If a transformation has a non-zero determinant, it can be undone with an inverse transformation, represented by the inverse matrix, $M^{-1}$. Applying a transformation and then its inverse leaves you back where you started: $M^{-1}M = I$, where $I$ is the identity matrix (a transformation that does nothing).
*   **Column Space:** The span of the columns of a matrix. This is the set of all possible outputs of the transformation. If the determinant is 0, the column space is a line or a point.
*   **Null Space (or Kernel):** The set of all vectors that land on the origin after the transformation. For a non-zero determinant transformation, only the zero vector is in the null space.

## 7. Dot Product

The dot product of two vectors $\vec{v}$ and $\vec{w}$ has two equivalent definitions:
1.  **Geometric:** $\vec{v} \cdot \vec{w} = \|\vec{v}\| \|\vec{w}\| \cos(\theta)$, where $\theta$ is the angle between them. This tells you about the alignment of the two vectors.
2.  **Numeric:** $\vec{v} \cdot \vec{w} = v_1 w_1 + v_2 w_2 + \dots + v_n w_n$.

The dot product connects to linear transformations through the concept of **duality**. The dot product with a vector can be interpreted as a linear transformation from a space to the number line.

## 8. Cross Product

The cross product of two vectors $\vec{v}$ and $\vec{w}$ in 3D space, $\vec{v} \times \vec{w}$, results in a new vector that is:
*   **Perpendicular** to both $\vec{v}$ and $\vec{w}$.
*   Its **magnitude** is the area of the parallelogram formed by $\vec{v}$ and $\vec{w}$.
*   Its **direction** is given by the right-hand rule.

The cross product can be calculated as the determinant of a special matrix:
$$
\vec{v} \times \vec{w} = \det \begin{pmatrix} \hat{i} & v_1 & w_1 \\ \hat{j} & v_2 & w_2 \\ \hat{k} & v_3 & w_3 \\ \end{pmatrix}
$$

## 9. Change of Basis

We can describe vectors using different basis vectors. To translate a vector's coordinates from our standard basis to a new basis, we can use a change of basis matrix. This matrix is constructed by taking the coordinates of the new basis vectors in our standard system.

To translate a transformation matrix to a new basis, you use the formula $A^{-1}MA$, where $M$ is the original transformation matrix and $A$ is the change of basis matrix.

## 10. Eigenvectors and Eigenvalues

An **eigenvector** of a linear transformation is a vector that does not change direction during the transformation; it only gets scaled. The factor by which it is scaled is the **eigenvalue**.

For a transformation with matrix $M$, an eigenvector $\vec{v}$ and its corresponding eigenvalue $\lambda$ satisfy the equation:
$$
M\vec{v} = \lambda\vec{v}
$$

Eigenvectors and eigenvalues are extremely useful for understanding the geometry of a transformation and for simplifying calculations, especially when working in a basis of eigenvectors.

## More on

* [Essence of linear algebra: 3Blue1Brown](https://www.youtube.com/playlist?list=PLZHQObOWTQDPD3MizzM2xVFitgF8hE_ab)
* [Linear Algebra course on Khan Academy](https://www.khanacademy.org/math/linear-algebra)
* [MIT OpenCourseWare: Linear Algebra](https://ocw.mit.edu/courses/18-06-linear-algebra-spring-2010/)
