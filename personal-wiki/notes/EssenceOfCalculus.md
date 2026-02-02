# `Essence of Calculus`

Notes on the 3Blue1Brown series, providing an intuition for the core ideas of calculus: derivatives, integrals, and the fundamental theorem that connects them.

## Introduction: The Paradox of Motion

Calculus is the study of change. It was developed to answer questions that had puzzled thinkers for centuries, such as how to describe the instantaneous speed of a moving object. At any given instant, an object is at a single point, so how can it be moving? Calculus provides the tools to resolve this paradox. It is built on two major, complementary ideas: **derivatives** and **integrals**.

## Derivatives

The derivative of a function at a point gives the **instantaneous rate of change** at that point.

### Geometric Intuition: The Slope of a Tangent

Geometrically, the derivative of a function $f(x)$ at a point $x=a$ is the slope of the line that is tangent to the graph of the function at that point. It tells you how "steep" the function is at that exact point.

### The "Nudge" Intuition

To find the rate of change at a point, we can "nudge" the input by a tiny amount, which we'll call $dx$. This causes a corresponding tiny change in the output of the function, $df$. The derivative is the ratio of these two tiny changes:
$$
\frac{df}{dx}
$$
This is not a fraction in the traditional sense, but a symbol representing the derivative of $f$ with respect to $x$.

### The Formal Definition

The intuition of "nudging" is formalized using the concept of a limit. The derivative of a function $f(x)$ is:
$$f'(x) = \lim_{h \to 0} \frac{f(x+h) - f(x)}{h}$$
This formula calculates the slope of a line between two points on the curve and then finds the limit as those two points get infinitely close to each other.

### Key Derivative Rules

*   **Power Rule:** $\frac{d}{dx}x^n = nx^{n-1}$
*   **Product Rule:** $\frac{d}{dx}(f(x)g(x)) = f'(x)g(x) + f(x)g'(x)$
*   **Chain Rule:** $\frac{d}{dx}f(g(x)) = f'(g(x))g'(x)$. This is for nested functions and can be thought of as multiplying the rates of change of the outer and inner functions.

## Integrals

The integral is the second major idea of calculus. It is a way of summing up infinitely many, infinitesimally small quantities.

### Geometric Intuition: Area Under a Curve

Geometrically, the definite integral of a function $f(x)$ from a point $a$ to a point $b$ is the **area under the curve** between those two points.

### The "Slicing" Intuition

To calculate this area, we can slice it into a series of infinitely thin rectangles.
*   Each rectangle has a width of $dx$.
*   The height of a rectangle at a point $x$ is given by the value of the function, $f(x)$.
*   The area of each tiny rectangle is $f(x)dx$.
*   The integral is the sum of the areas of all these rectangles. The integral symbol, $\int$, is an elongated "S" to stand for "sum".
    $$ \int_a^b f(x) dx $$

## The Fundamental Theorem of Calculus

This is the central theorem of calculus, as it provides the explicit link between derivatives and integrals. It shows that they are, in a sense, inverse operations.

### Part 1: The Derivative of an Integral

The first part of the theorem states that if you have a function representing the area under a curve, its derivative is the original function. In other words, the rate at which the area under a curve grows is equal to the height of the curve at that point.
$$ \frac{d}{dx} \int_a^x f(t) dt = f(x) $$

### Part 2: The Integral of a Derivative

The second part of the theorem gives us a practical way to calculate definite integrals. It says that to find the area under the curve of a function $f(x)$ from $a$ to $b$, you just need to find its **antiderivative**, $F(x)$ (a function whose derivative is $f(x)$), and then calculate the change in $F(x)$ from $a$ to $b$.
$$ \int_a^b f(x) dx = F(b) - F(a) \quad \text{where } F'(x) = f(x) $$

## Taylor Series

A Taylor series is a way to approximate any smooth function with an infinite polynomial. The idea is to make the polynomial match the function's value, its first derivative, its second derivative, and so on, at a particular point.

The formula for the Taylor series of a function $f(x)$ around a point $a$ is:
$$ f(x) = \sum_{n=0}^{\infty} \frac{f^{(n)}(a)}{n!} (x-a)^n = f(a) + f'(a)(x-a) + \frac{f''(a)}{2!}(x-a)^2 + \dots $$

## More on

* [Essence of calculus: 3Blue1Brown](https://www.youtube.com/playlist?list=PLZHQObOWTQDMsr9K-rj53DwVRMYO3t5Yr)
* [Khan Academy on Calculus](https://www.khanacademy.org/math/calculus-1)
* [MIT OpenCourseWare: Single Variable Calculus](https://ocw.mit.edu/courses/18-01sc-single-variable-calculus-fall-2010/)
