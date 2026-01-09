# `Differential Equations`

Notes on the 3Blue1Brown series, providing an intuition for what differential equations are and how to think about them.

## Introduction: The Language of Change

Differential equations are the language used to describe change in the universe. Anytime a law of nature describes how a quantity changes over time or space, it is likely expressed as a differential equation.

A **differential equation** is an equation that relates a function to its derivatives. For example, an equation might connect the position of an object to its velocity (the first derivative of position) and its acceleration (the second derivative).

## The Essence of a Differential Equation

The core idea of a differential equation is that it provides a rule for how a quantity changes at any given moment. Consider an equation of the form:
$$
\frac{dy}{dt} = f(y, t)
$$
This equation tells you the rate of change of $y$ at any given time $t$ and for any value of $y$. A **solution** to the differential equation is a function $y(t)$ that satisfies this rule for all $t$.

## Geometric View: Slope Fields

One of the most intuitive ways to understand a differential equation is to visualize it as a **slope field**.
*   For an equation $\frac{dy}{dt} = f(y, t)$, you can go to any point $(t, y)$ in the plane and draw a tiny arrow with the slope given by $f(y, t)$.
*   This collection of arrows is the slope field. It gives you a sense of the flow or the dynamics of the system.
*   A solution to the differential equation is a curve that "follows the arrows" – a curve that is tangent to the slope field at every point.

An **initial condition**, like $y(0) = y_0$, specifies a starting point for the curve.

## Numerical Approximation: Euler's Method

While it can be difficult to find an exact formula for the solution, we can always approximate it numerically using the slope field. This is the idea behind **Euler's method**:
1.  Start at an initial point $(t_0, y_0)$.
2.  Choose a small step size, $\Delta t$.
3.  Calculate the slope at the starting point: $m = f(y_0, t_0)$.
4.  Approximate the change in $y$ as $\Delta y \approx m \cdot \Delta t$.
5.  The new point is $(t_0 + \Delta t, y_0 + \Delta y)$.
6.  Repeat this process from the new point.

This is like walking through the slope field, taking small, straight steps at each point, with the direction of the step given by the slope at that point.

## Systems of Equations and Phase Space

Often, we have multiple quantities that change and depend on each other. This leads to a **system of differential equations**. A classic example is a predator-prey model, where the population of rabbits and foxes depend on each other.

To visualize such a system, we use a **phase space**.
*   Each axis in the phase space represents one of the quantities (e.g., one axis for rabbits, one for foxes).
*   A single point in the phase space represents the state of the entire system at a moment in time.
*   The system of differential equations defines a **vector field** in this space. At each point, there is a vector that tells you how the state of the system is changing.
*   A solution is a path through the phase space, following the vectors of the vector field.

## Linear Differential Equations

A special and very important class of differential equations is **linear differential equations**, where the function and its derivatives appear linearly.

For systems of linear differential equations, the tools of linear algebra become essential. The concepts of **eigenvectors** and **eigenvalues** of the matrix representing the system tell you about the long-term behavior of the system (e.g., whether it will go to a stable point, or grow to infinity).

## More on

* [Differential equations: 3Blue1Brown](https://www.youtube.com/playlist?list=PLZHQObOWTQDNPOjrT6KVlfJuKtYTftqH6)
* [Khan Academy on Differential Equations](https://www.khanacademy.org/math/differential-equations)
* [MIT OpenCourseWare: Differential Equations](https://ocw.mit.edu/courses/18-03-differential-equations-spring-2010/)
