# `Real analysis`

The rigorous study of real numbers, sequences, series, continuity, differentiation, and integration.

## Introduction

* Branch of mathematical analysis dealing with real numbers and real-valued functions
* Provides rigorous foundations for calculus using limits and formal proofs
* Studies properties of real numbers, convergence, continuity, and differentiability
* Essential for understanding advanced mathematics, physics, and engineering

## Quickstart

### Core Concepts

* **Real Numbers (ℝ)**: Complete ordered field containing rationals and irrationals
* **Sequences**: Ordered lists of real numbers {aₙ} indexed by natural numbers
* **Series**: Infinite sums Σaₙ of sequence terms
* **Functions**: Mappings f: ℝ → ℝ between real numbers
* **Limits**: Behavior of sequences/functions approaching specific values
* **Continuity**: Functions with no "jumps" or "breaks"
* **Derivatives**: Instantaneous rates of change
* **Integrals**: Areas under curves and accumulation

### Fundamental Properties

```
# ----- COMPLETENESS AXIOM -----
    # Every non-empty set of real numbers that is bounded above has a least upper bound (supremum)
    # Distinguishes ℝ from ℚ (rationals have "gaps")

# ----- ARCHIMEDEAN PROPERTY -----  
    # For any real numbers a > 0 and b, there exists n ∈ ℕ such that na > b
    # No real number is "infinitely large"

# ----- DENSITY OF RATIONALS -----
    # Between any two real numbers, there exists a rational number
    # ℚ is dense in ℝ
```

## Sequences and Limits

```
# ----- SEQUENCE CONVERGENCE -----
    # A sequence {aₙ} converges to L if:
    # For every ε > 0, there exists N ∈ ℕ such that |aₙ - L|  0, there exists N such that |aₘ - aₙ|  1 ⟹ series diverges
    # L = 1 ⟹ test inconclusive

# ROOT TEST
    # If limₙ→∞ ⁿ√|aₙ| = L, then:
    # L  1 ⟹ series diverges
    # L = 1 ⟹ test inconclusive

# Examples:
    # Geometric series: Σrⁿ converges iff |r|  1
```

## Continuity

```
# ----- FUNCTION CONTINUITY -----
    # f is continuous at c if: limₓ→c f(x) = f(c)
    # Equivalently: for every ε > 0, ∃δ > 0 such that |x-c|  0, ∃δ > 0 such that
    # |x-y|  [ ] . ,
    # Operates on array of memory cells with pointer
    # Turing complete despite extreme simplicity
    # + => increment cell, - => decrement cell
    # > => move pointer right,  move pointer left  
    # [ ] => loop while cell is non-zero
    # . => output cell, , => input to cell

# WHITESPACE
    # Only whitespace characters (space, tab, newline) are meaningful
    # All other characters ignored as comments
    # Stack-based operations using whitespace patterns
    # Space = 0, Tab = 1 in binary representations

# ----- DIMENSIONAL LANGUAGES -----
    # Code execution follows paths through 2D or higher dimensional grids

# BEFUNGE
    # 2D grid where execution pointer moves in cardinal directions
    # Commands change direction: >  terminate program, " => toggle string mode
    # Self-modifying: can write to its own code space

# ><> (FISH)
    # 2D language inspired by Befunge
    # Execution flows like fish swimming through water
    # Wraps around edges of code space
    # Stack-based with reflection and jumping
```