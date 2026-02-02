# `Bayesian Inference`

Notes on the 3Blue1Brown series "Probabilities of probabilities," which provides an introduction to the core ideas of Bayesian inference.

## Introduction: The Bayesian View

Bayesian inference is a framework for thinking about probability as a measure of belief in a proposition. The core idea is to update our beliefs in the light of new evidence. This is in contrast to the frequentist interpretation of probability, which sees probability as the long-run frequency of an event.

## Bayes' Theorem

The mathematical engine of Bayesian inference is Bayes' Theorem. It tells us how to update our belief in a hypothesis ($H$) after observing some evidence ($E$).

$$ 
P(H|E) = \frac{P(E|H) \cdot P(H)}{P(E)}
$$

Let's break down the terms:
*   $P(H|E)$: The **Posterior**. This is the probability of our hypothesis being true, given the evidence. This is what we want to calculate.
*   $P(E|H)$: The **Likelihood**. This is the probability of observing the evidence, assuming our hypothesis is true.
*   $P(H)$: The **Prior**. This is our initial belief in the hypothesis, before we've seen any evidence.
*   $P(E)$: The **Marginal Likelihood**. This is the total probability of observing the evidence, under all possible hypotheses. It acts as a normalization constant.

In practice, we often write the theorem as:
$$ 
\text{Posterior} \propto \text{Likelihood} \cdot \text{Prior}
$$ 

## From Numbers to Distributions

In many real-world problems, we are not interested in a single probability, but a continuous range of possibilities. For example, instead of asking "is this coin fair?", we might ask "what is the probability of this coin landing heads?". This is a value that could be anywhere between 0 and 1.

In the Bayesian framework, we can represent our belief about this unknown probability with a **probability distribution**.

## The Beta Distribution

The Beta distribution is a family of continuous probability distributions defined on the interval [0, 1]. It is a very natural choice for representing a belief about a probability. It is defined by two positive shape parameters, $\alpha$ and $\beta$.

$$ 
\text{Beta}(\alpha, \beta)
$$ 

*   The mean of the distribution is $\frac{\alpha}{\alpha + \beta}$.
*   The shape of the distribution can be interpreted as representing the knowledge gained from $\alpha - 1$ "successes" and $\beta - 1$ "failures".

For example, a flat prior (representing no knowledge) can be modeled with a Beta(1, 1) distribution, which is a uniform distribution.

## Conjugate Priors

The Beta distribution has a very special relationship with the binomial distribution (which describes the number of successes in a series of independent trials). The Beta distribution is a **conjugate prior** for the binomial likelihood.

This means that if you:
1.  Start with a **prior** belief about a probability, represented by a Beta distribution, $\text{Beta}(\alpha, \beta)$.
2.  Observe new evidence in the form of $k$ successes and $n-k$ failures.
3.  Your **posterior** belief will also be a Beta distribution, with updated parameters:
    $$ 
    \text{Beta}(\alpha + k, \beta + n - k)
    $$ 

This makes calculations much easier and provides a very intuitive way to think about updating beliefs. You just add the number of successes to $\alpha$ and the number of failures to $\beta$.

## Example: Coin Flipping

Suppose you want to determine the fairness of a coin.
*   **Prior:** You have no idea if it's fair, so you start with a uniform prior, $\text{Beta}(1, 1)$.
*   **Evidence:** You flip the coin 10 times and get 7 heads and 3 tails.
*   **Posterior:** Your new belief about the coin's probability of landing heads is represented by the distribution $\text{Beta}(1+7, 1+3) = \text{Beta}(8, 4)$.

The peak of this new distribution is at $7/10 = 0.7$, which is our most likely estimate for the probability of heads. But the distribution also tells us about our uncertainty; there's still a chance the coin is fair, or even biased towards tails.

## Bayesian A/B Testing

This framework can be applied to A/B testing. Instead of p-values, we can directly calculate the probability that variant B is better than variant A. We can model our belief about the conversion rate of each variant with a Beta distribution, update it with new data, and then compare the posterior distributions.

## More on

* [Probabilities of probabilities: 3Blue1Brown](https://www.youtube.com/playlist?list=PLZHQObOWTQDOjmo3Y6ADm0ScWAlEXf-fp)
* [An interactive introduction to Bayesian reasoning](https://seeing-theory.brown.edu/bayesian-inference/index.html)
* [Think Bayes by Allen B. Downey](https://greenteapress.com/wp/think-bayes/)
