# `Dynamic Programming`

> [!NOTE]
> Code snippets below are written in [Rust](Rust.md).

## Quickstart

*"Those who can't remember the past are condemned to repeat it."*

* dynamic programming: if you already solved a problem with some input, save that result for future reference

## Applications

1. Top-down *(Memoization)*
    1. Break the problem down
    2. If problem has already been solved, return the saved answer
    3. If not already solved, solve the problem and save the answer
2. Bottom-up *(Dynamic programming)*
    1. Determine the order in which sub-problems are solved
    2. Solving the trivial sub-problems
    3. Solve up to the given problem

## Example

Below is an example of dynamic programming within the [longest increasing subsequence problem](https://leetcode.com/problems/longest-increasing-subsequence/).

*eg.* Given a sequence $S= \{a1, a2, a3, a4, \ldots , an-1, an \}$, find the longest subset such that for all $j$ and $i$, $j \lt i$ in the subset $aj \lt ai$. 

### Thought process

1. Find the value of the longest subsequences (LSi) at every index $i$ with last element of sequence being $ai$. 
2. Then largest LSi would be the longest subsequence in the given sequence.

### Writing code

1. Assign LSi the value of $1$ since $ai$ is the last element of the sequence. 
2. For all $j$ such that $j \lt i$ and $aj \lt ai$, find the Largest LSj and add it to LSi. 
3. This algorithm has a time complexity $O(n^2)$.

```rs
fn longest_increasing_subsequence(nums: Vec<i32>) -> usize {

    if nums.is_empty() {
        return 0;
    }

    let n = nums.len();
    let mut lis = vec![1; n];

    for i in 1..n {
        for j in 0..i {
            if nums[j] < nums[i] {
                lis[i] = lis[i].max(lis[j] + 1);
            }
        }
    }

    *lis.iter().max().unwrap()
}
```

## More on

* [learn dynamic programming in y minutes](https://learnxinyminutes.com/docs/dynamic-programming/)
* [ultimate guide to dynamic programming](https://medium.com/@al.eks/the-ultimate-guide-to-dynamic-programming-65865ef7ec5b)
* [dynamic programming from novice to advanced](https://www.topcoder.com/thrive/articles/Dynamic%20Programming:%20From%20Novice%20to%20Advanced)
* [5 simple steps for solving dynamic programming problems](https://youtu.be/aPQY__2H3tE?si=OMmRfrW2Rkhp5iP6)
* [MIT 6.006 lessons 19,20,21,22](https://youtube.com/playlist?list=PLUl4u3cNGP61Oq3tWYp6V_F-5jb5L2iHb&si=2bcOc5PfLWdusSU5)
* [leetcode questions involving dynamic programming](https://leetcode.com/tag/dynamic-programming/)