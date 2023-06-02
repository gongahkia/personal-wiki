> Carry on from Video 3, Arrays Data Structure [here](https://frontendmasters.com/courses/algorithms/).

# Algorithms

This is based off [Prime's Algo course on Frontend Masters](https://frontendmasters.com/courses/algorithms/), which is conducted in Typescript.

## Resources to read

Go read this in your own time. Else you gonna suck!

* [Grokking algorithms](resources/Grokking-Algorithms.pdf)
* [The Introduction to Algorithms](resources/Introduction-to-Algorithms.pdf)
* [For programmers who wanna be the very best](resources/Common-Sense-Guide-to-Data-Structures.pdf)

## Time and Space Complexity

### Important concepts
1. Growth is with respect to the input.
2. Constants are dropped.
3. Worst case is usually the way we measure.

### Big O notation

* Big O categorizes an algorithm's *time* or *memory requirements* based on the size of input.
* Big O helps us make decisions about what data structures and algorithms we should use.

> TLDR: As your input grows, how fast does computation or memory grow?

Eg 1.

```ts
function sum_char_codes(n:string): number {
    let sum:number = 0;
    for (let i = 0; i < n.length, ++i) {
        sum += n.charCodeAt(i);
    }
    return sum;
}
```

* **Look for loops!**
* Time complexity is `0(N)`.

Eg 2.

```ts
function sum_char_codes(n:string): number {
    let sum:number = 0;
    for (let i = 0; i < n.length, ++i) {
        sum += n.charCodeAt(i);
    }
    for (let i = 0; i < n.length, ++i) {
        sum += n.charCodeAt(i);
    }
    return sum;
}
```

* Time complexity is still `0(N)`
    * This is as `0(2N)` -> `0(N)`.
    * **Constants are dropped**.

Eg 3.

```ts
function sum_char_codes(n:string): number {
    let sum:number = 0;
    for (let i = 0; i < n.length, ++i) {
        if (charCode === 69) {
            return sum;
        }
        sum += n.charCodeAt(i);
    }
    return sum;
}
```

* Time complexity is still `0(N)`.
    * This is as we usually consider **worse case**, which is `0(N)`.

### Common complexities we will see

![](https://he-s3.s3.amazonaws.com/media/uploads/ece920b.png)

Eg 4.

```ts
function sum_char_codes(n:string): number {
    let sum:number = 0;
    for (let i = 0; i < n.length, ++i) {
        for (let i = 0; i < n.length, ++i) {
            sum += n.charCodeAt(i);
        }
    }
    return sum;
}
```

* **Count the loops!**
* Time complexity is `0(N^2)`.

Eg 5.

```ts
function sum_char_codes(n:string): number {
    let sum:number = 0;
    for (let i = 0; i < n.length, ++i) {
        for (let i = 0; i < n.length, ++i) {
            for (let i = 0; i < n.length, ++i) {
                sum += n.charCodeAt(i);
            }
        }
    }
    return sum;
}
```

* **Count the loops!**
* Time complexity is `0(N^3)`.

Eg 6.

```md
Quicksort algorithm
```

* Time complexity is `0(n log n)`.

Eg 7. 

```md 
Binary search trees
```

* Time complexity is `0(log n)`.

Eg 8.

```md
Covered later!
```

* Time complexity is `0(sqrt(n))`.

## Data structures

### Array

* Fixed size, continiguous memory chunks.
    * That means you cannot grow an array.
    * There is no `insert_at`, `push` or `pop`. Those can be implemented separately, but under the hood arrays function based on the above characteristic.

* Time complexity of `0(1)`. (*get, insertion, deletion*)
* **Constant time**, as we do a constant amount of things regardless of input size.
* Number of computation steps do not increase with input.

![image_2023-06-02_17-35-17](https://github.com/gongahkia/university-notes/assets/117062305/a4f65168-40f2-45a5-ba6d-03ced7a4f9c0)

