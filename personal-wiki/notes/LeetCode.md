# `LeetCode`

Ace the coding interview.

## Common patterns 

1. Prefix Sum

* Query sum of elements in a subarray

```py
def create_prefix_sum(inp_array):
    for i in range(1, len(inp_array)):
        inp_array[i] += inp_array[i-1]
    return inp_array
```

* Questions that use it 
    * [303. Range Sum Query - Immutable](https://leetcode.com/problems/range-sum-query-immutable/)
    * [525. Contiguous Array](https://leetcode.com/problems/contiguous-array/)
    * [560. Subarray Sum Equals K](https://leetcode.com/problems/subarray-sum-equals-k/)

2. Two pointers

* Initialise 2 variables
* Move them towards or away from each other when a certain predicate is fulfilled

```py
def is_palindrome(inp_string):
    start = 0 # pointer 1
    end = len(inp_string) - 1 # pointer 2
    while start < end:
        if inp_string[start] != inp_string[end]:
            return False
        else:
            start += 1
            end -= 1
    return True
```

* Questions that use it 
    * [167. Two Sum II - Input Array Is Sorted](https://leetcode.com/problems/two-sum-ii-input-array-is-sorted/)
    * [15. 3Sum](https://leetcode.com/problems/3sum/)
    * [11. Container With Most Water](https://leetcode.com/problems/container-with-most-water/)

3. Sliding window

* Find subarrays and substrings of a specified size

```py
def max_subarray_sum_sliding_window(inp_array, k):
    n = len(inp_array)
    window_sum = sum(inp_array[:k])
    max_sum = window_sum
    max_start_index = 0
    for i in range(n-k):
        window_sum = window_sum - inp_array[i] + inp_array[i+k]
        if window_sum > max_sum:
            max_sum = window_sum
            max_start_index = i + 1
    return inp_array[max_start_index:max_start_index+k], max_sum
```

* Questions that use it
    * [643. Maximum Average Subarray I](https://leetcode.com/problems/maximum-average-subarray-i/)
    * [3. Longest Substring Without Repeating Characters](https://leetcode.com/problems/longest-substring-without-repeating-characters/)
    * [76. Minimum Window Substring](https://leetcode.com/problems/minimum-window-substring/)

4. Fast and slow pointers

* For problems involving linked lists and arrays
* Find a looping *(cycling)* node 
* Move two pointers at different speeds

```py
def find_cycling_node(head_node):
    slow = head_node # slow pointer
    fast = head_node # fast pointer
    while fast and fast.next:
        slow = slow.next
        fast = fast.next.next
        if slow == fast:
            return True
    return False
```

* Questions that use it
    * [141. Linked List Cycle](https://leetcode.com/problems/linked-list-cycle/)
    * [202. Happy Number](https://leetcode.com/problems/happy-number/)
    * [287. Find the Duplicate Number](https://leetcode.com/problems/find-the-duplicate-number/)


5. Linked list in-place reversal

* Reverses a linked list in place
* Space complexity of $O(1)$

```py
def reverse_linked_list(head_node):
    prev = None
    current = head_node
    while current is not None:
        next = current.next
        current.next = prev
        prev = current
        current = next
    return prev
```

* Questions that use it
    * [206. Reverse Linked List](https://leetcode.com/problems/reverse-linked-list/)
    * [92. Reverse Linked List II](https://leetcode.com/problems/reverse-linked-list-ii/)
    * [24. Swap Nodes in Pairs](https://leetcode.com/problems/swap-nodes-in-pairs/)

6. Monotonic stack

* Find next greater or next smaller element in an array
* Uses a stack 
* Execute stack operations when a specified predicate is fulfilled

```py
def next_greater_element(inp_array):
    n = len(inp_array)
    stack = []
    result = [-1] * n
    for i in range(n):
        while stack and inp_array[i] > inp_array[stack[-1]]:
            result[stack.pop()] = inp_array[i]
        stack.append(i)
    return result
```

* Questions that use it
    * [496. Next Greater Element I](https://leetcode.com/problems/next-greater-element-i/)
    * [739. Daily Temperatures](https://leetcode.com/problems/daily-temperatures/)
    * [84. Largest Rectangle in Histogram](https://leetcode.com/problems/largest-rectangle-in-histogram/)

7. Top 'K' elements

* Find 'K' largest / smallest / most frequent elements in a dataset
* Uses a heap *(min-heap or max-heap)*

```py
def build_min_heap(arr, k):
    for i in range(k//2 - 1, -1, -1):
        heapify(arr, k, i)

def heapify(arr, n, i):
    smallest = i
    left = 2 * i + 1
    right = 2 * i + 2
    if left < n and arr[left] < arr[smallest]:
        smallest = left
    if right < n and arr[right] < arr[smallest]:
        smallest = right
    if smallest != i:
        arr[i], arr[smallest] = arr[smallest], arr[i]
        heapify(arr, n, smallest)

def find_Kth_Largest(nums, k):
    heap = nums[:k]
    build_min_heap(heap, k)
    for i in range(k, len(nums)):
        if nums[i] > heap[0]:
            heap[0] = nums[i]
            heapify(heap, k, 0)
    return heap[0]
```

* Questions that use it
    * [215. Kth Largest Element in an Array](https://leetcode.com/problems/kth-largest-element-in-an-array/)
    * [347. Top K Frequent Elements](https://leetcode.com/problems/top-k-frequent-elements/)
    * [373. Find K Pairs with Smallest Sums](https://leetcode.com/problems/find-k-pairs-with-smallest-sums/)


8. Overlapping intervals

* Used for overlapping intervals or ranges

```py
def merge(intervals):
    intervals.sort(key=lambda x: x[0])
    merged = []
    for interval in intervals:
        if not merged or merged[-1][1] < interval[0]:
            merged.append(interval)
        else:
            merged[-1][1] = max(merged[-1][1], interval[1])
    return merged
```

* Questions that use it
    * [56. Merge Intervals](https://leetcode.com/problems/merge-intervals/)
    * [57. Insert Interval](https://leetcode.com/problems/insert-interval/)
    * [435. Non-overlapping Intervals](https://leetcode.com/problems/non-overlapping-intervals/)


9. Modified binary search

* Variant of binary search 
* Applied to arrays that are nearly sorted, rotated sorted, of unknown length etc.

```py
def modified_binary_search(nums, target):
    left, right = 0, len(nums) - 1
    while left <= right:
        mid = (left + right) // 2
        if nums[mid] == target:
            return mid
        if nums[left] <= nums[mid]:
            if nums[left] <= target < nums[mid]:
                right = mid - 1
            else:
                left = mid + 1
        else:
            if nums[mid] < target <= nums[right]:
                left = mid + 1
            else:
                right = mid - 1
    return -1
```

* Questions that use it
    * [33. Search in Rotated Sorted Array](https://leetcode.com/problems/search-in-rotated-sorted-array/)
    * [153. Find Minimum in Rotated Sorted Array](https://leetcode.com/problems/find-minimum-in-rotated-sorted-array/)
    * [240. Search a 2D Matrix II](https://leetcode.com/problems/search-a-2d-matrix-ii/)

10. Binary tree traversal

* Always consider which type of traversal is the best when doing a binary tree problem
* Pre-order: recursive, iterative *(stack)*
* In-order: recursive, iterative *(stack)*
* Post-order: recursive, iterative *(stack)*
* Level-order: recursive, iterative *(queue)*

```py
def preorder_traversal(root):
    if root:
        print(root.val)
        preorder_traversal(root.left)
        preorder_traversal(root.right)

def inorder_traversal(root):
    if root:
        inorder_traversal(root.left)
        print(root.val)
        inorder_traversal(root.right)

def postorder_traversal(root):
    if root:
        postorder_traversal(root.left)
        postorder_traversal(root.right)
        print(root.val)

def level_order_traversal(root): 
    """
    note the iterative approach is more 
    common for level order traversal of 
    a binary tree

    this recursive approach is less 
    commonly seen
    """
    def traverse_level(node, level):
        if node:
            if level == 1:
                print(node.val)
            elif level > 1:
                traverse_level(node.left, level - 1)
                traverse_level(node.right, level - 1)
    def height(node):
        if not node:
            return 0
        return 1 + max(height(node.left), height(node.right))
    h = height(root)
    for i in range(1, h + 1):
        traverse_level(root, i)
```

* Questions that use it
    * [257. Binary Tree Paths](https://leetcode.com/problems/binary-tree-paths/)
    * [230. Kth Smallest Element in a BST](https://leetcode.com/problems/kth-smallest-element-in-a-bst/)
    * [124. Binary Tree Maximum Path Sum](https://leetcode.com/problems/binary-tree-maximum-path-sum/)
    * [107. Binary Tree Level Order Traversal II](https://leetcode.com/problems/binary-tree-level-order-traversal-ii/)

11. Depth-first search (DFS)

* Explores all paths and branches in graphs and trees
* Recursive, iterative *(stack)*

```py
def dfs(graph, node, visited=None):
    if visited is None:
        visited = set()
    visited.add(node)
    print(node)
    for neighbor in graph[node]:
        if neighbor not in visited:
            dfs(graph, neighbor, visited)
```

* Questions that use it
    * [133. Clone Graph](https://leetcode.com/problems/clone-graph/)
    * [113. Path Sum II](https://leetcode.com/problems/path-sum-ii/)
    * [210. Course Schedule II](https://leetcode.com/problems/course-schedule-ii/)


12. Breadth-first search (BFS)

* Explores nodes level-by-level in graphs and trees
* Recursive, iterative *(queue)*

```py
def bfs_recursive(graph, queue, visited):
    """
    note the iterative approach is more 
    common for bfs of a binary tree using
    a queue

    this recursive approach is less 
    commonly seen
    """
    if not queue:
        return
    node = queue.pop(0)
    print(node)
    for neighbor in graph[node]:
        if neighbor not in visited:
            visited.add(neighbor)
            queue.append(neighbor)
    bfs_recursive(graph, queue, visited)

def start_bfs(graph, start_node):
    visited = set([start_node])
    queue = [start_node]
    bfs_recursive(graph, queue, visited)
```

* Questions that use it
    * [102. Binary Tree Level Order Traversal](https://leetcode.com/problems/binary-tree-level-order-traversal/)
    * [994. Rotting Oranges](https://leetcode.com/problems/rotting-oranges/)
    * [127. Word Ladder](https://leetcode.com/problems/word-ladder/)

13. Matrix traversal

* Most matrix traversal problems can be viewed as graph problems
* Allows us to apply graph algorithms *(DFS, BFS)* as we please

```py
def floodFill(image, sr, sc, newColor):
    originalColor = image[sr][sc]
    if originalColor == newColor:
        return image
    def fill(r, c):
        if image[r][c] == originalColor:
            image[r][c] = newColor
            if r > 0:
                fill(r - 1, c)
            if r < len(image) - 1:
                fill(r + 1, c)
            if c > 0:
                fill(r, c - 1)
            if c < len(image[0]) - 1:
                fill(r, c + 1)
    fill(sr, sc)
    return image
```

* Questions that use it
    * [733. Flood Fill](https://leetcode.com/problems/flood-fill/)
    * [200. Number of Islands](https://leetcode.com/problems/number-of-islands/)
    * [130. Surrounded Regions](https://leetcode.com/problems/surrounded-regions/)

14. Backtracking

* Used for exploring all potential solution paths and backtracking paths that don't lead to a valid solution

```py
def permute(nums):
    result = []
    def backtrack(path):
        if len(path) == len(nums):
            result.append(path)
            return
        for num in nums:
            if num not in path:
                backtrack(path + [num])
    backtrack([])
    return result
```

* Questions that use it
    * [46. Permutations](https://leetcode.com/problems/permutations/)
    * [78. Subsets](https://leetcode.com/problems/subsets/)
    * [51. N-Queens](https://leetcode.com/problems/n-queens/)

15. Dynamic programming (DP)

* Used for solving optimization problems 
    * Break down large problems into smaller sub-problems
    * Store the solutions to sub-problems to avoid repetitive work
* Two approaches
    * Memoization: **top-down appraoch** that uses *recursion* to break down the problem into smaller subproblems, storing the results for reuse
    * Tabulation: **bottom-up approach** that uses *iteration* to solve subproblems and build the solution incrementally
* Common DP patterns
    * Fibonacci 
    * 0/1 Knapsack
    * Longest common subsequence (LCS)
    * Longest increasing subsequence (LIS)
    * Subset sum
    * Matrix chain multiplication
* Questions that use it
    * [70. Climbing Stairs](https://leetcode.com/problems/climbing-stairs/)
    * [322. Coin Change](https://leetcode.com/problems/coin-change/)
    * [1143. Longest Common Subsequence](https://leetcode.com/problems/longest-common-subsequence/)
    * [300. Longest Increasing Subsequence](https://leetcode.com/problems/longest-increasing-subsequence/)
    * [416. Partition Equal Subset Sum](https://leetcode.com/problems/partition-equal-subset-sum/)
    * [312. Burst Balloons](https://leetcode.com/problems/burst-balloons/)

## More on

* [LeetCode Problems pattern frequency](https://seanprashad.com/leetcode-patterns/) by Sean Prashad
* [leetcode-patterns](https://github.com/seanprashad/leetcode-patterns) Github Repository
* [14 Patterns to Ace Any Coding Interview Question](https://leetcode.com/discuss/study-guide/4039411/14-Patterns-to-Ace-Any-Coding-Interview-Question) by unknow_user
* [Leetcode Patterns/Techniques Cheat Sheet](https://www.reddit.com/r/leetcode/comments/1d31ksp/leetcode_patternstechniques_cheat_sheet/) by r/leetcode
* [List of questions sorted by common patterns](https://leetcode.com/discuss/career/448285/List-of-questions-sorted-by-common-patterns) by Maverick2594
* [neetcode.io](https://neetcode.io/)
* [LeetCode was HARD until I Learned these 15 Patterns](https://youtu.be/DjYZk8nrXVY?si=FamjLQ5cmnaZ3IBk) by Ashish Pratap Singh
* [20 Patterns to Master Dynamic Programming](https://blog.algomaster.io/p/20-patterns-to-master-dynamic-programming) by Ashish Pratap Singh