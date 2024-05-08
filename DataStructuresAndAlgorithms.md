# `Data Structures and Algorithms`

Code snippets below are written in [Go](https://go.dev/).

## Definitions

* data structure: ordered collection of data provided by the language or defined and enforced by the programmer
* algorithm: sequence of steps to solve a problem
* big o notation: describes performance of an algorithm as size of the dataset increases
    * `n`: variable representing size of dataset
* time complexity: time taken for an algorithm to complete execution
* space complexity: memory taken for an algorithm to complete execution

## Big O Notation

1. CONSTANT time
    * `O(1)`
    * number of steps for algorithm to complete execution is CONSTANT regardless of size of dataset

```go
func addUp(int n) int{ // computation here has a time complexity of O(1)
    var sum int
    sum = n * (n + 1)/2
    return sum
}
```

2. LOGARITHMIC time
    * `O(log n)`
    * number of steps for algorithm to complete execution is MARGINALLY LOWER as size of dataset increases

```go
func binarySearch(arr []int, target int) int { // computation here has a time complexity of O(log n)
    left, right := 0, len(arr)-1
    for left <= right {
        mid := left + (right-left)/2
        if arr[mid] == target {
            return mid
        }
        if arr[mid] < target {
            left = mid + 1
        } else {
            right = mid - 1
        }
    }
    return -1 // target not found
}
```

3. LINEAR time
    * `O(n)`
    * number of steps for algorithm to complete execution INCREASES PROPORTIONALLY to size of dataset

```go
func addUp(int n) int{ // computation here has a time complexity of O(n)
    var sum int
    sum = 0
    for i := 0; i <= n; i++ {
        sum += i;
    }
    return sum
}
```

4. QUASILINEAR time
    * `O(n log n)`
    * similar to LINEAR time, but slows down further when working with larger datasets

```go
func mergeSort(arr []int) []int { // computation here has a time complexity of O(n log n)
    if len(arr) <= 1 {
        return arr
    }
    mid := len(arr) / 2
    left := mergeSort(arr[:mid])
    right := mergeSort(arr[mid:])
    return merge(left, right)
}

func merge(left, right []int) []int {
    result := make([]int, 0)
    for len(left) > 0 || len(right) > 0 {
        if len(left) == 0 {
            return append(result, right...)
        }
        if len(right) == 0 {
            return append(result, left...)
        }
        if left[0] <= right[0] {
            result = append(result, left[0])
            left = left[1:]
        } else {
            result = append(result, right[0])
            right = right[1:]
        }
    }
    return result
}
```

5. QUADRATIC time
    * `O(n^2)`
    * QUADRATIC increase in number of steps for algorithm to complete execution for a given increase in size of dataset

```go
func bubbleSort(arr []int) { // computation here has a time complexity of O(n^2)
    n := len(arr)
    for i := 0; i < n-1; i++ {
        for j := 0; j < n-i-1; j++ {
            if arr[j] > arr[j+1] {
                arr[j], arr[j+1] = arr[j+1], arr[j]
            }
        }
    }
}
```

6. FACTORIAL time
    * `O(n!)`
    * extremely slow and rarely used

```go
func factorial(n int) int { // computation here has a time complexity of O(n!)
    if n == 0 {
        return 1
    }
    return n * factorial(n-1)
}
```

### Summary

![](https://paper-attachments.dropbox.com/s_2D428973624E7FC84C7D69D11421DE762BEA6B6F3361231FCDCAE0425D14526F_1664885448372_Untitled.drawio+17.png)

## Data Structures

### Stack

```go
// --- STACK ---
    // last-in first-out (LIFO) data structure
    // push() => appends elements to the TOP of the stack
    // pop() => removes elements from the TOP of the stack

type Stack struct {
    items []interface{} // interface is used here to allow the slice to hold elements of any datatype
}

func (s *Stack) Push(item interface{}) {
    s.items = append(s.items, item)
}

func (s *Stack) Pop() interface{} {
    if len(s.items) == 0 { // empty stack
        return nil 
    }
    index := len(s.items) - 1
    item := s.items[index]
    s.items = s.items[:index] 
    return item
}

// --- USES ---
    // undo/redo features in text editors
    // moving backward/forward in browser history
    // backtracking algorithms (mazes, file directories)
    // function call stacks
```

### Queue

```go
// --- QUEUE ---
    // first-in first-out (FIFO) data structure
    // add() => enqueues an element to the END of the queue
    // remove() => dequeues an element from the FRONT of the queue

type Queue struct {
    items []interface{} // interface is used here to allow the slice to hold elements of any datatype
}

func (q *Queue) Add(item interface{}) {
    q.items = append(q.items, item)
}

func (q *Queue) Remove() interface{} {
    if len(q.items) == 0 { // empty queue
        return nil
    }
    item := q.items[0]
    q.items = q.items[1:] 
    return item
}

// --- USES ---
    // keyboard buffers
    // printer queues
    // priority queues
    // linked lists
    // breadth-first search algorithm
```

### Priority Queue

```go
// --- PRIORITY QUEUE ---
    // first-in first-out (FIFO) data structure
    // SORTS elements by PRIORITY, then dequeues elements of HIGHEST PRIORITY before elements of LOWER PRIORITY
    // add() => enqueues an element to the END of the queue with a specified value and priority
    // remove() => dequeues element of HIGHEST priority 

type Item struct { // item being EACH ELEMENT of the priority queue
    value interface{} // interface is used here to allow the value to hold elements of any datatype
    priority int
}

type PriorityQueue []*Item // priority queue is implemented via a sorted slice of Item elements

func (pq *PriorityQueue) Add(value interface{}, priority int) {
    item := &Item{
        value: value,
        priority: priority,
    }
    *pq = append(*pq, item)
    sort.Slice(*pq, func(i int, j int) bool { // sort the priority queue based on priority
        return (*pq)[i].priority < (*pq)[j].priority
    })
}

func (pq *PriorityQueue) Remove() interface{} {
    if len(*pq) == 0 { // empty priority queue
        return nil
    }
    item := (*pq)[0]
    *pq = (*pq)[1:] 
    return item.value
}

// --- USES ---
    // sorting algorithms (heap sort)
    // graph algorithms (dijkstra's algorithm, prim's algorithm)
    // system-related functions (load balancing and interrupt handling)
```

### Linked List

```go
// --- LINKED LIST --- 
    // collection of nodes (each node comprising a VALUE + pointer(s) to OTHER NODE'S memory address(es)) stored in non-consecutive memory locations
    // SINGLY LINKED LIST
        // nodes comprise...
            // 1. VALUE
            // 2. NEXT node's memory address
    // DOUBLY LINKED LIST
        // nodes comprise...
            // 1. VALUE
            // 2. PREVIOUS node's memory address
            // 3. NEXT node's memory address

type Node struct { // type definition for an element in a singly linked list
    value interface{} // interface is used here to allow the value to hold elements of any datatype
    next *Node // pointer to the next node
}

type LinkedList struct { // type definition for a singly linked list
    head *Node // define the head node
}

func (list *LinkedList) append(value interface{}) { // appends a new node to the END of the linked list

    newNode := &Node{ // creation of current node
        value: value, 
        next: nil,
    }

    if list.head == nil { // if linked list empty, make current node the new head node
        list.head = newNode
        return
    }

    lastNode := list.head // traverse through the entire linked list until we reach the actual last node in the linkedlist where the next node is nil
    for lastNode.next != nil {
        lastNode = lastNode.next 
    }
    lastNode.next = newNode // make the current node the new last node

}

func (list *LinkedList) add(value interface{}, position int) error { // adds a new node to a specified position in the linked list

    if position < 0 { // if negative index, invalid index
        return fmt.Errorf("invalid position") // error
    }

    newNode := &Node{ // creation of current node
        value: value, 
        next: nil,
    }

    if position == 0 { // if insert current node at start of linked list, then just point current node's next at the old head
        newNode.next = list.head
        list.head = newNode
        return nil 
    }

    prevNode := list.head // traverse through the entire linked list until we reach the node at the desired position
    for i := 0; i < position-1 && prevNode != nil; i++ { 
        prevNode = prevNode.next
    }

    if prevNode == nil { // if insert current node at index outside length of the list, invalid index
        return fmt.Errorf("position out of range") // error
    } else { // assuming no error
        newNode.next = prevNode.next // assigns current node pointer to the previousnode's pointer to insert the current node in between the old previous node and its old next node
        prevNode.next = newNode // assigns pointer from previousnode to point to current node
        return nil
    }

}

func (list *LinkedList) remove(value interface{}) error { // remove a node from the linked list by value

    if list.head == nil { // empty list
        return fmt.Errorf("empty list") // error
    }

    if list.head.value == value { // check if current list head is node with desired value
        list.head = list.head.next
        return nil
    }

    prevNode := list.head // traverse through the entire linked list until we reach the node with the desired value
    for prevNode.next != nil && prevNode.next.value != value {
        prevNode = prevNode.next
    }

    if prevNode.next == nil { // if reach the end of the linked list and the node with desired value not found
        return fmt.Errorf("element not found") // error
    } else { // node with desired value is found and it is the next node
        prevNode.next = prevNode.next.next // assigns pointer from current node to one node after the next node, to effectively "remove" it from the linked list
        return nil
    }

}

func (list *LinkedList) display() { // display all linkedlist nodes
    current := list.head // assigns current node to starting node
    for current != nil { // traverses through the entire linked list from start to finish
        fmt.Printf("%v -> ", current.value)
        current = current.next
    }
    fmt.Println("nil")
}

// --- USES ---
    // advantageous to arraylists
        // faster insertion and deletion of nodes with 0(1) time complexity
        // low memory waste
    // dynamically allocates memory as required
```

### Hash Table

```go
// --- HASH TABLE ---
    // collection of unique entries that enables fast insertion, lookup and deletion of entries by leveraging on hashing and buckets
    // entry: a key-value pair
    // hashing: computing an integer based on a key (formulas vary depending on the key's datatype) to determine an entry's index
    // collision: when hashing a key returns the SAME index for more than one key
    // bucket: indexed storage location for one or more entries that functions like a LINKED LIST, allowing multiple entries to be stored in cases of collision

type Node struct { // type definition for a node in a hashtable
    key   string
    value string
    next  *Node
}

type HashTable struct { // type definition for the actual hash table struct
    size  int
    table []*Node
}

func NewHashTable(size int) *HashTable { // initialize a hash table of a specified size
    return &HashTable{
        size:  size,
        table: make([]*Node, size),
    }
}

func (ht *HashTable) hash(key string) int { // generates an index from a key
    sum := 0
    for _, char := range key {
        sum += int(char)
    }
    return sum % ht.size
}

func (ht *HashTable) Insert(key, value string) { // insert a new key-value pair into the hash table
    index := ht.hash(key)
    newNode := &Node{
        key:   key,
        value: value,
    }

    if ht.table[index] == nil { // handle collisions within the same bucket by implementing a linked list
        ht.table[index] = newNode
    } else {
        current := ht.table[index]
        for current.next != nil {
            current = current.next
        }
        current.next = newNode
    }
}

func (ht *HashTable) Get(key string) (string, bool) { // retrieve a value based on its key within the hash table
    index := ht.hash(key)
    current := ht.table[index]
    for current != nil {
        if current.key == key {
            return current.value, true
        }
        current = current.next
    }
    return "", false
}

func (ht *HashTable) Delete(key string) { // delete a key-value pair from the hash table
    index := ht.hash(key)
    if ht.table[index] == nil {
        return
    }

    if ht.table[index].key == key {
        ht.table[index] = ht.table[index].next
        return
    }

    prev := ht.table[index]
    current := prev.next
    for current != nil {
        if current.key == key {
            prev.next = current.next
            return
        }
        prev = current
        current = current.next
    }
}

// --- USES ---
    // best case CONSTANT time complexity of O(1)
    // worst case LINEAR time complexity of O(n)
        // less efficient for smaller datasets
        // extremely efficient for larger datasets
```

### Graph

```go
// --- GRAPH ---
    // non-linear aggregation of nodes and edges
        // node: vertex that stores data
        // edge: connection between two nodes
        // adjacency: relationship between two nodes when they are connected by an edge
    // UNDIRECTED GRAPH
        // graph with bi-directional adjacency by default
        // eg. graph of a social network
    // DIRECTED GRAPH  
        // graph with uni-directional adjacency by default (arrowheads specify direction adjacency flows in), and bi-directional adjacency has to be specified with two seperate arrows
        // eg. graph of a street map (some roads only allow one-way traffic)
    // graphs can be represented by ADJACENCY MATRIXes or ADJACENCY LISTs as covered below

// --- USES ---
    // representing social networks
    // visualising network routing
    // recommendation engines
    // gps mapping applications
    // knowledge graphs
```

### Adjacency Matrix 

```go
// --- ADJACENCY MATRIX ---
    // 2d nested array which acts as a conceptual representation of adjacency between any two nodes in the graph

type Graph struct { // type definition for an undirected graph represented by an adjacency matrix
    vertices int
    matrix   [][]bool
}

func NewGraph(vertices int) *Graph { // initializes an undirected graph with a given number of vertices
    matrix := make([][]bool, vertices)
    for i := range matrix {
        matrix[i] = make([]bool, vertices)
    }
    return &Graph{
        vertices: vertices,
        matrix:   matrix,
    }
}

func (g *Graph) AddEdge(v1, v2 int) { // adds an undirected edge between two vertices
    if v1 >= 0 && v1 < g.vertices && v2 >= 0 && v2 < g.vertices {
        g.matrix[v1][v2] = true
        g.matrix[v2][v1] = true
    }
}

func (g *Graph) PrintMatrix() { // displays adjacency matrix
    for _, row := range g.matrix {
        fmt.Println(row)
    }
}

// --- USES ---
    // CONSTANT time complexity of O(1)
        // relatively quicker compared to an ADJACENCY LIST for any given dataset
    // QUADRATIC space complexity of O(n^2)
        // less efficient for smaller graph datasets
        // extremely efficient for larger graph datasets
```

### Adjacency List

```go
// --- ADJACENCY LIST ---
    // array of LINKED LISTs, where each LINKED LIST head represents a node and its adjacent nodes

type Node struct { // type definition for a node within an undirected graph 
    vertex int
    next   *Node
}

type Graph struct { // type definition for an undirected graph represented by an adjacency list
    vertices int
    adjList  []*Node
}

func NewGraph(vertices int) *Graph { // initializes an undirected graph with a given number of vertices
    adjList := make([]*Node, vertices)
    return &Graph{
        vertices: vertices,
        adjList:  adjList,
    }
}

func (g *Graph) AddEdge(v1, v2 int) { // adds an undirected edge between two vertices
    if v1 >= 0 && v1 < g.vertices && v2 >= 0 && v2 < g.vertices {
        nodeV1 := &Node{vertex: v2, next: g.adjList[v1]}
        g.adjList[v1] = nodeV1

        nodeV2 := &Node{vertex: v1, next: g.adjList[v2]}
        g.adjList[v2] = nodeV2
    }
}

func (g *Graph) PrintList() { // displays adjacency list
    for vertex, node := range g.adjList {
        fmt.Printf("Vertex %d -> ", vertex)
        for node != nil {
            fmt.Printf("%d ", node.vertex)
            node = node.next
        }
        fmt.Println()
    }
}

// --- USES ---
    // LINEAR time complexity of O(n)
    // UNIQUE space complexity of O(numVertex + numEdge)
        // uses less space compared to an ADJACENCY MATRIX for any given dataset
```

### Binary Search Tree

```go
// --- BINARY SEARCH TREE ---
    // 

// --- USES ---
    // 
```

## Algorithms

### Linear Search

```go
// --- LINEAR SEARCH ---
    // LINEAR time complexity of O(n)
    // iterates through a collection one element at a time
    // pros
        // dataset can be unsorted
        // fast for searching small to medium-sized datasets
        // useful for data structures without random access 
    // cons
        // slow for large datasets

func linearSearch(arr []interface{}, target interface{}) int { // returns index of target element if found
    for i, item := range arr {
        if item == target {
            return i 
        }
    }
    return -1 // returns -1 if target element not found
}
```

### Binary Search

```go
// --- BINARY SEARCH ---
    // LOGARITHMIC time complexity of O(log n)
    // eliminates half of the collection at each step to find the target element
    // pros
        // more efficient for large datasets
    // cons
        // dataset has to be sorted
        // less efficient for smaller datasets

func binarySearch(arr []interface{}, target interface{}) int { 
    left, right := 0, len(arr)-1
    for left <= right {
        mid := left + (right-left)/2
        if arr[mid] == target {
            return mid // returns index of target element
        }
        if arr[mid] < target {
            left = mid + 1
        } else {
            right = mid - 1
        }
    }
    return -1 // returns -1 if target element not found
}
```

### Interpolation Search

```go
// --- INTERPOLATION SEARCH ---
    // average time complexity of O(log(log n))
    // worst case time complexity of O(n)
    // guesses where a value might be based on estimated probe results, with incorrect probes narrowing the search area and recalculating a new probe
    // improvement over BINARY searches that are best used for uniformly distributed datasets
    // pros
        // more efficient for uniformly distributed datasets
        // more efficient for large datasets
    // cons
        // dataset has to be sorted
        // dataset has to be uniformly distributed

func interpolationSearch(arr []interface{}, target interface{}) int {

    low := 0
    high := len(arr) - 1

    for low <= high && target >= arr[low] && target <= arr[high] {
        pos := low + ((target - arr[low]) * (high - low)) / (arr[high] - arr[low])

        if arr[pos] == target {
            return pos // returns index of target element
        }

        if arr[pos] < target {
            low = pos + 1
        } else {
            high = pos - 1
        }

    }

    return -1 // returns -1 if target element not found

}
```

### Bubble Sort

```go
// --- BUBBLE SORT ---
    // QUADRATIC time complexity of O(n^2)
    // CONSTANT space complexity of O(1)
    // compares pairs of adjacent elements and swaps them if they are not in order
    // pros
        // relatively fast for small datasets
        // lower CONSTANT space complexity since collection sorted in place
    // cons
        // extremely slow for medium and large datasets

func bubbleSort(arr []int) { // slices are reference types in Go so changes made to the slice within the function are reflected outside the function
    n := len(arr)
    for i := 0; i < n-1; i++ {
        for j := 0; j < n-i-1; j++ {
            if arr[j] > arr[j+1] {
                arr[j], arr[j+1] = arr[j+1], arr[j]
            }
        }
    }
}
```

### Selection Sort

```go
// --- SELECTION SORT ---
    // QUADRATIC time complexity of O(n^2)
    // CONSTANT space complexity of O(1)
    // iterates across each element in a collection and compares against and stores the minimum value, swapping variables after each iteration
    // pros
        // relatively fast for small and medium datasets
        // lower CONSTANT space complexity since collection sorted in place
    // cons
        // slower for large datasets

func selectionSort(arr []int) { // slices are reference types in Go so changes made to the slice within the function are reflected outside the function
    n := len(arr)
    for i := 0; i < n-1; i++ {
        minIndex := i 
        for j := i + 1; j < n; j++ {
            if arr[j] < arr[minIndex] {
                minIndex = j
            }
        }
        arr[i], arr[minIndex] = arr[minIndex], arr[i]
    }
}
```

### Insertion Sort

```go
// --- INSERTION SORT ---
    // QUADRATIC time complexity of O(n^2)
    // CONSTANT space complexity of O(1)
    // compares all elements to the left of a given element, then shift elements to the right to make room to insert a value
    // pros
        // fast for small and medium datasets
        // fewer steps than BUBBLE SORT
        // best case time LINEAR complexity is O(n) compared to SELECTION SORT'S O(n^2)
        // lower CONSTANT space complexity since collection sorted in place
    // cons
        // slower for large datasets

func insertionSort(arr []int) { // slices are reference types in Go so changes made to the slice within the function are reflected outside the function
    n := len(arr)
    for i := 1; i < n; i++ {
        key := arr[i]
        j := i - 1
        for j >= 0 && arr[j] > key {
            arr[j+1] = arr[j]
            j = j - 1
        }
        arr[j+1] = key
    }
}
```

### Merge Sort

```go
// --- MERGE SORT ---
    // QUASILINEAR time complexity of O(n log n)
    // LINEAR space complexity of O(n)
    // recursively split collection in half, sort each half, then recombine the two halves
    // pros
        // faster than all sorting algorithms with QUADRATIC time complexity (bubble sort, selection sort, insertion sort)
    // cons
        // higher LINEAR space complexity since new subarrays are created to store elements for each level of recursion

func mergeSort(arr []int) []int { 
    if len(arr) <= 1 {
        return arr
    }
    mid := len(arr) / 2
    left := mergeSort(arr[:mid])
    right := mergeSort(arr[mid:])
    return merge(left, right)
}

func merge(left, right []int) []int {
    result := make([]int, 0)
    for len(left) > 0 || len(right) > 0 {
        if len(left) == 0 {
            return append(result, right...)
        }
        if len(right) == 0 {
            return append(result, left...)
        }
        if left[0] <= right[0] {
            result = append(result, left[0])
            left = left[1:]
        } else {
            result = append(result, right[0])
            right = right[1:]
        }
    }
    return result
}
```

### Quick Sort

```go
// --- QUICK SORT ---
    // QUASILINEAR time complexity of O(n log n) in best case and average case
    // QUADRATIC time complexity of O(n^2) in worst case 
    // LOGARITHMIC space complexity of O(log n)
    // moves smaller elements in a collection to left side of a pivot element, then recursively divide the collection into 2 partitions
    // pros
        // faster than all sorting algorithms with QUADRATIC time complexity (bubble sort, selection sort, insertion sort) in best and average cases
        // lower LOGARITHMIC space complexity than MERGE SORT'S LINEAR space complexity since collection sorted in place
    // cons
        // higher LOGARITHMIC space complexity since quick sort relies on recursion 

func quickSort(arr []int) { // slices are reference types in Go so changes made to the slice within the function are reflected outside the function
    if len(arr) <= 1 {
        return
    }
    pivotIndex := partition(arr)
    quickSort(arr[:pivotIndex])
    quickSort(arr[pivotIndex+1:])
}

func partition(arr []int) int {
    pivot := arr[len(arr)-1]
    i := -1
    for j := 0; j < len(arr)-1; j++ {
        if arr[j] < pivot {
            i++
            arr[i], arr[j] = arr[j], arr[i]
        }
    }
    arr[i+1], arr[len(arr)-1] = arr[len(arr)-1], arr[i+1]
    return i + 1
}
```

### Depth First Search

```go
// ---
    // pros
        //
    // cons
        //


```

### Breadth First Search

```go
// ---
    // pros
        //
    // cons
        //


```

## More on

* [primeagen's last algorithms course](https://frontendmasters.com/courses/algorithms/)
* [brocode playlist](https://youtube.com/playlist?list=PLZPZq0r_RZON1eaqfafTnEexRzuHbfZX8&si=Fdvpj73H-3FqQpBs)
* [freecodecamp](https://youtu.be/8hly31xKli0?si=aEbuILtMyqXl5PPu)
* [cs50 week 5 data structures](https://cs50.harvard.edu/x/2024/weeks/5/)
* [cs50 ai with python](https://youtu.be/5NgNicANyqM?si=qYD6DDvV5Fe3n-AA)
* [sorting algorithms explained](https://visualgo.net/en/sorting)
* [sorting algorithms visualised](https://www.toptal.com/developers/sorting-algorithms)
* [big o notation cheatsheet](https://www.bigocheatsheet.com/)