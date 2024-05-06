# `Data Structures and Algorithms`

Code snippets below are written in [Go](https://go.dev/).

## Definitions

* data structure: ordered collection of data provided by the language or defined and enforced by the programmer
* algorithm: sequence of steps to solve a problem

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

## Algorithms

> FUA add here later from Brocode

## More on

* [primeagen's last algorithms course](https://frontendmasters.com/courses/algorithms/)
* [brocode playlist](https://youtube.com/playlist?list=PLZPZq0r_RZON1eaqfafTnEexRzuHbfZX8&si=Fdvpj73H-3FqQpBs)
* [freecodecamp](https://youtu.be/8hly31xKli0?si=aEbuILtMyqXl5PPu)
* [cs50 week 5 data structures](https://cs50.harvard.edu/x/2024/weeks/5/)
* [cs50 ai with python](https://youtu.be/5NgNicANyqM?si=qYD6DDvV5Fe3n-AA)
* [sorting algorithms explained](https://visualgo.net/en/sorting)
* [sorting algorithms visualised](https://www.toptal.com/developers/sorting-algorithms)