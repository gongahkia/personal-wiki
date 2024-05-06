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

## Algorithms

> FUA add here later from Brocode

## More on

* [primeagen's last algorithms course](https://frontendmasters.com/courses/algorithms/)
* [brocode playlist](https://youtube.com/playlist?list=PLZPZq0r_RZON1eaqfafTnEexRzuHbfZX8&si=Fdvpj73H-3FqQpBs)
* [freecodecamp](https://youtu.be/8hly31xKli0?si=aEbuILtMyqXl5PPu)
* [cs50 week 5 data structures](https://cs50.harvard.edu/x/2024/weeks/5/)
* [cs50 ai with python](https://youtu.be/5NgNicANyqM?si=qYD6DDvV5Fe3n-AA)