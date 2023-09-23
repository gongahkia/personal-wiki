# Linked List
    # - linear data structure
    # - values not stored in a contiguous array of bytes, but are linked together using pointers
    # - each node consists of data and a pointer pointing to the next node

class Linked_list:

    def __init__(self):
    # instantiates a linked list class with a single head node
        self.head = None
        print("A linked list has been instantiated.")

    def insert(self, stored_data):
    # inserts a new node to the linked list
        new_node = Node(stored_data)
        if self.head:
            # if head node, then iterate through existing nodes until there is no more, then append the newest node to the linked list
            current = self.head
            while current.next:
                current = current.next
            current.next = new_node
        else:
            # if no header node, creates a head node
            self.head = new_node

    def print_linked_list(self):
        node_count = 0
        current = self.head
        while current:
            node_count += 1
            print(current.data)
            current = current.next
        print(f"Linked list consists of {node_count} nodes.")

class Node:
# a single node of a singly linked list
    def __init__(self, stored_data, next=None):
        self.data = stored_data
        self.next = next
        print("A node has been instantiated.")

# ----------

# testing out the linked list

ll = Linked_list()
ll.insert(1)
ll.insert(2)
ll.insert(3)
ll.insert(4)
ll.insert(5)
ll.print_linked_list()
