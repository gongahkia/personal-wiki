# Binary Tree
    # - non-linear data structure with roots and nodes
    # - root is original first node from which tree branches out
    # - parent and child nodes
    # - last nodes are leaves

class Binary_tree:
# despite being named Binary_tree, this class acts as the default root node of the binary tree
    
    def __init__(self, stored_data):
        self.left = None
        self.right = None
        self.data = stored_data

    def create_node(self, stored_data):
    # method uses recursion to create additional nodes by calling the Binary_tree class again
        if self.data > stored_data:
            if self.left is None:
                self.left = Binary_tree(stored_data)
            else:
                self.left.create_node(stored_data)
        elif self.data < stored_data:
            if self.right is None:
                self.right = Binary_tree(stored_data)
            else:
                self.right.create_node(stored_data)

    def print_tree(self):
    # method similarly uses recursion to call the print_tree() method again for every single value in a given branch
        if self.left:
            self.left.print_tree()
        print(self.data)
        if self.right:
            self.right.print_tree()

root = Binary_tree(0)
root.create_node(111)
root.create_node(1)
root.create_node(11)
root.print_tree()

# FUA: continue adding methods for binary trees from here (https://www.tutorialspoint.com/python_data_structure/python_binary_tree.htm)
