# Stack
    # - linear data structure
    # - operates on a last-in-first-out principle
    # - top of stack defined as element with index 0
    # - imagine a stack of plates

class Stack:

    def __init__(self, contents=[]):
        self.stack:list = contents
        print("A stack has been instantiated.")

    def push_element(self, value):
        self.stack.insert(0,value)
        # pushes element to the top of the stack

    def pop_element(self):
        return self.stack.pop(0)
        # pops element from the top of the stack

# ----------

# testing out the stack

s1 = Stack([1,2,3,4,5,6,7,8,9,10])
s1.push_element(0)
print(s1.stack)
popped_element = s1.pop_element()
print(s1.stack)
