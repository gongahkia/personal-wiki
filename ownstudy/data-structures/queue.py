# Queue
    # - linear data structure
    # - operates on first-in-first-out principle
    # - imagine a literal queue

class Queue:

    def __init__(self, content=[]):
        self.queue:list = content
        print("A queue has been instantiated.")

    def enqueue_element(self, element):
        self.queue.append(element)
        # enqueus element to the back of the queue

    def dequeue_element(self):
        return self.queue.pop(0)
        # dequeus element and removes it from front of the list

# ----------

# testing out the queue
q1 = Queue([1,2,3,4,5,6,7,8,9,10])
print(q1.queue)
q1.enqueue_element(11)
print(q1.queue)
dequeud_element = q1.dequeue_element()
print(f"Dequeud element: {dequeud_element}")
print(q1.queue)
