import random
import os 

# snake class

class snake:

    def __init__(self, health = 5, length = 1, coordinate_array = [[0,0]]):
        self.health = health
        self.coordinate_array = coordinate_array
        self.fruit_coordinate_array = [[random.randint(0,50), random.randint(0,20)], [random.randint(0,50), random.randint(0,20)]]
        self.length = length
        print("snake has been instantiated")

    def move(self):
        # FUA: implement check to see whether snake is moving into itself or going out of bounds
        direction = input("W/A/S/D\n").lower()
        match direction:
            case "w":
                self.coordinate_array.append([self.coordinate_array[-1][0], self.coordinate_array[-1][1] - 1])
                # self.coordinates[1] -= 1
            case "a":
                self.coordinate_array.append([self.coordinate_array[-1][0] - 1, self.coordinate_array[-1][1]])
                # self.coordinates[0] -= 1
            case "s":
                self.coordinate_array.append([self.coordinate_array[-1][0], self.coordinate_array[-1][1] + 1])
                # self.coordinates[1] += 1
            case "d":
                self.coordinate_array.append([self.coordinate_array[-1][0] + 1, self.coordinate_array[-1][1]])
                # self.coordinates[0] += 1

        self.coordinate_array = self.coordinate_array[-self.length:] # updates by removing the stray coordinates from coordinate_array as per length to allow snake to maintain consistent length 

    def eat_fruit(self):
        self.length += 1
        self.fruit_coordinate_array.remove(self.coordinate_array[-1]) # removes the eaten fruit from the fruit_coordinate_array
        self.fruit_coordinate_array.append([[random.randint(0,50), random.randint(0,20)]]) # respawns another fruit
        # FUA: debug why this above line does not respawn the above fruit??

#----------

# visualising the grid

# FUA: figure out why fruit moves one coordinate to the right, track excess space coordinate when on same row
def print_grid(snake_coordinate_list:list, fruit_coordinate_list:list, snake_length:int):
    print("-" * 52)
    for y in range(21):
        row_string = ""
        print("|",end="")
        for x in range(51):
            for fruit_coordinate in fruit_coordinate_list:
                if [x,y] == fruit_coordinate:
                    row_string += "O"
                    # print("O", end="")
            for snake_coordinate in snake_coordinate_list:
                if [x,y] == snake_coordinate:
                    row_string += "X"
                    # print("X",end="")
            else:
                row_string += " "
                # print(" ",end="")
        row_string = row_string[:50]
        print(row_string, end="")
        print("|")
    print("-" * 52)
    print(f"Score: {snake_length}")

# make edits here to event loop
def event_loop():
    # coordinate_matrix is implied, not explicitly instantiated
    s1 = snake(1, 1, [[0,0]])
    while True: 
        s1.move()
        os.system("clear")
        # print(s1.coordinate_array)
        # print(s1.fruit_coordinate_array)
        if s1.coordinate_array[-1] in s1.fruit_coordinate_array:
            s1.eat_fruit()
        print_grid(s1.coordinate_array, s1.fruit_coordinate_array, s1.length)

# --- 

# actual running code

event_loop()
