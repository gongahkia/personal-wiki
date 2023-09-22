import random
import os 

# snake class

class snake:

    def __init__(self, health = 5, length = 1, coordinate_array = [[0,0]]):
        self.health = health
        self.coordinate_array = coordinate_array
        self.fruit_coordinate_array = [[random.randint(0,50), random.randint(0,20)], [random.randint(0,50), random.randint(0,20)],[random.randint(0,50), random.randint(0,20)]]
        self.length = length
        # print("snake has been instantiated")

    def move(self):
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

        # check whether snake is moving into itself 
        if self.coordinate_array[-1] in self.coordinate_array[:-1]:
            self.health -= 1
            # print("snake dead")

        # check whether snake is going out of bounds
        if self.coordinate_array[-1][0] < 0 or self.coordinate_array[-1][0] > 50 or self.coordinate_array[-1][1] < 0 or self.coordinate_array[-1][1] > 20:
            self.health -= 1
            # print("snake dead")

    def eat_fruit(self):
        self.length += 1
        self.fruit_coordinate_array.remove(self.coordinate_array[-1]) # removes the eaten fruit from the fruit_coordinate_array
        self.fruit_coordinate_array.append([random.randint(0,50), random.randint(0,20)]) # respawns another fruit

#----------

# visualising the grid

# FUA: figure out why fruit moves one coordinate to the right, track excess space coordinate when on same row
# FUA: figure out how to add colored text to the terminal
def print_grid(snake_coordinate_list:list, fruit_coordinate_list:list, snake_length:int):
    print("-" * 52)
    for y in range(21):
        row_string = ""
        print("|",end="")
        for x in range(51):
            for fruit_coordinate in fruit_coordinate_list:
                if [x,y] == fruit_coordinate:
                    row_string += "O"
                    # row_string += colored("O","red")
            for snake_coordinate in snake_coordinate_list:
                if [x,y] == snake_coordinate:
                    row_string += "X"
                    # row_string += colored("X","green")
            else:
                row_string += " "
                # print(" ",end="")
        row_string = row_string[:50]
        print(row_string, end="")
        print("|")
    print("-" * 52)
    print(f"Score: {snake_length-1}")

def print_title_screen():
    os.system("clear")
    username = input("Your name: ")
    os.system("clear")
    while True:
        consent = input(f"Welcome {username}. Do you love IS111?\n[Y]es/[N]o\n").lower()
        match consent:
            case "y":
                os.system("clear")
                print("Stop coping :(\nTry to answer that again.\n")
                continue
            case "n":
                os.system("clear")
                print("I hope you enjoy this 'game' as much as I enjoyed making it.")
                break
            case _:
                print("Invalid input. 'Y' or 'N' bruh how hard can that be?")
                continue

# make edits here to event loop
def event_loop():
    print_title_screen()
    # coordinate_matrix is implied, not explicitly instantiated
    s1 = snake(1, 1, [[0,0]])
    while s1.health > 0: 
        s1.move()
        os.system("clear")
        # print(s1.coordinate_array)
        # print(s1.fruit_coordinate_array)
        if s1.coordinate_array[-1] in s1.fruit_coordinate_array:
            s1.eat_fruit()
        print_grid(s1.coordinate_array, s1.fruit_coordinate_array, s1.length)

    os.system("clear")
    print(f"You lost. Your score is {s1.length-1}")

# --- 

# actual running code

event_loop()
