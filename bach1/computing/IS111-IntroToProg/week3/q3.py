# part a
def get_day_of_week(number:int):
    week_tuple = ("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    return week_tuple[number]

# part b
user_input = int(input("Enter a number indicating the day of the week [0-6]: "))
if user_input > 6:
    print("Your number should be at most 6.")
elif user_input < 0:
    print("Your number should be at least 0.")
else:
    print(get_day_of_week(user_input))
