# brief: checks whether an integer is a palindrome, and returns a boolean result
# parameters: 1. input_integer
# eg. check_palindrome(123454321) returns true, check_palindrome(678987) returns false

def check_palindrome(input_integer):
    str_integer = str(input_integer)
    return str_integer == str_integer[::-1]