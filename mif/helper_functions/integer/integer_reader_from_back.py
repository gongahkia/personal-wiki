# brief: reverses an integer digit by digit
# parameters: 1. input_integer of type integer
# eg. reverse_integer(12345) returns 54321 of type integer

def reverse_integer(input_integer):
    return int(str(input_integer)[::-1])