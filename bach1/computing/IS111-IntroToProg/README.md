# Learning Python

## Lists

```python
# calling BUILT-IN FUNCTIONS and CONCATENATION on a list don't edit the actual list unless theres a return value
list1:[int1] = [1,2,3]
list1 + [4] # this might even be an execution error lol idk
print(list1)

# calling METHODS on lists alter the actual list even without assigning to a variable
list2:[int] = [1,2,3]
list2.append(4)
print(list2) # prints [1,2,3,4]

# list assignment passes the actual list (memory address and all) to the new variable by reference
list3:[int] = [1,2,3,4,5]
list4 = list3 # passes by reference, now ANY CHANGES made to list3 alter list4's value
list3.append(6)
print(list4) # this will print [1,2,3,4,5,6] and list3 has that value also

# EXCEPTIONS --> CONCATENATION will create a new list
list3 += [9,10,11] # this only affects list3 which is now [1,2,3,4,5,6,9,10,11], list4 will not share that new value since its a pointer to a diff memory address and list4 will isntead retain its OG value of [1,2,3,4,5,6,7]

# COMPLETE REASSIGNMENT of a list literal will create a new reference in memory, so it will not then affect the original list
list1:[int] = [1,2,3,4,5]
list2:[int] = list1 # list2 points to list1 in memory

list2 = [] # reassignment of list2 to become a list literal, list1 and list2 are no longer linked, this applied also if its list2 being reassigned to ANOTHER LITERAL like [6,7,8,9,10]
print(list1) # [1,2,3,4,5]
print(list2) # []

# THIS APPLIES FOR IF LIST1 BECOMES [] instead also! Ultimately be clear who's pointing to who
```

## Slicing 

```python
eg_list:[int] = [1,2,3,4,5,6,7,8,9,10]
sublist1:[int] = eg_list[1:3] # slicing normally appears with 2 arguments, first being inclusive last being exclusive
sublist2:[int] = eg_list[1:] # leaving an argument empty implies the last valid index of the main list
sublist3:[int] = eg_list[1:-1] # a negative index counts from the last index, so in this case its the first element of eg_list to the 2nd to last element
sublist4:[int] = eg_list[1:9:2] # though its not often seen, a third optional argument for slicing is the step size, indicating how many indices to skip between elements, in this case 2; by default the third argument has a value of 1
print(sublist4) # this will print [2,4,6,8,10]
```

## Order of operations

```python
# Precedence ranks top to bottom
** # EXPONENT; first, done right to left 
*, /, %, // # MULTIPLICATION, DIVIDE, MODULO, FLOOR DIVIDE; secoind, done left to right
+,- # ADDITION, SUBTRACTION; third, done left to right

# Precedence ranks top to bottom
not # FIRST
and # SECOND
or # THIRD
```

## De Morgan's Law

```python
assert not (A and B) == not A or not B # True
assert not (A or B) == not A and not B # True
assert not(not(A)) == A # True, 2 nots cancel out each other
```

## Modulo

```python
print(4 % 20) # --- When numerator is larger than denominator, the quotient will be the remainder returned by modulo operator, in this case 4
```

## String multiplication

```python
# Can use the multiplication operator in conjunction with a string to repeatedly print the same string
yes = 'no'
print(f"shit and also {yes * 10}")
```

## Type conversion

```python
print(int(float("20.0"))) # --- this will cause the string "20.0" to undergo type conversion to the float 20.0, which is then type converted to the int 20 (no decimal places), so this will print the int 20 to the console
```

## Importing files

```python
import retail_utility # --- remember to import modules and functions from diff files by using 'import FILENAME' 
```

## List comprehension

```python
int_list = [1,2,3,4,5,6,7,8,9,10]
# list comprehension
new_int_list = [str(num+10000) for num in int_list if num%2!=0]
print(new_int_list)
```

## Dictionary functions

```python
dictionary_example = {1:"yes", 2:"no", 3:"maybe", 4:"ok", 5:"alright"}
# .items() use in a dictionary
new_list = [(value,key) for key,value in dictionary_example.items() if key%2!=0]
print(new_list)
```

## Formatted strings

```python
variable = 20.54
string = f"the value is {variable}"
print(f"shitass look at em: {variable}"})
user_input = input(f"welcome to event, participant {variable}"})

# zero padding is also possible using string.zfill()
another_variable = "20"
padded_variable = another_variable.zfill(5) # this pads the string "20" up to five digits including existing ones
print(padded_variable) # this will print the padded string "00020"
```

## Differentiating between functions and class methods

```python
example_list = ["yes", "no", "eat shit"]
sorted_list = sorted(example_list) # sorted() is a function, and RETURNS a copy of the new sorted list
example_list.sort() # .sort() is a class method called on iterable objects that can be sorted, and does NOT return any value, instead mutating the original list
```

## Python [currying](https://towardsdatascience.com/what-is-currying-in-programming-56fd57103431) 🍛

```python
def sort_strings(huatah):
    length_dict = {string: len(string) for string in huatah}
    final_array = list(element for length, element in sorted((length, element) for element, length in length_dict.items()))
    return final_array
```

## Regarding any time-related questions

Convert all terms to an arbitrary integer that can be easily compared using `<` and `>`.
