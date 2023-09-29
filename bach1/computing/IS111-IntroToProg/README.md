# Learning Python

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
