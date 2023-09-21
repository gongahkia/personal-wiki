# Week 1

```python
print(4 % 20) # --- When numerator is larger than denominator, the quotient will be the remainder returned by modulo operator, in this case 4

# Can use the multiplication operator in conjunction with a string to repeatedly print the same string
yes = 'no'
print(f"shit and also {yes * 10}")
```
# Week 3

```python
print(int(float("20.0"))) # --- this will cause the string "20.0" to undergo type conversion to the float 20.0, which is then type converted to the int 20 (no decimal places), so this will print the int 20 to the console
```

# Lab Test Reminders!

```python
import retail_utility # --- remember to import modules and functions from diff files by using 'import FILENAME' 
```

# List comprehension

```python
int_list = [1,2,3,4,5,6,7,8,9,10]
# list comprehension
new_int_list = [str(num+10000) for num in int_list if num%2!=0]
print(new_int_list)
```

# Dictionaries

```python
dictionary_example = {1:"yes", 2:"no", 3:"maybe", 4:"ok", 5:"alright"}
# .items() use in a dictionary
new_list = [(value,key) for key,value in dictionary_example.items() if key%2!=0]
print(new_list)
```
