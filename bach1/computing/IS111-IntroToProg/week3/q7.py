# part a
def calculate_max_quantity_and_change(unit_price:float, amount:float):
    max_quantity = amount // unit_price
    change_remaining = amount % unit_price
    return (max_quantity, change_remaining)

# part b
# take in user input
user_allowance = float(input("How much money do you want to spend? $"))

# actual logic
onekg_struct = calculate_max_quantity_and_change(98.5, user_allowance)
fivehundredgram_struct = calculate_max_quantity_and_change(58.5, onekg_struct[1])

print(f"You can buy {int(onekg_struct[0])} 1kg jars and {int(fivehundredgram_struct[0])} 500g jars.")
print(f"You can buy {int(onekg_struct[0] * 1000 + fivehundredgram_struct[0] * 500)} grams of honey. You have ${fivehundredgram_struct[1]} left as your change.")
