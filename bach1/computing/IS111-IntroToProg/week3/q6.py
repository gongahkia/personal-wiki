# part a
def get_discount_rate(num_boxes:int):
    if num_boxes >= 5:
        discount_rate = 0.2
    elif num_boxes >= 2 and num_boxes <= 4:
        discount_rate = 0.1
    else: 
        discount_rate = 0.0
    return discount_rate

# part b
def calculate_total_amount(brand:str, num_boxes:int):
    if brand == "tung lok":
       amount_due = 55.40 * num_boxes * (1 - get_discount_rate(num_boxes))
    elif brand == "man fu yuan":
       amount_due = 59.60 * num_boxes * (1 - get_discount_rate(num_boxes))
    else:
        print("Edge case detected")
    return amount_due

# part c

user_mooncake_brand = input("Which brand do you want to buy? ").lower()
user_num_boxes = int(input("How many boxes do you want to buy? "))
print(f"You need to pay ${calculate_total_amount(user_mooncake_brand, user_num_boxes)}")
