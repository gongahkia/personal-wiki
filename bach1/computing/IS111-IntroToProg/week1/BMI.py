try:
  user_weight = float(input("What's your weight (in kg)? "))
  user_height = float(input("What's your height (in m)? "))
  user_BMI = user_weight / user_height ** 2
  print(f"Your BMI is {user_BMI}")
except:
  print("Invalid input detected. Please input a valid weight or height.")
