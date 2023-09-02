# part a
def calculate_income_tax(annual_taxable_income:float):
    tax_to_pay = 0
    if annual_taxable_income >= 320000:
        tax_to_pay = 44550 + ((annual_taxable_income - 320000) * 0.22)
    elif annual_taxable_income >= 280000:
        tax_to_pay = 36550 + ((annual_taxable_income - 280000) * 0.2)
    elif annual_taxable_income >= 240000:
        tax_to_pay = 28750 + ((annual_taxable_income - 240000) * 0.195)
    elif annual_taxable_income >= 200000:
        tax_to_pay = 21150 + ((annual_taxable_income - 200000) * 0.19)
    elif annual_taxable_income >= 160000:
        tax_to_pay = 13950 + ((annual_taxable_income - 160000) * 0.18)
    elif annual_taxable_income >= 120000:
        tax_to_pay = 7950 + ((annual_taxable_income - 120000) * 0.15)
    elif annual_taxable_income >= 80000:
        tax_to_pay = 3350 + ((annual_taxable_income - 80000)* 0.115)
    elif annual_taxable_income >= 40000:
        tax_to_pay = 550 + ((annual_taxable_income - 40000) * 0.07)
    elif annual_taxable_income >= 30000:
        tax_to_pay = 200 + ((annual_taxable_income - 30000) * 0.035)
    else:
        tax_to_pay = (annual_taxable_income - 20000) * 0.02
    return tax_to_pay

# part b
user_input = float(input("Enter your annual taxable income: "))
print(f"Your total tax is ${calculate_income_tax(user_input)}")
