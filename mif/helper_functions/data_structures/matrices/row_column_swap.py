# brief: reverses a matrice (2-dimensional array) of a given size
# parameters: 1. 2-dimensional array
# eg. reverse_matrice([[1,2,3],[4,5,6],[7,8,9]]) returns [[1,4,7],[2,5,8],[3,6,9]]

def reverse_matrice(matrice):
    output_matrice = []

    # -----
    # ! Add additional arrays here for each row/column in the 2-dimensional array, below is for a 3 by 3 matrice.
    temp_matrice1 = []
    temp_matrice2 = []
    temp_matrice3 = []
    # -----

    for each_matrice in matrice:
        for index in range(0,len(each_matrice)):

    # -----
    # ! Add additional {temp_matrice name}.append(each_matrice[index+{incrementing number}]) line here for each row/column in the 2-dimensional array
            temp_matrice1.append(each_matrice[index])
            temp_matrice2.append(each_matrice[index+1])
            temp_matrice3.append(each_matrice[index+2])
    # -----

            break

    # -----
    # ! Add additional output_matrice.append({temp_matrice name}) here for each row/column in the 2-dimensional array
    output_matrice.append(temp_matrice1)
    output_matrice.append(temp_matrice2)
    output_matrice.append(temp_matrice3)
    # -----

    return output_matrice
