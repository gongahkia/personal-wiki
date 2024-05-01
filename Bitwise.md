# `Bitwise operations`

Bitwise operations involve manipulation of individual bits.

## Decimal Binary two-way conversion

* Decimal are BASE 10 numbers
* Binary are BASE 2 numbers
* need to know how to convert Decimal to Binary and vice-versa since Bitwise operations involve Binary numbers

### Converting binary to decimal

```go
// --- worked examples --- 

// q1
    // 10110
    // = (1 * math.Pow(2,4)) + (0 * math.Pow(2,3)) + (1 * math.Pow(2,2)) + (1 * math.Pow(2,1)) + (0 * math.Pow(2,0)) 
    // = 16 + 0 + 4 + 2 + 0
    // = 22

// q2
    // 1011 
    // = (1 * math.Pow(2,3)) + (0 * math.Pow(2,2)) + (1 * math.Pow(2,1)) + (1 * math.Pow(2,0))
    // = 8 + 0 + 2 + 1
    // = 11

// q3
    // 11011 
    // = (1 * math.Pow(2,4)) + (1 * math.Pow(2,3)) + (0 * math.Pow(2,2)) + (1 * math.Pow(2,1)) + (1 * math.Pow(2,0))
    // = 16 + 8 + 0 + 2 + 1
    // = 27

// --- go function implementation ---

func binaryToDecimal(binaryStr string) (int) {
    decimal := 0
    power := 1 // represents math.Pow(2,0) = 1
    for i := len(binaryStr) - 1; i >= 0; i-- {
        char := binaryStr[i]
        if char == '1' { // if char == '1' then multiply it against correspondin g power of 2
            decimal += power
        } else {} // if char == '0' then do nothing
        power *= 2  
    }
    return decimal
}
```

### Converting decimal to binary

```go
// --- worked examples --- 

// 26
// = (1 * 16) + (1 * 8) + (0 * 4) + (1 * 2) + (0 * 1)
// = 11010

// 14
// = (1 * 8) + (1 * 4) + (1 * 2) + (0 * 1)
// = 1110

// 35
// = (1 * 32) + (0 * 16) + (0 * 8) + (0 * 4) + (1 * 2) + (1 * 1)
// = 100011

// --- go function implementation --- 

func decimalToBinary(decimal int) string {
    if decimal == 0 {
        return "0"
    }
    binaryStr := ""
    for decimal > 0 {
        remainder := decimal % 2
        digit := '0'
        if remainder == 1 {
            digit = '1'
        }
        binaryStr = string(digit) + binaryStr 
        decimal /= 2
    }
    return binaryStr
}
```

## Bits

* Bits are the smallest units of data available to the programmer
* store single BINARY values of 0 or 1
* BYTE is 8 Bits
* common Bit multiples are 8, 16, 32, 64

## ALU
* arithmetic logic unit
* located within the Computer's CPU
* handles mathematical operations like addition, subtraction, multiplication, division at a BIT level using BITWISE operators
 
## Bitwise operators

> note that the Bitwise syntax might differ between languages *(C, C++, Go, Rust, JS etc)*

* Bitwise operators perform actions on individual Bits by positionally matching single Bits within 2-BIT patterns of equal length  
* recall that Bits store binary values of either 0 or 1

### Logical operators

* Bitwise AND (`&`) returns a **1** if BOTH first AND second Bit are 1, else 0
* Bitwise OR (`|`) returns a **1** if at least first OR second Bit are 1, else 0
* Bitwise XOR aka EXCLUSIVE OR (`^`) returns a **1** if the first and second Bit are DIFFERENT, else 0
* Bitwise NOT aka Bitwise Complement or Bitwise Inversion (`~`) can be used to augment aforementioned operations by returning the inverse of the given Bit (flips 0 to 1 and 1 to 0)

### Bit Shift operators

* Left shift (`<<`) aligns Bits by SHIFTING left operand value LEFT by specified number of Bits in right operand and right is padded with 0, any 1s are truncated
* Signed Right shift (`>>`) aligns Bits by SHIFTING left operand value RIGHT by specified number of Bits in right operand and left is padded with 0, any 1s are truncated
* NOTE: Left shift and Right shift Bit Shift operators **SHOULD NOT** be used for negative numbers since doing so results in undefined behaviour
    * Some languages like Java and JS resolve this by providing the Unsigned Right shift (`>>>`) that always fills left-vacated positions with 0 regardless of the sign of the number
    * C, C++, Rust and Go do not have this added functionality

## Summary

| Operator | Name | Description | Usage |
| --- | --- | --- | --- |
| `&` | Bitwise AND | copies a Bit to the result if it exists in BOTH operands | sets up a mask to check the values of specific Bits |
| `\|` | Bitwise OR | copies a Bit to the result if it exists in EITHER or BOTH operands | adds two numbers if there is no carry involved |
| `^` | Bitwise XOR (Exclusive OR) | copies a Bit to the result if it exists in EITHER but NOT BOTH operands | toggle Bits or swap two variables without using a third temporary variable, find specific types of numbers in a series of numbers, find nonrepeating elements, detect if two integers are of opposite signs
| `~` | Bitwise NOT (Bitwise COMPLEMENT, Bitwise INVERSION) | flips 0 into 1 and 1 into 0 | flip or invert Bits |
| `<<` | LEFT SHIFT | left operand value is SHIFTED LEFT by the number of Bits specified by the right operand and right padded with 0, truncating any 1s | aligns Bits |
| `>>` | Signed RIGHT SHIFT | left operand value is SHIFTED RIGHT by the number of Bits specified by the right operand and left padded with 0, truncating any 1s | aligns Bits |

## Application

```go
// --- PRESETS ---
    // below implementation is in go

var x int 
var y int 
var z int 

x = 6 // decimal 6 in binary => 00000110
y = 12 // decimal 12 in binary => 00001100

// --- BITWISE AND ---

z = x & y // Bitwise AND results in 00000100
z = 4 // binary 00000100 in decimal => 4

// --- BITWISE OR ---

z = x | y // Bitwise OR results in 00001110
z = 14 // binary 00001110 in decimal => 14

// --- BITWISE XOR --- 

z = x ^ y // Bitwise XOR results in 00001010
z = 10 // binary 00001010 in decimal => 10

// --- LEFT SHIFT ---

z = x << 1 // Left shift results in 00001100
z = 12 // binary 00001100 in decimal => 12

// --- RIGHT SHIFT ---

z = y >> 2 // Right shift results in 00000011
z = 3 // binary 00000011 in decimal => 3
```

## More on

* [converting binary to decimal](https://youtu.be/a2FpnU9Mm3E?si=7_bFrtMgsAATZi8y)
* [converting decimal to binary](https://youtu.be/gGiEu7QTi68?si=7v3Jb6G9KorflO4P)
* [converting decimal to hexadecimal](https://youtu.be/GePdOJNnaQg?si=J5cjO6XaJ0-CkPnU)
* [basic bitwise](https://www.techtarget.com/whatis/definition/bitwise)
* [bitwise operations applied](https://youtu.be/BGeOwlIGRGI?si=lbSyji_4bXDprrsD)
* [bitwise operation exercises](https://medium.com/@jeremythen16/master-bitwise-operations-once-and-for-all-f5283e3c9a11)
* [cs50 bitwise operators](https://youtu.be/79i1iu7iyAc?si=Es_JsfqwuX0PQhfe)