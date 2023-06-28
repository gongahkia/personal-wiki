> * Read the [documentation](https://www.lua.org/start.html) and add notes from there if neccesary.  

# `Lua`

[Lua](https://insights.stackoverflow.com/survey/2018/#most-loved-dreaded-and-wanted) is a dynamically typed scripting language that is faster, easier to learn, and more portable than Python.

![](https://cdn.icon-icons.com/icons2/2699/PNG/512/lua_logo_icon_168117.png)

## [Rationale for learning](https://www.quora.com/What-is-the-Lua-programming-language-used-for-Is-it-still-used-in-these-days)

* Lightweight, extensible with C libraries
* Fast VM
* Embeddable and scaleable 
* Luarocks package manager
* Neovim scripting
* Game development in Lua 
    * [LOVE 2D](https://youtu.be/I549C6SmUnk)
        * [Here also](https://youtu.be/ZQCky-_Ad5Y)
        * [Documentation](https://www.lua.org/)

## Quickstart

Lua files end with the `.lua` file extension.

### Printing to the console `and` comments

* `print()`
* `--`
* `--[[ ]]`
* `..`

```lua
hello = "ah shit"
-- dynamically typed language, so no type annotations are required

print(hello)
print("this is the same as python")

-- this is a single-line comment
--[[ 
    this is a multi-line comment
]]

local name = "gongahkia"
print("my name is " .. name .. "and thanks") -- .. handles string concatenation
```

### Variables

Variables can be initialized without a value, and will be automatically assigned the value *nil*.

* `local` indicates the variable is **scoped locally**.

```lua
local local_variable = "shit" -- creates a variable within the local scope
```

### Data types

* `nil`
* `number`
* `string`
* `boolean`
* `tables`
* `type()`

```lua
local eg_nil -- an initialized variable with no value is assigned the nil value
local eg_nil2 = nil -- you can also manually assign it to clear and reassign variables
local eg_number = 2
local eg_string = "ah shit thanks so much g"
local eg_boolean = true -- could be false also
local eg_table = {
    ['ah'] = "shit",     
    ['good'] = "morning",
    ['people'] = "thanks"
}
print(type(eg_table)) -- type() will print the data type of a given variable in Lua, and will return the type table in this case
```

### Arithmetic

Computer arithmetic follows BODMAS rules.

* `+`/`-`/`*`/`/`
* `^`
* `%`
* [Other Lua math functions](https://www.lua.org/pil/18.html)

### Strings

* Lua takes *strings* and *characters* as the **same thing**, similar to Python.
* `""`, `''` and `[[ ]]` are all allowed.

```lua
local eg_string = "This is a string!"
print(eg_string) -- prints the "This is a string" string to the console

local eg_also_string = "This is also a string"
print(eg_also_string) -- prints the "This is also a string" string to the console

local eg_multi_line_string = [[ 
    Hello World!
    2 + 2 = 4
]]
print(eg_multi_line_string) -- prints the "\n\tHello World!\n\t2 + 2 = 4" multi-line string including the newline character and tab character of the string
```

* `#` returns length of string

```lua
local str = Hello
print(#str) -- returns the length of the string "Hello", 5
```

### Functions

Lua has **no classes**. Functions are all we have in Lua.

* `function`, `end`
* `return`

```lua
function doMath(n)
    return n*2
end 

function voidFunc()
    print("shit ass")
end

doMath(2)
voidFunc()
```

### Data structures

Lua only has **one** data structure, the `table`, which is actually an associative array / dictionary.

* Lua tables are **one-indexed** *(array indexes start from 1)*!
* `{}` used to indicate a table.
* **Nested tables** are possible as well!

```lua
array = {"shit", "ass", "hat"}
-- creating a conventional array using a table
-- lua tables are one-indexed!

dict = {
    ['shit'] = "help",
    ['thanks'] = "chief"
}
-- creating a dictionary using a table

for key,value in pairs(dict) do
    print(key)
    print(value)
end
-- iterating over said table with a for loop

local nested_array {
    {1,2,3,8,10},
    {6,8,0},
    {9,99,989}
}

for i = 1, #nested_array do
    for j = 1, #nested_array[i] do
        print(nested_array[i][j]) -- prints every value within the nested array
    end
end
```

### Conditional flow `and` logical operators

Lua uses the `then` and `end` keywords to indicate the start of conditional flow code blocks, similar to Bash.

* `then`, `end`
* `if`
* `elseif`
* `else`
* `>`, `<`, `>=`, `<=`, `=`, `==`, `===`, `!=`, `!==`
* `and`
* `or`

```lua
if false then 
    print("this shit hella false bruv") -- an example if statement
end
```

### Loops

Lua uses the `do` and `end` keywords to indicate the start of loop code blocks, similar to Bash.

* `do`, `end`
* `for` loops
* `while` loops
* `repeat`, `until` loops *(equivalent of a `do while` loop that runs at least once)*

```lua
for i = 2, 1000, 1 do
    print(i) -- similar format to a classic for loop in C
end

local peeps = 10
while peeps > 0 do
    peeps = peeps - 1
    print("People count:" ... peeps) -- similar format to a classic while loop in C
end

local x = 1
repeat
    print("Hey there!") -- the equivalent of a do while loop that runs at least one time
    x = x + 1
until x > 10
```

### [Coroutines](https://www.lua.org/pil/9.1.html)

### [Lua modules](http://lua-users.org/wiki/ModulesTutorial)

### [OOP in Lua](https://www.lua.org/pil/16.html)

As previously mentioned, Lua has no classes or objects in the same way as Rust, and only has functions. However, we can use tables to simulate objects *(like structs in Rust)*.

### [Metamethods](https://www.lua.org/pil/13.html)
