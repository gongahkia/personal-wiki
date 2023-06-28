> * Continue learning from [this video](https://www.youtube.com/watch?v=1srFmjt1Ib0) at `40:52`.  
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

### Printing to the console && comments

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

### Functions

Lua has **no classes**. Functions are all we have.

* `function`, `end`, `return`

```lua
function doMath(n)
    return n*2
end 

doMath(2)
```

### Data structures

Lua only has **one** data structure, the `table`, which is actually an associative array / dictionary.

* Lua tables are **one-indexed** *(array indexes start from 1)*!
* `{}`

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
```

### Conditional flow

```lua

```

### [Coroutines](https://www.lua.org/pil/9.1.html)
