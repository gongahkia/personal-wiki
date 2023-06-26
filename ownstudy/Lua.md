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

### Printing to the console

* `print()`

```lua
hello = "ah shit"
print(hello)
-- this is a comment
-- dynamically typed language, so no type annotations are required
```

### Data types

```lua

```

### Functions

Lua has **no classes**.

* `function`, `end`, `return`

```lua
function doMath(n)
    return n*2
end 

doMath(2)
```

### Data structure

Lua only has **one** data structure, the `table`, which is actually an associative array / dictionary.

* Lua is **one-indexed** *(array indexes start from 1)*!
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

### [Coroutine](https://www.lua.org/pil/9.1.html)
