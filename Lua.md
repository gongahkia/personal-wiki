# `Lua`

## Comments

```lua
-- single-line comment

--[[ 
this is 
a 
multi-line
comment
]]--
```

## Printing

```lua
print("huat ah, this is just like python")
```

## Types

```lua
-- ---------- VARIABLE ----------
    -- variables are global by default
    -- local makes them local

iAmGlobal = "Gojo Satoru"
local iAmLocal = "Geto Suguru"

-- ---------- TYPE ----------
    -- number (integers, floats, doubles)
    -- string (single, double-quoted, [[]] square brackets; strings are immutable like python)
    -- boolean (true, false; only nil and false evaluate to false, everything else evalautes to true including 0 and '')
    -- nil (no data; undefined variables evalaute to nil, and this is not an error)

aNumber = 42
anotherNumber = 12.890
aString = "watermelon"
aMultilineString = [[ Double brackets for 
                      multi-line
                      string ]]
aBoolean = true
aNil = nil
foo = anUndefinedVariable -- this evaluates to nil, not an error
```

## Control structures

```lua
-- ---------- BLOCK -----------
    -- lua takes a page from bash's syntax in seperating its code blocks with do/end and indentation of block contents

-- ---------- IF ELSEIF ELSE THEN ----------
    -- standard conditionals
    -- there are no switch case statements in lua

if num > 40 then 
    print("Over 40")
elseif num == 20 then
    print("It's 20")
else 
    print("Lovely")

-- ---------- LOOP ----------

-- FOR LOOP
    -- for {VARIABLE BEGINNING}, {VARIABLE END}, {STEP}
    -- ranges are inclusive on both ends

for i=10, 1, -1
do 
    print(i) -- prints 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 to the console
end

for q=1,100 do 
    print(q) -- prints the numbers 1 to 100 to the console
end

-- FOR IN LOOP
    -- useful when looping over iterable data structures

for key,value in pairs(aTable) 
do
    aTable[key] = "i am jujutsu kaisen"
end

-- WHILE LOOP
    -- while do loop
    -- probably what you're used to seeing

while num < 50 do 
    num = num + 1
end

-- REPEAT UNTIL LOOP
    -- kinda like a do while loop
    -- similar to Rust's loop construct

breakCase = 10
repeat
    print("watermelon")
    breakCase = breakCase - 1
until breakCase == 0
```

## Data structures

```lua
-- --------- WARNING ---------
    -- indice count starts at 1 in lua
    -- crazy, i know

-- ---------- TABLE ----------
    -- lua's only data structure is the table, similar to PHP's associative arrays (both a dictionary and a list)
    -- initialised using {} curly braces
    -- table keys are string values by default so quotation marks are unnecessary
    -- table keys can also be non-string values, and are declared via literal notation
    -- table values can be accessed via . dot notation or [] square bracket notation
    -- list literals have implicit int keys set up, just not explicitly stated

aTable = {key1 = "value1", key2 = false}
print(aTable.key1) -- prints the value associated with the key key1 in aTable
aTable.key3 = 200 -- adds a new key value pair to aTable of key key3 and value 200
aTable.key2 = nil -- removes key2 from the table

anotherTable = {["@!#"] = 'qbert', [{}] = 1729, [6.29] = "tau"}
print(anotherTable[6.29]) -- prints the string value "tau"

aListButActuallyATable = {"value1", 'value2', 1.21, true}
print(#aListButActuallyATable) -- # returns the size of the table
for i = 1, #aListButActuallyATable do  -- recall that indices start at 1 in lua
    print(v[i]) -- this syntax works since we're technically using tables with implicit indice keys 
end
```

## Functions

```lua
-- ---------- FUNCTIONS ----------
    -- function/end declares a function block
    -- return works as expected
    -- anonymous functions are allowed
    -- functions are first-class and can be fed into other functions
    -- fuction scope can be specified using the local/global keyword

function fib(n)
    if n < 2 then 
        return 1 
    end 
    return fib(n-2) + fib(n-1)
end
```

## More on

* io.read()
* ~=
* _G global table
* metatables
* metamethods
* class-like tables
* inheritance
* modules
* [learn lua in y minutes](https://learnxinyminutes.com/docs/lua/)
* [lua book](http://www.lua.org/pil/contents.html)
* [lua cheatsheet](https://devhints.io/lua)
