> add notes from The contents of data structures can also be iterated using each.

# `Ruby`

In Ruby, everything is an object.

## Printing

```ruby
puts "this prints with a newline" # puts
print "this prints without a newline/n" # print
```

## Comment

```ruby
# single-line comment

=begin
this
is
a
multi-line
comment
=end

# everything is an object in Ruby

3.class # Integer
"Hello".class # String
"Hello".method(:class).class # Method
:pending.class # Symbol
```

## Variables

```ruby
# VARIABLE
    # variable assignment returns the value assigned
    # by convention, snake_case used for variable names

a_variable = 25
another_variable = "awesome sauce"
yet_another_variable = true

x = y = 10 # this will assign 10 to y, and that will then return 10 which is assigned to x
```

## Types

```ruby

# ---------- STRING ----------

eg_string = "hello world" 

# FORMATTED STRINGS

placeholder = "watermelon"
put "I really am the largest #{placeholder} in this County." # formatted string

# STRING CONCATENATION
    # + can concatenate two strings together but not with other types

"hello" + "world" # "hello world"
"hello" + 3.to_s # "hello3", this is okay because the number 3 undergoes type conversion
"hello#{3}" # "hello3"
"hello" << " world" # "hellow world", appends a string to a string

# STRING MULTIPLICATION
    # just like Python!

"hello" * 3 # evaluates to "hellohellohello"
"ok and " * 2 # evaluates to "ok and ok and "

# ---------- NUMBER ----------
    # covers integers, floats and doubles like Typescript

eg_number = 1000
another_eg_number = 1.23

# ARITHMETIC OPERATORS
    # arithmetic is just syntatic sugar for calling a method on an object

1 + 1 # addition
8 - 1 # subtraction
10 * 2 # multiplication
35 / 5 # division
2 ** 5 # power 
5 % 3 # modulo
1 < 10 # comparison operators
1 > 10 # comparison operators
1 <= 10 # comparison operators
1 >= 10 # comparison operators
1 == 1 # complete equality, of both type and value
1 != 1 # complete inequality, of both type and value

# ---------- BOOLEAN -----------

eg_bool = true # of the TrueClass
other_eg_bool = false # of the FalseClass

# LOGICAL OPERATORS

true && false # and, evaluates to false
true || false # or, evaluates to true

# ---------- SYMBOL ----------
    # symbols are immutable reusable constants often used in place of strings to convey specific meaning
    # represented internally by an integer

status = :pending
status == :pending # returns true
status == "pending" # returns false
status == :approved # returns false
status == "pending".to_sym # returns true since type conversion of String to Symbol

# ---------- SPECIAL VALUES ----------
nil # evaluates to false, same as null, of the NilClass
```

## Data structures

```ruby
# BOTH ARRAYS AND HASHES ARE ENUMERABLE!

# ---------- ARRAY ----------
    # similar to Javascript arrays, arrays can store multiple values of different types

eg_array = [1,2,3,4,5]
mixed_array = [1, "hello", true]
another_mixed_array = %w[foo bar baz] # this evaluates to ["foo", "bar", "baz"]

# ARRAY METHODS

# INDEXING and SLICES
    # similar to Python
    # this is also just syntatic sugar to call the method [] on the array object

eg_array[0] # returns 1
eg_array.first # also returns 1
eg_array[12] # returns nil since index out of range

eg_array[-1] # returns last element, 5
eg_array.last # also returns 5

eg_array[2,3] # returns subarray of start index and length, [3,4,5]
eg_array[1..3] # returns a subarray of indexes specified in a range, similar to Python, [2,3,4]

## APPEND VALUE

eg_array << 6 # eg_array is now [1,2,3,4,5,6]
eg_array.push(6) # this does the same as the above

## CHECK IF VALUE IN ARRAY

eg_array.include?(1) # evaluates to true
eg_array.include?(100) # evaluates to false

# REVERSE AN ARRAY

[1,2,3].reverse # evaluates to [3,2,1]

# ---------- HASH ---------- 
    # similar to PHP's associative arrays
    # the equivalent of Python's dictionaries or Javascript's objects
    # stores key-value pairs
    # you can use symbols for keys also

eg_hash = {
    "color" => "green",
    "number" => 5
}

eg_hash.keys # returns ["color", "number"]
eg_hash["color"] # returns "green"
eg_hash["number"] # returns 5
eg_hash["nothing here"] # returns nil since the key doesn't exist 

# to use symbol as keys
another_hash = {
    :defcon => 3,
    :action => true
}
another_hash.keys # returns [:defcon, :action]

# an alternative syntax when using symbols for keys

yet_another_hash = {
    defcon:3, 
    action:true
}
yet_another_hash.keys # also returns [:defcon, :action]

yet_another_hash.key?(:defcon) # checks the existence of keys in hash, evaluates to true
yet_another_hash.value?(3) # checks the existence of values in hash, evaluates to true
```

## Control structures and logic flow

```ruby
# CONDITIONALS
    # similar syntax to bash
    # postfix-if notation is available also

if true
    "if statement"
elsif false
    "else if, optional"
else
    "else, also optional"
end

warnings = ["Patronimic is missing", "Address is too short"]
puts("Some warnings occured:\n" + warnings.join("\n")) if !warnings.empty? # postfix-if notation can be used for single statements with no code blocks
puts("Some warnings occured:\n" + warnings.join("\n")) unless warnings.empty? # unless can be used in place with if

# LOOPS
    # traditional for loops aren't common
    # basic loops are implemented with each enumerable
    # also similar syntax to bash and rust

# APPROVED SYNTAX AND COMMONLY SEEN
(1..5).each do |counter|
    puts "this is the ${counter}"
end

# this is also approved syntax since blocks can be wrapped in curly braces
(1..5).each {|counter| puts "this is also the #{counter}"}

# APPROVED SYNTAX BUT RARELY SEEN
for counter in 1..5
    puts "iteration #{counter}"
end
```

## More on

* [ruby ecosystem](https://learnxinyminutes.com/docs/ruby-ecosystem/)
* [ruby documentation](https://www.ruby-lang.org/en/documentation/quickstart/)