# Bern

<img align="center" src="img/logo.png" alt="Bern Logo" width="400"/>
<p align="center">An interpreted, dynamically typed programming language</p>

<p align="center">
  <a href="https://miragem.app.br/">
    <img alt="Project Status" src="https://img.shields.io/badge/Status-Development%20(Pre--Alpha)-red"
  ></a>
  <a href="#technology">
    <img alt="Language" src="https://img.shields.io/badge/Built%20With-Haskell-purple"
  ></a>
</p>

## About
Bern is a small, interpreted, dynamically typed programming language focused on clarity and expressiveness.

It blends ideas from functional programming (inspired by Haskell) with a lightweight, imperative syntax influenced by Lua and Odin.

Bern is designed so that:

* Everything is an expression
* Literals and expressions are evaluated and printed automatically
* Syntax stays minimal and readable
* Control flow is explicit and uniform

The goal is to make experimentation, scripting, and learning feel natural without sacrificing structure.

## Language Overview

### Literals and Evaluation

In Bern, literals and expressions are evaluated immediately:

```
"Hello, World!"
23
```

There is no need for an explicit print function - evaluation implies output.

---

### Variables

Variables are defined with a simple assignment syntax:

```
integer = 2
double = 3.14
helloworld = "Hello, World!"
```

Variables can be used directly in expressions:

```
"Result from variable \"helloworld\": " + helloworld
```

---

### Conditionals

Bern provides clear `if / else if / else` control flow:

```
age = 18

if age >= 18 then
    "You are an adult."
else
    "You are a minor."
end
```

Multiple conditions can be chained naturally:

```
score = 85

if score >= 90 then
    "You got an A."
else if score >= 80 then
    "You got a B."
else if score >= 70 then
    "You got a C."
else
    "You need to improve."
end
```

---

### Loops

Bern uses a single keyword, `for`, with different forms depending on context.

#### Repeat Loop

```
for 3 do
    "Hello, Loops!"
end
```


#### Conditional Loop

```
counter = 2

for counter > 0 do
    "Hello, Loops!"
    counter = counter - 1
end
```

#### Infinite Loop

```
for true do
    "Hello!"
end
```

---

### For-In Iteration

Iterating over a string:

```
text = "Bern"

for char : text do
    "Current char is: " + char
end
```


With automatic indexing:

```
text = "Bern"

for char, index : text do
    "Current char is: " + char
    "Current index is: " + index
end
```

## FizzBuzz in Bern

A complete FizzBuzz example demonstrating variables, conditionals, and loops:

```
numbers = [1..100]
for n : numbers do 
    if n % 15 == 0 then 
        "FizzBuzz"
    else if n % 3 == 0 then
        "Fizz"
    else if n % 5 == 0 then
        "Buzz"
    else
        n
    end
end
```

Each branch evaluates to a value, which is automatically printed.


## Technology

* Interpreter written in Haskell
* Dynamically typed
* Expression-oriented evaluation model


## Status

Bern is currently in pre-alpha and under active development.
Syntax and semantics may change as the language evolves.

Contributions, feedback, and experimentation are welcome.
