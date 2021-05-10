(**
---
title: Task 04 
category: Tasks(english)
categoryindex: 3
index: 5
---
*)


(**

[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/csbiology/BIO-BTE-12-V-4/gh-pages?filepath={{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

# Task04 - Programming with functions

## Higher order functions

### Task 1.1:

Rewrite the following function by using the pipe operator `|>` and the given functions.

Clue: 

  - Pipe operators lead to a positioning of the parameter left of the operator to the end of the right side: 

  - `a |> add b` is equal to `add b a`
*)
(******)

let mathNotation x y = ((2. + 3. * (x + 1.))**2.) * y

let add (a:float) b = a + b
let square (x :float) = x * x
let multiply (a:float) b = a * b

let pipeNotation x y =
    1. //replace me

// Test if both notations report the same result
pipeNotation 1. 7. = mathNotation 1. 7.


(**

### Task 1.2

Declare any function with the signature `int -> int -> int`. Make sure that all parameters are used within the function body.

Is it a first order function or a higher order function?

*)
(******)

(**
### Task 1.3


Declare any function with the signature `int -> int -> (int -> int -> int) -> float`. Make sure that all parameters are used within the function body.

Is it a first order function or a higher order function?

*)
(******)

(**
### Task 1.4

Call the function defined in `1.3`. Use two random values and the function from `1.2` as parameters.

*)
(******)


(**

## Programming with functions

### Task 2.1.1

Below you can see a record type called `Cat`. Create two random values of type `Cat` and bind them to random names.
Have a look at `Exercise 02` if you have any difficulties here.

*)
(******)

type Cat = {
    Name    : string
    Weight  : float
    Color   : string
    Age     : int}

(**
### Task 2.1.2

Declare a function called `getWeight`, that takes a `Cat` as parameter and reports its weight.

*)
(******)

(**
### Task 2.1.3

You cannot apply standard mathematical operators to record types. 
Declare a function called `applyToWeight` that receives two parameters of type Cat (c1 and c2) and one parameter of type `(float -> float -> float)` (f). 
In the function body, the function f should be applied to the weights of the two cats c1 and c2.

Clue: Define the types of the parameters. 

*)
(******)

(**
### Task 2.1.4
 
Apply the function `applyToWeight` with the previously created Cats and the function `averageOfTwo`.  

*)
(******)

let averageOfTwo (a:float) b =
    (a + b) / 2.

(**
### Task 2.2 (optional)

This task does not have to be solved! It serves to create a cat list that is required later.
If you want, you can comment on what happens in the indicated lines (extra point).

*)
(******)

// This is a random number generator. 
// randomNumberGenerator.NextDouble() creates a random float between 0. and 1.
// randomNumberGenerator.Next(x) creates a random int between 0 and x-1.
let randomNumberGenerator = System.Random()

// Here a function is defined that creates a value of typ Cat.
let createRandomCat (i :int) = 
    // Comment: 
    let name   = "Cat " + string i 
    // Comment: 
    let age    = randomNumberGenerator.Next(16)
    // Comment: 
    let weight = 3. + 5. * randomNumberGenerator.NextDouble()
    let color  = 
        let colors = ["white";"red";"black";"grey";"mixed"]
        // Comment: 
        colors.[i%5]
    // Comment: 
    {
    Name    = name
    Weight  = weight
    Color   = color   
    Age     = age
    }

// Comment: 
let catList = [for i=1 to 60 do yield createRandomCat i]


(**
### Task 2.3

Create a float list with the values from 0. to 100. and bind it to the name `nList`.

*)
(******)


(**
### Task 2.4

With map functions, one can apply a function that can be applied to individual elements to all elements of a list.
Parameters of the `map` function:

  1. `doSomethingWithX`:  Function to be applied to each list item

  2. `list`:              List of elements to which `doSomeThingWithX` is to be applied

`List.map` is applied as follows:

  - `List.map (fun listElement -> doSomethingWithX listElement) list`

Apply the function `square` (Task 1.1) to each element of `nList`. Bind the result to the name `nListSquared`.

*)
(******)

(**
### Task 2.5

Extract all weigts from `catList` and bind it to the name `catWeights`

Clue: Search the `List` module for a suitable function.

*)
(******)


(**
### Task 2.6

Calculate the sum of the values from `nList`. 

*)
(******)

(**
### Task 2.7

Filter all list items that are divisible by 15.

Clue: Make use of `List.filter` and the given function.

*)
(******)

let isDivisibleWithoutRemainder denominator numerator =
    numerator % denominator = 0.

(**
### Task 2.8

Calculate the sum of the squares of the individual values from nList.
Try to achieve this in one step with `List.fold`.

*)
(******)

(**
### Task 2.9

Calculate the average weight of the cats from `catList`. 

Clue: The task can either be solved by using functions from the `List` module, or you can solve it by using a single fold function (for experts).

*)
(******)

