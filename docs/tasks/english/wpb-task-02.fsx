(**
---
title: Task 02
category: Tasks(english)
categoryindex: 2
index: 3
---
*)

(**

[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/csbiology/BIO-BTE-12-V-4/gh-pages?filepath={{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

# Task02 - Functions and Data types

## Tuples

### Task 1.1:

Declare a tuple consisting of 2. and "February" and bind it to the name `tuple1`.
*)

(******)

(**
### Task 1.2:

Declare a tuple consisting of 2., "February" and "Spring" and bind it to the name `tuple2`.
*)

(******)

(**
### Task 1.3:

Access the second value of the tuple of 1.1.
*)

(******)

(**
### Task 1.4:

Bind `tuple1` of 1.1 to: `(output1,output2)`.
Why could that be useful?
Keyword: tuple deconstruction
*)

(******)

(**
## Record Types

### Task 2.1: 

Define a record type with the name `Person`. Person should contain the fields `Name`(string) and `Age`(int).
Pascal case is used for record types instead of camel case (pascal case is like camel case but the first letter is big too).
*)

(******)

(**
### Task 2.2:

Create a value of the type `Person` with made-up values in its fields and bind it to the name `myPerson1`.
Access the field `Age` of `myPerson1`.
*)

(******)

(**
### Task 2.3:

Declare a function that takes two input parameters und returns a `Person`.
Bind the result of this function to the name `myPerson2`.
*)

(******)

(**
### Task 2.4:

Define another record type with the name `ExtendedPerson`. ExtendedPerson shall have the same fields like `Person` but get the
additional field `Hobby`. Since everything can be your hobby, `Hobby` shall be of generic type.
Hint: The generic type must be stated between the name of the record type and `=` via `<'a>`.
*)

(******)

(**
### Task 2.5

Create a value of the type `ExtendedPerson`.
*)

(******)

(**
## Signatures

### Task 3.1

Declare an arbitrary function with the signature `inct -> int`.
*)

(******)

(**
### Task 3.2

Declare an arbitrary function with the signature `int -> (int -> int) -> float`.
*)

(******)

(**
### Task 3.3

Describe the return value of a function with the signature `char -> (string -> char -> float)`.
*)

(******)

(**
## Collection types

### Task 4.1

Create an int list with the numbers 1 to 100 and bind them to a name.
*)

(******)

(**
### Task 4.2

Bind the first and the last element of the list each to a name.
*)

(******)

(**
### Task 4.3

Create a float array with the numbers 1. to 100. and bind them to a name.
*)

(******)

(**
### Task 4.4

Bind the 15. element of the array of 4.3 to a name.
*)

(******)

(**
### Task 4.5

Create a map that contains all months. Use the number of the month (integer) as key and the name (string) as value.
*)

(******)

(**
### Task 4.6

Check, based on a suitable key, if the month March exists in the map.
*)

(******)

(**
### Task 4.7

Create two sets. One with numbers 1 to 10 and another one with numbers 3 to 7 and bind them each to a name.
*)

(******)

(**
### Task 4.8 

Compare the previously created sets. Form the intersection (instersect) and the union of both sets and bind them each to a name.
Hint: Use the module Set to get suitable functions via 'dotting in'.
*)

(******)