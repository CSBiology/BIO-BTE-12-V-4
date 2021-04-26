(**
---
title: Übung 02
category: Übungen(deutsch)
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

Deklariere einen Tuple aus 2. und "Februar" und binde den Tuple an den Namen `tuple1`.
*)

(*******)

(**
### Task 1.2:

Deklariere einen Tuple aus 2., "Februar" und "Fruehling" und bindet ihn an den Namen `tuple2`.
*)

(*******)

(**
### Task 1.3:

Greife auf den zweiten Wert des Tuples aus 1.1 zu.
*)

(*******)

(**
### Task 1.4:

Bindet `tuple1` aus Task 1.1 an: `(output1,output2)`
Warum kann das sinnvoll sein?
Stichwort: Tuple Deconstruction.
*)

(*******)

(**
## Record Types

### Task 2.1: 

Definiere einen Record Type mit dem Namen `Person`. Person soll die Felder `Name`(string) und `Alter`(int) haben.
Bei Record Types wird kein camel case, sondern pascal case verwendet (wie camel case nur ist der erste Buchstabe auch gro�).
*)

(*******)

(**
### Task 2.2:

Erstellt einen Wert des Typs `Person` mit ausgedachten Feldwerten und bindet ihn an den Namen `meinePerson1`.
Greift auf das Feld `Alter` von `meinePerson1` zu.
*)

(*******)

(**
### Task 2.3:

Deklariere eine Funktion die zwei Inputparameter erhaelt und als Rueckgabewert eine `Person` gibt.
Binde ein Ergebnis dieser Funktion an den Namen `meinePerson2`.
*)

(*******)

(**
### Task 2.4:

Definiere einen weiteren Record Type mit dem Namen `ErweitertePerson`. ErweitertePerson soll die gleichen Felder wie `Person`
haben, bekommt aber noch das zusaetzliche Feld `Hobby`. Da man alles zu seinem Hobby machen kann, soll
'Hobby' vom Typ generic type sein.
Tipp: Zwischen dem Namen des Record types und `=` muss der generic type durch `<'a>` angegeben werden
*)

(*******)

(**
### Task 2.5

Erstellt einen Wert des Typs `ErweitertePerson`.
*)

(*******)

(**
## Signatures

### Task 3.1

Deklariere eine beliebige Funktion mit der Signatur `int -> int`.
*)

(*******)

(**
### Task 3.2

Deklariere eine beliebige Funktion mit der Signatur `int -> (int -> int) -> float`.
*)

(*******)

(**
### Task 3.3

Beschreibe den Rueckgabewert einer Funktion mit folgender Signatur `char -> (string -> char -> float)`.
*)

(*******)

(**
## Collection types

### Task 4.1

Erstelle eine int Liste mit den Zahlen 1 bis 100 und binde sie an einen Namen.
*)

(*******)

(**
### Task 4.2

Binde das erste und das letzte Element der Liste an je einen Namen.
*)

(*******)

(**
### Task 4.3

Erstelle ein float Array mit den Zahlen von 1. bis 100. und binde es an einen Namen.
*)

(*******)

(**
### Task 4.4

Binde das 15. Element des Arrays aus 4.3 an einen Namen.
*)

(*******)

(**
### Task 4.5

Erstelle eine Map, die alle Monate enthaelt. Verwende dafuer die Zahl des Monats (integer) als Key und den Namen (string) als Value.
*)

(*******)

(**
### Task 4.6

Ueberpruefe anhand eines geeigneten Keys, ob der Monat Maerz in der Map vorhanden ist.
*)

(*******)

(**
### Task 4.7

Erstelle zwei Sets. Eins mit Zahlen von 1 bis 10 und eins mit Zahlen von 3 bis 7 und binde sie an je einen Namen.
*)

(*******)

(**
### Task 4.8 

Vergleicht die zuvor erstellten Sets. Bildet die Schnittmenge (intersect) und die Vereinigungsmenge (union) aus beiden Sets und bindet sie an je einen Namen.
Tipp: Nutzt das Set Modul um durch 'reindotten' geeignete Funktionen zu erhalten.
*)

(*******)