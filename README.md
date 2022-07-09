# Structure and Interpretation of Computer Programs

Repository for the labs assignments, and projects for the [Computer Science 61A (CS61A) Course of Berkeley](https://archive.org/details/ucberkeley-webcast-PL3E89002AA9B9879E?sort=titleSorter), based on the book [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html) that uses Scheme.

## Set up

- Using the [`Scheme Compiler (MIT/GNU)`](./docs/MIT.md)
- Using the [`Racket`](./docs/RACKET.md)
- Using the [`STklos`](./docs/STk.md)

## Projects

- [Twenty One](./Projects/01)
- [Picture Language](./Projects/02)
- [Adventure Game](./Projects/03)
- [Python Interpreter](./Projects/04/schyton)

## Notes

### Compiler

First of all, at the start of the course the [`Scheme Compiler (MIT/GNU)`](http://www.gnu.org/software/mit-scheme/) was used. So in some sections there are two folders:

- `MIT`: where the programs where made using the `MIT/GNU` compiler.
- `Racket`: where the `Racket` compiler was used.


For the labs, we follow the naming convention given by:

```
./Labs/T{unit_number}/L{lab_number}/E{exercise_number}.scm
```

## Libs

- For scheme libraries refer to [`Scheme Compiler (MIT/GNU)`](./docs/MIT.md)
- For racket libraries refer to [`Racket Compiler`](./docs/RACKET.md)

---

### Structure

#### Simple-Scheme

For the `SS` book exercises we follow the convention given by:

```
./SimpleScheme/Ch{chapter_number}/E{exercise_number}.scm
```
---

#### Book/Note Problems

For problems laid out on the book, the following convention is used:

```
./BookProblems/T{unit_number}/P{section_number}_{subsection_number}.scm
```

Also inside the books problems, there will be some programs written on the notes, that follow the following format:


```
./BookProblems/T{unit_number}/Notes/W{week_number}.scm
```

#### Custom Interpreters

Throughout the course we will create an interpreter, that will be built little by little. So we'll manage several versions:

1. `./BookProblems/T2/Notes/racket1.rkt` (mind you, this is made to work with racket, not the MIT compiler)
  - The tests are in `./BookProblems/T2/Notes/W05_7.rkt`. Execute `racket W05_7.rkt` to run the interpreter.
2. In `./Labs/T2/L06/E02` and `./Labs/T2/L06/E03`, there are two different versions of the interpreter, each of which add new evaluation rules. 
  - `./Labs/T2/L06/E02`: implements a rule to evaluate an `and` expression
  - `./Labs/T2/L06/E03`: implements a rule to evaluate a `map` expression
However the interpreter with the most functionality is in `./Labs/T2/L06/E03`.

---

#### Projects

For projects, the following convention is used:

```
./Projects/{unit_number}/
```

And some discarded procedures are in:

```
./Projects/{unit_number}/._mytests
```
---
