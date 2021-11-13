# Structure and Interpretation of Computer Programs

Repository for the labs assignments, and projects for the [`MIT` course](https://archive.org/details/ucberkeley-webcast-PL3E89002AA9B9879E?sort=titleSorter), based on the book [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html) that uses Scheme.

## Structure

https://inst.eecs.berkeley.edu/~cs61as/sp13/

## Steps

Be sure to follow the former link, where the course's structure is laid out.

1. Reading (usually book or webpage)
2. Reading exercises (if there are any)
2. Lecture
3. Labs
4. Homework

## Notes

For the labs, we follow the naming convention given by:

```
L{lab_number}_E{exercise_number}.scm
```

For the `SS` book exercises we follow the convention given by:

```
Ch{chapter_number}/E{exercise_number}.scm
```

Some course resources (labs, projects, class notes) can be accessed from:

- [**Homework exercises**](https://inst.eecs.berkeley.edu/%7Ecs61a/reader/nodate-hw.pdf)
- [**Projects**](https://people.eecs.berkeley.edu/~bh/61a-pages/Volume1/CS%2061A%20Course%20Reader,%20Volume%201.html)
- [**Lecutre Notes**](https://people.eecs.berkeley.edu/~bh/61a-pages/Volume2/notes.pdf)
- [**Labs exercises**](https://inst.eecs.berkeley.edu/%7Ecs61a/reader/nodate-labs.pdf)

### Library

You'll need some external libraries that contain some utility functions, so you need to run:

```console
$ scheme --load $WORKDIR/lib/simply.scm
```

### Overlap between SimpleScheme and Labs

Some of the labs exercices are the same as the `SS` book exercices:

#### Lab 02

| Lab excercise | `SS` excercise |
| ------------- | -------------- |
| Lab 02: 1 | Ch3: 9 |
| Lab 02: 2 | Ch5: 15-16 |
| Lab 02: 3 | Ch6: 6 |
| Lab 02: 4 | Ch6: 8 |
| Lab 02: 5 | Ch5: 19 |
| Lab 02: 6 | Ch5: 21 |
| Lab 02: 7 | Ch6: 5 |

## Instructions

How to run scheme programs locally on an machine running `Arch Linux` (install the [`Scheme Compiler (MIT/GNU)`](https://wiki.archlinux.org/title/Scheme))

###  Run the interpreter:

```console
$ scheme -interactive
```

## Run a file

```console
$ scheme < file.scm
$ scheme --quiet < file.scm
```

### Load a file and run the interpreter:

```console
$ scheme --quiet --load file.scm
```

To execute load a library and execute a file:

```console
$ scheme --load ../../lib/simply.scm < E01.scm
```
