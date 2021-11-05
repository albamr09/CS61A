# Structure and Interpretation of Computer Programs

Repository for the labs assignments, and projects for the [`MIT` course](https://archive.org/details/ucberkeley-webcast-PL3E89002AA9B9879E?sort=titleSorter), based on the book [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html) that uses Scheme.

## Structure

https://inst.eecs.berkeley.edu/~cs61as/sp13/

## Notes

For the labs, we follow the naming convention given by:

```
W{week_number}_L{lab_number}_E{exercise_number}.scm
```

Some course resources (labs, projects, class notes) can be accessed from:

- [**Homework**](https://inst.eecs.berkeley.edu/%7Ecs61a/reader/nodate-hw.pdf)
- [**Projects**](https://people.eecs.berkeley.edu/~bh/61a-pages/Volume1/CS%2061A%20Course%20Reader,%20Volume%201.html)
- [**Notes**](https://people.eecs.berkeley.edu/~bh/61a-pages/Volume2/notes.pdf)
- [**Labs**](https://inst.eecs.berkeley.edu/%7Ecs61a/reader/nodate-labs.pdf)

### Library

You'll need some external libraries that contain some utility functions, so you need to run:

```console
$ scheme --load $WORKDIR/lib/simply.scm
```

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
