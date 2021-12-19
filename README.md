# Structure and Interpretation of Computer Programs

Repository for the labs assignments, and projects for the [`MIT` course](https://archive.org/details/ucberkeley-webcast-PL3E89002AA9B9879E?sort=titleSorter), based on the book [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html) that uses Scheme.

## Projects

- [Twenty One](./Projects/01)

## Notes

For the labs, we follow the naming convention given by:

```
./Labs/T{unit_number}/L{lab_number}/E{exercise_number}.scm
```
---

For the `SS` book exercises we follow the convention given by:

```
./SimpleScheme/Ch{chapter_number}/E{exercise_number}.scm
```
---

For problems layed out on the book, the following convention is used:

```
./BProblems/T{unit_number}/P{section_number}_{subsection_number}.scm
```
---

For projects, the following convention is used:

```
./Projects/{unit_number}/
```

Run the tests by executing:

```bash
$ scheme --load ../../../lib/simply.scm < tests.scm
```

And some discarded procedures are in:

```
./Projects/{unit_number}/._mytests
```
---

## Instructions

### Library

You'll need some external libraries that contain some utility functions, so you need to run:

```console
$ scheme --load $WORKDIR/lib/simply.scm
```

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
