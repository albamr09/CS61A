# Racket

If you want to use the `Racket` compiler instead of the `MIT/Scheme` one, follow the instructions.

## Install Racket

First of all, we need to install `Racket`, for that, on `Arch` we execute:

```bash
$ yay -S racket
```

If you are on another OS, refer to the [Racket Download Page](https://download.racket-lang.org/).

---

## Install necessary packages

Use `racket's` package manager `raco` to install the `berkeley` and the `sugar` (for `sublists`) package:

```bash
$ raco pkg install berkeley 
$ raco pkg install sugar 
```

## Local libraries

In some instances we will use custom libraries given by berkeley:

- `obj.rkt`: The OOP (Object Oriented Programming) library, that allows you to code using this programming style, given it is not natively implemented in scheme.
    Note that it has been modified to work correctly, the original can be found in [`obj.rkt`](https://github.com/tmaetzold/cs61as/blob/master/lib/obj.rkt)
    - Procedures modified: `get-instance-var-stuff` and `get-parent-stuff`

---

## Use the module

Now, in order to use this module we can import it by

- `(require berkeley)` or
- `#land berkeley`

---

## Import from a file

Now, in order to import a local file use

- `(require "/path/to/file")`

## Exporting

Now, in order to export procedures, compound data, etc:

- `(provide proc-1 proc-2)`

If you want to export everything that is defined on the file

- `(provide (all-defined-out))`

---

## Compile and run a racket file

Given the `test.rkt` program:

```bash
$ racket test.rkt
```
