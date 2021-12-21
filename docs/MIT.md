# MIT

Follow this instructions to use the [`Scheme Compiler (MIT/GNU)`](https://wiki.archlinux.org/title/Scheme).

## Install

To run scheme programs locally on an machine running `Arch Linux`, install the [`Scheme Compiler (MIT/GNU)`](http://www.gnu.org/software/mit-scheme/) accessible on the [`AUR`](https://wiki.archlinux.org/title/Scheme).

If you have a package manager like `yay`, simply run:

```bash
$ yay -S mit-scheme
```


## Instructions

### Library

You'll need some external libraries that contain some utility functions, so you need to run:

```console
$ scheme --load $WORKDIR/lib/simply.scm
```

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
$ scheme --load $WORKDIR/lib/simply.scm < E01.scm
```

## Run the tests on the projects

Run the tests by executing:

```bash
$ scheme --load $WORKDIR/lib/simply.scm < tests.scm
```