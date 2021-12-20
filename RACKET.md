# Racket

If you want to use the `Racket` compiler instead of the `MIT/Scheme` one, follow the instructions.

## Install Racket

First of all, we need to install `Racket`, for that, on `Arch` we execute:

```bash
$ yay -S racket
```

If you are on another OS, refer to the [Racket Download Page](https://download.racket-lang.org/).

## Install `simply-scheme` package

Use `racket's` package manager `raco` to install the `simply-scheme` package:

```bash
$ raco pkg install simply-scheme 
```

## Use the module

Now, in order to use this module we can import it by

- `(require simply-scheme)` or
- `#land simply-scheme`

## Compile and run a racket file

Given the `test.rkt` program:

```bash
$ racket test.rkt
```
