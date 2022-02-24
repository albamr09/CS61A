# STk 

For some reason, we are going to use yet another scheme interpreter: `STklos`. It will be mainly used in OOP (T3).

## Installation

The package is in `/resources`:

1. Unpack `stklos-${your_version}.tar.gz`
```bash
tar -xvf stklos-${your_version}.tar.gz
```
2. Access the directory `stklos-${your_version}`
```bash
cd stklos-${your_version}/
```
3. Configure the installation with `configure`
```bash
bash ./configure
```
4. Compile the interpreter
```bash
make
```
5. Install the interpreter
```bash
sudo make install
```

---

Just is case we have not tried all flavours, you can look into the original [STk](http://kaolin.unice.fr/STk/Binary/), and try to install it.

```bash
tar -xvf STk-4.0.1.tar.gz
cd Tcl
bash configure
make
cd ..
bash configure 
make
sudo make install
```

It gives me an error when compiling. There are [some other ways of installing](https://www-inst.eecs.berkeley.edu/~scheme/precompiled/Linux/), but I truly could not be bothered.


## Basic instructions

### Interactive mode

```bash
stklos
```

### Load a file (library)

```bash
stklos -l library.stk
```

### Run a program

```bash
stklos -i < program.stk
```

To load the `simply.scm` library:

```bash
stklos -l ../../../lib/simply.scm -i < E01.scm
```

The rest you can figure out with `stklos -h`
