# STklos

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

# STk

Just is case we have not tried all flavours, you can look into the original [STk](http://kaolin.unice.fr/STk/Binary/), and try to install it.

**Update**

Well, I have installed it. Inside the `resources` folders there are two files for the binaries of the STk interpreter (two different versions).

First of all, we need to convert the `.rmp` to a `.deb` file, and for that we need `alien`.
I have also saved the `.deb` file in the `resources` folder, in case you did not want to go through all the trouble.
So in ubuntu:

```bash
$ sudo apt-get install alien
```

Now we convert it in a roundabout way, because obviously (basically it is because my sistem is 64 bits, and this package was built for 32 bit systems):

```bash
$ sudo alien --scripts --to-tgz STk-4.0.1-1.i586.rpm
$ sudo alien --scripts --to-deb STk-4.0.1.tgz
```

Finally we install `STk`:

```bash
$ sudo dpkg -i stk_4.0.1-2_all.deb
```

Before we run `STk` we need to install a library (that I think migh have something to do with me not being able to compile the source code):

```bash
$ sudo apt-get install libsm6:i386
```

## Build from source

```bash
tar -xvf STk-4.0.1.tar.gz
cd Tcl
bash configure
cd ..
./configure
make
sudo make install
```

It gives me an error when compiling. Might try again with the library `libsm6:i386` installed.

## Basic instructions

### Interactive mode

```bash
$ stk
```

### Load a file (library)

```bash
$ stk -l library.stk
```

### Run a program

```bash
$ stk
STk> (load "program.stk")
```

To load the `simply.scm` library:

```bash
$ stk -l ../../../lib/simply.scm
STk> (load "program.stk")
```

## EnvDraw

- [Slib library](https://inst.eecs.berkeley.edu/~cs3s/stk/slib/)
- [EnvDraw](https://inst.eecs.berkeley.edu/~cs3s/stk/site-scheme/envdraw/)
