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

```scheme
$ stk
STk> (load "program.stk")
```

To load the `simply.scm` library:

```scheme
$ stk -l ../../../lib/simply.scm
STk> (load "program.stk")
```

## EnvDraw

We use EnvDraw to draw environment diagrams, 

1. We need to install the slib library, you can do that using a package manager 

```bash
$ sudo apt-get install slib
```

or by downloading the [Slib library](https://inst.eecs.berkeley.edu/~cs3s/stk/slib/). 

```bash
$ wget -r -np -nH --cut-dirs=3 -R index.html https://inst.eecs.berkeley.edu/\~cs3s/stk/slib/
```

Note that you must place it in `/usr/lib/slib/` (see the executable `./envdraw`)

2. Now we install envdraw:
- We extract the file `envdraw.tar.xz` in the resources folder. You can also find it in [EnvDraw](https://inst.eecs.berkeley.edu/~cs3s/stk/site-scheme/envdraw/), but the file `placement.stk` has been modified (we removed the last parenthesis) because of a syntax error.
```bash 
$ tar -xvf envdraw.tar.xz
```
- We copy the file to our preferred directory (for example our home directory).

3. To start STk with envdraw, the first time we run it with sudo (permission problems on the stk lib directory: `/usr/local/lib/stk/4.0.1`). So, we head to the folder where the envdraw installation is, and then we go into `lib`. There we will find an executable (if no give execute permissions) `envdraw`, we run like so:
```bash
$ sudo ./envdraw
```

### Usage

- To see the diagram of a single object
```scheme
STk> (define l (list 1 2 3))
STk> (view l)
```
- To see a general environment diagram:
```scheme
STk> (envdraw)
```

