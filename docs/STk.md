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

## Docker

I have created a Dockerfile to install `stk` on a docker container running ubuntu. Thus, you will need to have docker installed. Once you have it installed, simply run:

```bash
$ ./runDocker
```

This will lead into the container's terminal:

```bash
root@85594f04062e:/#
```

If you type `stk` inside the terminal, you will enter the `stk` interpreter. Inside this container all my source files will be installed.

Note that each time you run `./runDocker`, you are creating a new image. To remove them all execute:

```bash
./removeImages.sh
```

**Update**

Well, I have installed it. Inside the `resources` folder, there is the `STk` folder, where the `.deb` package is (this was converted from `.rpm` to `.deb` with alien)
The `.rpm` file is in [STk Berkeley](http://inst.eecs.berkeley.edu/~scheme/precompiled/Linux/STk-4.0.1-ucb1.3.6.i386.rpm)

First of all, we need to convert the `.rmp` to a `.deb` file, and for that we need `alien`.
So in ubuntu:

```bash
$ sudo apt-get install alien
```

Now we convert it in a roundabout way, because obviously (basically it is because my sistem is 64 bits, and this package was built for 32 bit systems):

```bash
$ sudo alien --scripts --to-tgz STk-4.0.1-ucb1.3.6.i386.rpm
$ sudo alien --scripts --to-deb STk-4.0.1-ucb1.3.6.i386.tgz
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

1. We install envdraw:
- We extract the file `envdraw.tar.xz` in the resources folder. You can also find it in [EnvDraw](https://inst.eecs.berkeley.edu/~cs3s/stk/site-scheme/envdraw/), but
    - the file `placement.stk` has been modified (we removed the last parenthesis) because of a syntax error.
    - the executable `envdraw` has also been modified to use my libraries (it is advisable to check this executable, to see if all the environment variables are correct)

```bash 
$ tar -xvf envdraw.tar.xz
```

2. To start STk with envdraw, the first time we run it with sudo (permission problems on the stk lib directory: `/usr/local/lib/stk/`). So, we head to the folder where the envdraw installation is, and then we go into `lib`. There we will find an executable (if no give execute permissions) `envdraw`, we run like so:

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

![EnvDraw](/docs/img/envdraw.png)

```scheme
STk> (define (f n) n)
```

![Define](/docs/img/define.png)

