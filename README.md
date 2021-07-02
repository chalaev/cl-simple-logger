
# Table of Contents

1.  [The scope](#orgd981c6a)
2.  [Prerequisites](#orgc73bb77)
3.  [Quick start](#orgcf9efec)
    1.  [Create log directory](#org04fce78)
    2.  [Install the package](#orgbfe5bf4)
    3.  [Test in an SBCL session](#orgefc5da1)
4.  [Packaging, compiling and testing](#org1e10d4d)
5.  [Files](#org1ba888e)
6.  [License](#org475ef90)

Simple logging system for Common Lisp; currently works only on [sbcl](http://www.sbcl.org/), see [other-CLs.org](other-CLs.org).


<a id="orgd981c6a"></a>

# The scope

1.  The logging system must be as simple and robust as a hammer.
2.  It must support multi-threading and (sometimes simultaneously) receive messages from different lisp programs (e.g., services on a web server).
3.  It should be simpler and shorter than any of the projects that would need a logging system.
4.  One should never suspect that bugs in his/her projects arise from the logging system.

Examples of other CL log systems: [log4cl](https://github.com/7max/log4cl), [cl-log](https://github.com/nicklevine/cl-log).


<a id="orgc73bb77"></a>

# Prerequisites

It is assumed that [quicklisp](https://www.quicklisp.org/beta/) is installed; [emacs](https://www.gnu.org/software/emacs/) would help although it is not compulsory.


<a id="orgcf9efec"></a>

# Quick start

Configure the log system before launching the log:


<a id="org04fce78"></a>

## Create log directory

Under root privileges:

    adduser shalaev staff
    mkdir /var/log/sbcl
    chgrp staff /var/log/sbcl
    chmod 770 /var/log/sbcl

where `shalaev` should be replaced with your user name.


<a id="orgbfe5bf4"></a>

## Install the package

Unpack [simple-log.tbz](packaged/simple-log.tbz) into your [Quicklisp](https://www.quicklisp.org/beta/) directory:  
`tar xjfv simple-log.tbz --directory=$HOME/quicklisp/local-projects/`


<a id="orgefc5da1"></a>

## Test in an SBCL session

Open [quick-start.lisp](quick-start.lisp) and evaluate it in an interactive SBCL session.


<a id="org1e10d4d"></a>

# Packaging, compiling and testing

(Requires [emacs](https://www.gnu.org/software/emacs/).)

In [Makefile](Makefile),

1.  set `SBCL` to the path of the [sbcl](http://www.sbcl.org/) binary, and
2.  set `quicklispDir` to the directory where locally created `server-log` package will be stored; then
3.  `make`

The `make` command

-   compiles the test to the small (13mb) binary and copies it to the current directory, and
-   tests the code by running it in interpreting mode.

The compiled binary can be found in the [generated/](generated/) directory.

Notes:

1.  [Makefile](Makefile) requires
    1.  [emacs](https://www.gnu.org/software/emacs/) for generating the source code files in the  [generated/](generated/) directory from [simple-log.org](simple-log.org) with the `M-x org-babel-tangle` command, and
    2.  `printangle` function defined in [lisp-goodies/dot.emacs](https://github.com/chalaev/lisp-goodies/blob/master/generated/dot.emacs) file.
2.  Unless you have manually compiled [sbcl](http://www.sbcl.org/) with `sh make.sh --with-sb-core-compression` command,
    the resulting binary will be several times larger than 13mb.


<a id="org1ba888e"></a>

# Files

1.  [README.org](README.org) generates `README.md` for [notabug](https://notabug.org/shalaev/cl-simple-logger) and [github](https://github.com/chalaev/cl-simple-logger).
2.  [simple-log.org](simple-log.org) contains the code from `generated/*` together with explanations.
3.  [Makefile](Makefile) is needed to test the changes made to the code; it
    1.  uses `emacs` to generate the source code from the `.org` file,
    2.  launches [example.lisp](generated/example.lisp) in interpreting mode, and
    3.  compiles [example.lisp](generated/example.lisp) into `generated/example.bin`.
    4.  creates [simple-log.tbz](packaged/simple-log.tbz)
4.  [other-CLs.org](other-CLs.org) contains a note about making the code compatible with other CL dialects. (This does not seem to be hard.)
5.  [helpers/\*](helpers/) assist compilation.


<a id="org475ef90"></a>

# License

This code is released under [MIT license](https://mit-license.org/).

