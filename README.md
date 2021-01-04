
# Table of Contents

1.  [The scope](#org75bd043)
2.  [Prerequisites](#orga4e6b04)
3.  [Quick start](#orgf7adbb9)
    1.  [Create log directory](#orga927079)
    2.  [Install the package](#org6361989)
    3.  [Test in an SBCL session](#org3ca5752)
4.  [Packaging, compiling and testing](#org42bb853)
5.  [Files](#org84dabaa)
6.  [License](#org16ed7ab)
7.  [Support](#orgb55cad3)

Simple logging system for Common Lisp; currently works only on [sbcl](http://www.sbcl.org/), see [other-CLs.org](other-CLs.org).


<a id="org75bd043"></a>

# The scope

1.  The logging system must be as simple and robust as a hammer.
2.  It must support multi-threading and (sometimes simultaneously) receive messages from different lisp programs (e.g., services on a web server).
3.  It should be simpler and shorter than any of the projects that would need a logging system.
4.  One should never suspect that bugs in his/her projects arise from the logging system.

Examples of other CL log systems: [log4cl](https://github.com/7max/log4cl), [cl-log](https://github.com/nicklevine/cl-log).


<a id="orga4e6b04"></a>

# Prerequisites

It is assumed that [quicklisp](https://www.quicklisp.org/beta/) is installed; [emacs](https://www.gnu.org/software/emacs/) would help although it is not compulsory.


<a id="orgf7adbb9"></a>

# Quick start

Configure the log system before launching the log:


<a id="orga927079"></a>

## Create log directory

Under root privileges:

    adduser shalaev staff
    mkdir /var/log/sbcl
    chgrp staff /var/log/sbcl
    chmod 770 /var/log/sbcl

where `shalaev` should be replaced with your user name.


<a id="org6361989"></a>

## Install the package

Unpack [simple-log.tbz](packaged/simple-log.tbz) into your [Quicklisp](https://www.quicklisp.org/beta/) directory:  
`tar xjfv simple-log.tbz --directory=$HOME/quicklisp/local-projects/`


<a id="org3ca5752"></a>

## Test in an SBCL session

Open [quick-start.lisp](quick-start.lisp) and evaluate it in an interactive SBCL session.


<a id="org42bb853"></a>

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


<a id="org84dabaa"></a>

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


<a id="org16ed7ab"></a>

# License

This code is released under [MIT license](https://mit-license.org/).


<a id="orgb55cad3"></a>

# Support

You can support this project by sending

1.  comments/questions to [oleg@chalaev.com](mailto:oleg@chalaev.com) and
2.  donations via [liberapay](https://liberapay.com/shalaev/donate) or [paypal](https://www.paypal.com/paypalme/chalaev).

