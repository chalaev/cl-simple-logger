
# Table of Contents

1.  [The scope](#org96a2669)
2.  [Prerequisites](#orgd60ecfd)
3.  [Quick start](#org1d20a4d)
    1.  [Create log directory](#org85daef1)
    2.  [Either use quick-start.lisp](#orgb64ed85)
    3.  [or update Makefile, then make](#org8d9f280)
    4.  [Notes](#orgbf994e0)
4.  [Source code files](#org3949264)
5.  [License](#org562200c)
6.  [Support](#orgb6c05ce)

Simple logging system for Common Lisp;
currently works only on [sbcl](http://www.sbcl.org/), see [other-CLs.org](other-CLs.md).


<a id="org96a2669"></a>

# The scope

1.  The logging system must be as simple and robust as a hammer.
2.  It must support multi-threading and (sometimes simultaneously) receive messages from different lisp programs (e.g., services on a web server).
3.  It should be simpler and shorter than any of the projects that would need a logging system.
4.  One should never suspect that bugs in his/her projects arise from the logging system.


<a id="orgd60ecfd"></a>

# Prerequisites

It is assumed that [quicklisp](https://www.quicklisp.org/beta/) is installed. As usually with `lisp` projects, [emacs](https://www.gnu.org/software/emacs/) helps.


<a id="org1d20a4d"></a>

# Quick start

Configure the log system before launching the log:


<a id="org85daef1"></a>

## Create log directory

Under root privileges:

    adduser shalaev staff
    mkdir /var/log/sbcl
    chgrp staff /var/log/sbcl
    chmod 770 /var/log/sbcl

where `shalaev` should be replaced with your user name.


<a id="orgb64ed85"></a>

## Either use [quick-start.lisp](quick-start.lisp)

If you do not want to go into the details,
just copy the source code files from the [generated/](generated/) directory to some place where `(ql:quickload :simple-log)` command would find them,
for example, `~/quicklisp/local-projects/simple-log/`

Then open and run `quick-start.lisp` to see how the log system works.


<a id="org8d9f280"></a>

## or update [Makefile](Makefile), then make

If the code was changed, `make` will be useful for testing and compiling it.

In [Makefile](Makefile),

1.  set [SBCL](//WWW.SBCL.ORG/) to the path of the [sbcl](http://www.sbcl.org/) binary, and
2.  set `quicklispDir` to the directory where locally created `server-log` package will be stored; then
3.  `make`

This `make` command

-   compiles the test to the small (13mb) binary and copies it to the current directory, and
-   tests the code by running it in interpreting mode.

The compiled binary can be found in the  [generated/](generated/) directory.


<a id="orgbf994e0"></a>

## Notes

1.  [Makefile](Makefile) requires [emacs](https://www.gnu.org/software/emacs/) for generating the source code files in the  [generated/](generated/) directory from [simple-log.org](simple-log.md) with the `M-x org-babel-tangle` command.
2.  Unless you have manually compiled [sbcl](http://www.sbcl.org/) with `sh make.sh --with-sb-core-compression` command,
    your binary will be several times larger than 13mb.


<a id="org3949264"></a>

# Source code files

1.  [README.org](README.md) generates `README.md` for [notabug](https://notabug.org/shalaev/cl-simple-logger) and [github](https://github.com/chalaev/cl-simple-logger).
2.  [simple-log.org](simple-log.md) contains the code from `generated/*` together with explanations.
3.  [macros.lisp](goodies/macros.lisp) is copied from the [elisp-goodies](https://notabug.org/shalaev/elisp-goodies) project.
4.  [Makefile](Makefile) is needed to test the changes made to the code; it
    1.  uses `emacs` to generate the source code from the `.org` file,
    2.  launches [generated/example.lisp](generated/example.lisp) in interpreting mode, and
    3.  compiles [generated/example.lisp](generated/example.lisp) into `generated/example.bin`.
5.  [other-CLs.org](other-CLs.md) is about making the code compatible with other CL dialects. (This does not seem to be hard.)


<a id="org562200c"></a>

# License

This code is released under [MIT license](https://mit-license.org/).


<a id="orgb6c05ce"></a>

# Support

You can support this project by sending

1.  comments/questions to [oleg@chalaev.com](mailto:oleg@chalaev.com) and
2.  donations via [liberapay](https://liberapay.com/shalaev/donate) or [paypal](https://www.paypal.com/paypalme/chalaev).

