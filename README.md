
# Table of Contents

1.  [The scope](#orgb037d56)
2.  [Prerequisites](#org6a57263)
3.  [Quick start](#orgbe51d84)
    1.  [Create log directory](#orgeb5747d)
    2.  [Either just use the provided code](#orgcee2d88)
    3.  [or update Makefile, then make](#orgd4d6f10)
    4.  [Notes](#org72680d2)
4.  [Source code files](#orga228c6b)
5.  [License](#org8544159)
6.  [Support](#orgda28c07)

Simple logging system for Common Lisp.


<a id="orgb037d56"></a>

# The scope

1.  The logging system must be as simple and robust as a hammer.
2.  It must support multi-threading and (sometimes simultaneously) receive messages from different lisp programs (e.g., services on a web server).
3.  It should be simpler and shorter than any of the projects that would need a logging system.
4.  One should never suspect that bugs in his/her projects arise from the logging system.


<a id="org6a57263"></a>

# Prerequisites

Apart from `sbcl`, it is assumed that [quicklisp](https://www.quicklisp.org/beta/) is installed. As usually with `lisp` projects, [emacs](https://www.gnu.org/software/emacs/) helps.


<a id="orgbe51d84"></a>

# Quick start

Configure the log system before launching the log:


<a id="orgeb5747d"></a>

## Create log directory

Under root privileges:

    adduser shalaev staff
    mkdir /var/log/sbcl
    chgrp staff /var/log/sbcl
    chmod 770 /var/log/sbcl

where `shalaev` should be replaced with your user name.


<a id="orgcee2d88"></a>

## Either just use the provided code

If you do not want to go into the details,
just copy the source code files from `generated/` to the directory where `(ql:quickload :simple-log)` command would find them,
for example, `~/quicklisp/local-projects/simple-log/`

Then just open and run `quick-start.lisp` to see how the log system works.


<a id="orgd4d6f10"></a>

## or update [Makefile](Makefile), then make

If you changed the code, `make` will be useful for testing and compiling it.

In [Makefile](Makefile),

1.  set `SBCL` to the path of the `sbcl` binary, and
2.  set `quicklispDir` to the directory where locally created `server-log` package will be stored; then
3.  `make`

The `make` command

-   compiles the test to the small (13mb) binary and copies it to the current directory, and
-   tests the code by running it in interpreter's mode.

You can run the compiled binary from the `generated/` directory.


<a id="org72680d2"></a>

## Notes

1.  [Makefile](Makefile) requires [emacs](https://www.gnu.org/software/emacs/) for generating the source code files in the `generated/` directory from [simple-log.org](simple-log.md) with the `M-x org-babel-tangle` command.
2.  Unless you have manually compiled `sbcl` with `sh make.sh --with-sb-core-compression` command,
    your binary will be several times larger than 13mb.


<a id="orga228c6b"></a>

# Source code files

1.  [README.org](README.md) generates `README.md` for [notabug](https://notabug.org/shalaev/cl-simple-logger) and [github](https://github.com/chalaev/cl-simple-logger).
2.  [simple-log.org](simple-log.md) contains the code from `generated/*` together with explanations.
3.  [macros.lisp](goodies/macros.lisp) is copied from the [elisp-goodies](https://notabug.org/shalaev/elisp-goodies) project.
4.  [Makefile](Makefile) is needed to test the changes made to the code; it
    1.  uses `emacs` to generate the source code from the `.org` file,
    2.  launches `generated/example.lisp` in interpreting mode, and
    3.  compiles `generated/example.lisp` into `generated/example.bin`.


<a id="org8544159"></a>

# License

This code is released under [MIT license](https://mit-license.org/).


<a id="orgda28c07"></a>

# Support

You can support this project by sending

1.  comments/questions to [oleg@chalaev.com](mailto:oleg@chalaev.com) and
2.  donations via [liberapay](https://liberapay.com/shalaev/donate) or [paypal](https://www.paypal.com/paypalme/chalaev).

