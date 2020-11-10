
# Table of Contents

1.  [The scope](#org2c60815)
2.  [Prerequisites](#orgad09e85)
3.  [Quick start](#orgd2a3b72)
    1.  [Create log directory](#org19fc50d)
    2.  [either just use the provided code](#org59edd76)
    3.  [or update Makefile, then make](#org1dda562)
    4.  [Notes](#orge5ffa66)
4.  [License](#org1161bf1)
5.  [Support](#orgf971ef9)

Simple logging system for Common Lisp.


<a id="org2c60815"></a>

# The scope

1.  The logging system must be as simple and robust as a hammer.
2.  It must support multi-threading and (sometimes simultaneously) receive messages from different lisp programs (e.g., services on a web server).
3.  It should be simpler and shorter than any of the projects that would need a logging system.
4.  One should never suspect that bugs in his/her projects arise from the logging system.


<a id="orgad09e85"></a>

# Prerequisites

Apart from `sbcl`, it is assumed that [quicklisp](https://www.quicklisp.org/beta/) is installed. As usually with `lisp` projects, [emacs](https://www.gnu.org/software/emacs/) helps.


<a id="orgd2a3b72"></a>

# Quick start

Configure the log system before launching the log:


<a id="org19fc50d"></a>

## Create log directory

Under root privileges:

    adduser shalaev staff
    mkdir /var/log/sbcl
    chgrp staff /var/log/sbcl
    chmod 770 /var/log/sbcl

where `shalaev` should be replaced with your user name.


<a id="org59edd76"></a>

## either just use the provided code

If you do not want to go into the details,
just copy the source code files from `generated/` to the directory where `(ql:quickload :simple-log)` command would find them,
for example, `~/quicklisp/local-projects/simple-log/`

Then just open and run `quick-start.lisp` to see how the log system works.


<a id="org1dda562"></a>

## or update Makefile, then make

If you changed the code, `make` will be useful for testing and compiling it.

In `Makefile`,

1.  set `SBCL` to the path of the `sbcl` binary, and
2.  set `quicklispDir` to the directory where locally created `server-log` package will be stored; then
3.  `make`

The `make` command

-   compiles the test to the small (13mb) binary and copies it to the current directory, and
-   tests the code by running it in interpreter's mode.

You can run the compiled binary from the `generated/` directory.


<a id="orge5ffa66"></a>

## Notes

1.  `Makefile` requires [emacs](https://www.gnu.org/software/emacs/) for generating the source code files in the `generated/` directory from `simple-log.org` with the `M-x org-babel-tangle` command.
2.  Unless you have manually compiled `sbcl` with `sh make.sh --with-sb-core-compression` command,
    your binary will be several times larger than 13mb.


<a id="org1161bf1"></a>

# License

This code is released under [MIT license](https://mit-license.org/).


<a id="orgf971ef9"></a>

# Support

You can support this project by sending

1.  comments/questions to [oleg@chalaev.com](mailto:oleg@chalaev.com) and
2.  donations via [liberapay](https://liberapay.com/shalaev/donate) or [paypal](https://www.paypal.com/paypalme/chalaev).

