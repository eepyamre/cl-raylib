# cl-raylib
Common Lisp binding of [raylib](https://www.raylib.com/)

## run example
1. install raylib
cl-raylib require raylib dynamic library
run `brew install raylib` to install the library if run example on macOS

2. install cffi
cl-raylib use cffi to load dynamic library

quicklisp can setup cffi, or cffi in system package manager is also ok

3. fetch cl-raylib code
cl-raylib is not on quicklisp now, so you should fetch it manually

```bash
git clone https://github.com/longlene/cl-raylib.git ~/.quicklisp/local-projects/cl-raylib
```
my quicklisp install path is ~/.quicklisp

clone to ~/.local/share/common-lisp/source/cl-raylib is ok if you has no quicklisp

4. run basic example
enter sbcl repl (or ccl etc) :blush:
```lisp
(require :cl-raylib)
(load "~/.quicklisp/local-projects/cl-raylib/examples/basic.lisp")
```
Press `ESC` to close the basic window
