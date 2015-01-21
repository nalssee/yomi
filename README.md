## YOMI
Common Lisp version of iPython notebook

![Alt text](/images/ss1.png?raw=true "Screen Shot")

## Tested
OS: MACOS X (Yosemite), Linux (Ubuntu 14.04)

CL: SBCL, CCL (must support threads)

Web browsers: Recent ones (must support web socket)

## Dependencies
cl-who, hunchentoot, clws, parenscript, cl-json, inferior-shell, cl-fad, bordeaux-threads, usocket


## Installation && Quick start
cd ~/quicklisp/local-projects

git clone https://github.com/nalssee/yomi.git


After starting sbcl/ccl

\(ql:quickload :yomi\)

\(yomi:start-yomi\)

When a web browser opens a notebook page, type in (demo) in a textarea for help.


## Keyboard Shortcut
Ctrl-Enter : Evaluate the focused cell

Ctrl-s : Save notebook

## Utils
change directory : (cd "~/Desktop/notebooks") default: *default-pathname-defaults*

change keymap : (keymap "vim") or (keymap "sublime") , default: "emacs"




