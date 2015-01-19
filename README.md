## YOMI
Common Lisp version of iPython notebook

## Tested
OS: MACOS X (Yosemite), Linux (Ubuntu 14.04)

CL implementation: SBCL, CCL (must support multi-threads)

Web browsers: Recent versions of Google Chrome, Firefox and Safari, must support web socket


## Install Quick start
cd ~/quicklisp/local-projects

git clone https://github.com/nalssee/yomi.git


After starting sbcl/ccl

\(ql:quickload :yomi\)

\(yomi:start-yomi\)

When a web browser opens a notebook page, type in (demo) in a textarea for help.


## Keyboard Shortcut
Ctrl-Return : Evaluate focused cell

Ctrl-s : Save notebook

## Utils
change directory : (cd "~/Desktop/notebooks")

change keymap : (keymap "vim") or (keymap "sublime"), default is "emacs"


![Alt text](/images/ss1.png?raw=true "Screen Shot")



