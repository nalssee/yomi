(in-package :yomi)

(defparameter *yomi-server-port* 8888)
(defparameter *web-socket-port* 9999)

(defvar *ws-loc* "/ws_yomi")

(defparameter *server-running-p* nil)
   
(defparameter *working-directory* *default-pathname-defaults*)

;; All connected clients are registered here,
;; with eventloops and some others related.
(defvar *workplaces* nil)

;; one of "emacs" "vim" "sublime"
(defparameter *keymap* "emacs")

(defparameter *yomi-path-default*
  (asdf:system-source-directory 'yomi))

(defparameter *default-working-package* "YNB")
;; 
(defparameter *js-css-files*
  '(
    "jquery/dist/jquery.min.js"
    "d3/d3.min.js"
    "c3/c3.css"
    "c3/c3.min.js"
    
    "CodeMirror/lib/codemirror.js"
    "CodeMirror/lib/codemirror.css"
    "CodeMirror/keymap/emacs.js"
    "CodeMirror/keymap/vim.js"
    "CodeMirror/keymap/sublime.js"
    "CodeMirror/keymap/sublime.js"
    
    "CodeMirror/mode/commonlisp/commonlisp.js"
    "CodeMirror/addon/edit/matchbrackets.js"
    
    "basic_style.css"
    ))

(defparameter *image-files*
  '("images/symbolize-icons-set/png/128x128/play.png"
    "images/symbolize-icons-set/png/128x128/arrow_up.png"
    "images/symbolize-icons-set/png/128x128/arrow_down.png"
    "images/symbolize-icons-set/png/128x128/stop.png"
    "images/symbolize-icons-set/png/128x128/cut.png"
    "images/symbolize-icons-set/png/128x128/save.png"
    "images/symbolize-icons-set/png/128x128/notebook.png"
    "images/symbolize-icons-set/png/128x128/folder.png"
    "images/symbolize-icons-set/png/128x128/fast_forward.png"
    "images/symbolize-icons-set/png/128x128/add.png"
    "images/symbolize-icons-set/png/128x128/undo.png"))

