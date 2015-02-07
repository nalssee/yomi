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
    "flot/jquery.js"
    "flot/jquery.flot.js"
    "flot/jquery.flot.symbol.js"
    "flot-axislabels/jquery.flot.axislabels.js"
    "flot/excanvas.js"
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


(defparameter *color-list*
  '("#000000" "#00FF00" "#0000FF" "#FF0000" "#01FFFE" "#FFA6FE" "#FFDB66" "#006401"
    "#010067" "#95003A" "#007DB5" "#FF00F6" "#FFEEE8" "#774D00" "#90FB92" "#0076FF"
    "#D5FF00" "#FF937E" "#6A826C" "#FF029D" "#FE8900" "#7A4782" "#7E2DD2" "#85A900"
    "#FF0056" "#A42400" "#00AE7E" "#683D3B" "#BDC6FF" "#263400" "#BDD393" "#00B917"
    "#9E008E" "#001544" "#C28C9F" "#FF74A3" "#01D0FF" "#004754" "#E56FFE" "#788231"
    "#0E4CA1" "#91D0CB" "#BE9970" "#968AE8" "#BB8800" "#43002C" "#DEFF74" "#00FFC6"
    "#FFE502" "#620E00" "#008F9C" "#98FF52" "#7544B1" "#B500FF" "#00FF78" "#FF6E41"
    "#005F39" "#6B6882" "#5FAD4E" "#A75740" "#A5FFD2" "#FFB167" "#009BFF" "#E85EBE"))
