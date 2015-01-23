(in-package :yomi)


(defparameter *yomi-server-port* 8888)
(defparameter *web-socket-port* 9999)


(defvar *ws-loc* "/ws_yomi")

(defparameter *server-running-p* nil)
   

;; renaming
(defparameter *working-directory*
  (merge-pathnames-as-directory (user-homedir-pathname) #p"Desktop/"))



(defvar *notebook-clients* nil)

;; I believe 20 is much more than enough.
;; unless you hit play button like cracy for heavy jobs
(defparameter *max-eval-threads* 20)


;; one of "emacs" "vim" "sublime"
(defparameter *keymap* "emacs")



(defparameter *yomi-path-default*
  (asdf:system-source-directory 'yomi))


(defparameter *default-working-package* "YNB")


;; 
(defparameter *js-css-files*
  '("flot/jquery.js"
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
  '(
    "images/symbolize-icons-set/png/128x128/play.png"
    "images/symbolize-icons-set/png/128x128/arrow_up.png" 
    "images/symbolize-icons-set/png/128x128/arrow_down.png" 
    "images/symbolize-icons-set/png/128x128/stop.png" 
    "images/symbolize-icons-set/png/128x128/cut.png" 
    "images/symbolize-icons-set/png/128x128/save.png"
    "images/symbolize-icons-set/png/128x128/notebook.png"
    "images/symbolize-icons-set/png/128x128/folder.png"
    "images/symbolize-icons-set/png/128x128/fast_forward.png"
    "images/symbolize-icons-set/png/128x128/add.png"
    "images/symbolize-icons-set/png/128x128/undo.png"    
    ))


;; Scraped from
;; http://www.w3schools.com/html/html_colornames.asp
(defparameter *color-list*
  '("AliceBlue"
    "AntiqueWhite"
    "Aqua"
    "Aquamarine"
    "Azure"
    "Beige"
    "Bisque"
    "Black"
    "BlanchedAlmond"
    "Blue"
    "BlueViolet"
    "Brown"
    "BurlyWood"
    "CadetBlue"
    "Chartreuse"
    "Chocolate"
    "Coral"
    "CornflowerBlue"
    "Cornsilk"
    "Crimson"
    "Cyan"
    "DarkBlue"
    "DarkCyan"
    "DarkGoldenRod"
    "DarkGray"
    "DarkGreen"
    "DarkKhaki"
    "DarkMagenta"
    "DarkOliveGreen"
    "DarkOrange"
    "DarkOrchid"
    "DarkRed"
    "DarkSalmon"
    "DarkSeaGreen"
    "DarkSlateBlue"
    "DarkSlateGray"
    "DarkTurquoise"
    "DarkViolet"
    "DeepPink"
    "DeepSkyBlue"
    "DimGray"
    "DodgerBlue"
    "FireBrick"
    "FloralWhite"
    "ForestGreen"
    "Fuchsia"
    "Gainsboro"
    "GhostWhite"
    "Gold"
    "GoldenRod"
    "Gray"
    "Green"
    "GreenYellow"
    "HoneyDew"
    "HotPink"
    "IndianRed"
    "Indigo"
    "Ivory"
    "Khaki"
    "Lavender"
    "LavenderBlush"
    "LawnGreen"
    "LemonChiffon"
    "LightBlue"
    "LightCoral"
    "LightCyan"
    "LightGoldenRodYellow"
    "LightGray"
    "LightGreen"
    "LightPink"
    "LightSalmon"
    "LightSeaGreen"
    "LightSkyBlue"
    "LightSlateGray"
    "LightSteelBlue"
    "LightYellow"
    "Lime"
    "LimeGreen"
    "Linen"
    "Magenta"
    "Maroon"
    "MediumAquaMarine"
    "MediumBlue"
    "MediumOrchid"
    "MediumPurple"
    "MediumSeaGreen"
    "MediumSlateBlue"
    "MediumSpringGreen"
    "MediumTurquoise"
    "MediumVioletRed"
    "MidnightBlue"
    "MintCream"
    "MistyRose"
    "Moccasin"
    "NavajoWhite"
    "Navy"
    "OldLace"
    "Olive"
    "OliveDrab"
    "Orange"
    "OrangeRed"
    "Orchid"
    "PaleGoldenRod"
    "PaleGreen"
    "PaleTurquoise"
    "PaleVioletRed"
    "PapayaWhip"
    "PeachPuff"
    "Peru"
    "Pink"
    "Plum"
    "PowderBlue"
    "Purple"
    "Red"
    "RosyBrown"
    "RoyalBlue"
    "SaddleBrown"
    "Salmon"
    "SandyBrown"
    "SeaGreen"
    "SeaShell"
    "Sienna"
    "Silver"
    "SkyBlue"
    "SlateBlue"
    "SlateGray"
    "Snow"
    "SpringGreen"
    "SteelBlue"
    "Tan"
    "Teal"
    "Thistle"
    "Tomato"
    "Turquoise"
    "Violet"
    "Wheat"
    "White"
    "WhiteSmoke"
    "Yellow"
    "YellowGreen"))
