;;; emacs-goodies-el.el --- startup file for the emacs-goodies-el package

;;; Commentary:
;; 
;; This file is loaded from /etc/emacs/site-start.d/50emacs-goodies-el.el

;;; History:
;;
;; 2003-06-14 - Peter Galbraith
;;  - Delete autoloads that can be generated automatically.
;; 2003-05-14 - Peter Galbraith
;;  - Created from 50emacs-goodies-el.el contents.

;;; Code:

(require 'emacs-goodies-loaddefs)

(defgroup emacs-goodies-el '((apt-sources custom-group)
                             (apt-utils custom-group)
                             (auto-fill-inhibit custom-group)
                             (bar-cursor custom-group)
                             (box-quote custom-group)
                             (browse-kill-ring custom-group)
                             (df custom-group)
                             (diminish custom-group)
                             (ff-paths custom-group))
  "Debian emacs-goodies-el package customization."
  :group 'convenience)

(defcustom emacs-goodies-el-defaults nil
  "Whether default settings are chosen conservatively or aggressively.
non-nil means aggressive.
Setting to aggresisve will enable feature that superceed Emacs defaults."
  :type '(radio (const :tag "conservative" nil)
                (const :tag "aggressive" t))
  :link '(custom-manual "(emacs-goodies-el)Top")
  :group 'emacs-goodies-el)

;; align-string.el
(autoload 'align-string "align-string"
  "Align first occurrence of REGEXP in each line of region."
  t)
(autoload 'align-all-strings "align-string"
  "Align all occurrences of REGEXP in each line of region."
  t)

;; apt-sources
(add-to-list 'auto-mode-alist '("sources.list$" . apt-sources-mode))
(defgroup apt-sources nil "Mode for editing apt source.list file"
  :group 'tools
  :load 'apt-sources
  :link '(custom-manual "(emacs-goodies-el)apt-sources")
  :prefix "apt-sources-")

;; apt-utils.el
(defgroup apt-utils nil
  "Emacs interface to APT (Debian package management)"
  :load 'apt-utils
  :link '(custom-manual "(emacs-goodies-el)apt-utils")
  :group 'tools)

(autoload 'apt-utils-search "apt-utils"
  "Search Debian packages for regular expression.
With ARG, match names only."
  t)

;;  auto-fill-inhibit.el
(defgroup auto-fill-inhibit '((auto-fill-inhibit-list custom-variable))
  "Finer grained control over auto-fill-mode (de)activation."
  :load 'auto-fill-inhibit
  :link '(custom-manual "(emacs-goodies-el)auto-fill-inhibit")
  :group 'emacs-goodies-el)

;; bar-cursor.el
(autoload 'bar-cursor-change "bar-cursor"
  "Enable or disable advice based on value of variable `bar-cursor-mode'."
  t)
(defgroup bar-cursor '((bar-cursor-mode custom-variable))
  "switch block cursor to a bar."
  :load 'bar-cursor
  :link '(custom-manual "(emacs-goodies-el)bar-cursor")
  :group 'emacs-goodies-el)

;; boxquote.el
(defgroup boxquote nil
  "Mark regions of text with a half-box."
  :load 'boxquote
  :link '(custom-manual "(emacs-goodies-el)boxquote")
  :group  'editing
  :prefix "boxquote-")

;; browse-kill-ring.el
(defgroup browse-kill-ring nil
  "A package for browsing and inserting the items in `kill-ring'."
  :link '(url-link "http://web.verbum.org/~walters")
  :link '(custom-manual "(emacs-goodies-el)browse-kill-ring")
  :group 'convenience)

;; clipper.el
(autoload 'clipper-create "clipper" "Create a new 'clip' for use within Emacs."
  t)
(autoload 'clipper-delete "clipper" "Delete an existing 'clip'." t)
(autoload 'clipper-insert "clipper" 
  "Insert a new 'clip' into the current buffer."
  t)
(autoload 'clipper-edit-clip "clipper" "Edit an existing 'clip'." t)

;; df.el
(defgroup df nil
  "Display space left on partitions in the mode-line."
  :load 'df
  :link '(custom-manual "(emacs-goodies-el)df")
  :group 'tools)

;; diminish.el
(defgroup diminish '((diminished-minor-modes custom-variable))
  "Diminished modes are minor modes with no modeline display."
  :load 'diminish
  :link '(custom-manual "(emacs-goodies-el)diminish")
  :group 'emacs-goodies-el)

;; ff-paths.el
(defgroup ff-paths nil
  "Find file using paths."
  :link '(custom-manual "(emacs-goodies-el)ff-paths")
  :group 'ffap
  :group 'matching
  :group 'convenience)

(defcustom ff-paths-install emacs-goodies-el-defaults
  "Whether to setup ff-paths for use.
find-file-using-paths searches certain paths to find files."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (ff-paths-install)))
  :load 'ff-paths
  :group 'emacs-goodies-el
  :group 'ff-paths)

(defcustom ff-paths-use-ffap emacs-goodies-el-defaults
  "Whether to setup ffap for use.

Usually packages don't advertise or try to setup other packages, but
ff-paths works well in combination with ffap (Find FILENAME, guessing a
default from text around point) and so I recommend it here.

find-file-using-paths searches certain paths to find files."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (ff-paths-in-ffap-install)))
  :load 'ff-paths
  :group 'emacs-goodies-el
  :group 'ff-paths)

; autoloads for highlight-completion.el
(autoload 'highlight-completion-mode "highlight-completion"
  "Activate highlight-completion."
  t)

; autoloads for toggle-buffer.el
(autoload 'joc-toggle-buffer "toggle-buffer"
  "Switch to previous active buffer."
  t)

; autoloads for mutt-alias.el
(autoload 'mutt-alias-insert "mutt-alias"
  "Insert the expansion for ALIAS into the current buffer."
  t)
(autoload 'mutt-alias-lookup "mutt-alias"
  "Lookup and display the expansion for ALIAS."
  t)
  
; autoloads for setnu.el
(autoload 'setnu-mode "setnu"
  "Toggle setnu-mode."
  t)
(autoload 'turn-on-setnu-mode "setnu"
  "Turn on setnu-mode."
  t)

; autoloads for wdired.el
(add-hook
 'dired-load-hook
 '(lambda ()
    (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
    (define-key dired-mode-map
      [menu-bar immediate wdired-change-to-wdired-mode]
      '("Edit File Names" . wdired-change-to-wdired-mode))))


; autoloads for projects.el
(autoload 'add-project "projects"
  "Add the project named NAME with root directory DIRECTORY."
  t)
(autoload 'remove-project "projects"
  "Remove the project named NAME."
  t)
(autoload 'list-projects "projects"
  "List all projects sorted by project name."
  t)

; autoloads for toggle-case.el
(autoload 'joc-toggle-case "toggle-case"
  "Toggles the case of the character under point."
  t)

(autoload 'joc-toggle-case "toggle-case-backwards"
  "Toggle case of character preceding point."
  t)

(autoload 'joc-toggle-case "toggle-case-by-word"
  "Toggles the case of the word under point."
  t)

(autoload 'joc-toggle-case "toggle-case-by-word-backwards"
  "Toggles the case of the word preceding point."
  t)

(autoload 'joc-toggle-case "toggle-case-by-region"
  "Toggles the case of all characters in the current region."
  t)

; autoloads for tail.el
(autoload 'tail-file "tail"
  "Tails file specified with argument ``file'' inside a new buffer."
  t)
(autoload 'tail-command "tail"
  "Tails command specified with argument ``command'' inside a new buffer."
  t)

; autoloads for under.el
(autoload 'underline-region "under"
  "Underline the region."
  t)

; autoloads for highlight-current-line.el
(autoload 'highlight-current-line-on "highlight-current-line"
  "Switch highlighting of cursor-line on/off."
  t)

; autoloads for keydef.el
(autoload 'keydef "keydef"
  "Define the key sequence SEQ, written in kbd form, to run CMD."
  t)

; autoloads for toggle-option.el
(autoload 'toggle-option "toggle-option"
  "Easily toggle frequently toggled options."
  t)

; autoloads and automode for todoo.el
(autoload 'todoo "todoo"
  "TODO Mode."
  t)
(autoload 'todoo-mode "todoo"
  "TODO Mode"
  t)
(add-to-list 'auto-mode-alist '("TODO$" . todoo-mode))

; autoloads for cyclebuffer.el
(autoload 'cyclebuffer-forward "cyclebuffer"
  "Cycle buffer forward."
  t)
(autoload 'cyclebuffer-backward "cyclebuffer"
  "Cycle buffer backward."
  t)

; autoloads for keywiz.el
(autoload 'keywiz "keywiz"
  "Start a key sequence quiz."
  t)

; autoloads and automode for muttrc-mode.el
(add-to-list 'auto-mode-alist '("muttrc" . muttrc-mode))

; autoloads and automode for xrdb-mode.el
(add-to-list 'auto-mode-alist '("\\.Xdefaults$" . xrdb-mode))
(add-to-list 'auto-mode-alist '("\\.Xenvironment$". xrdb-mode))
(add-to-list 'auto-mode-alist '("\\.Xresources$". xrdb-mode))
(add-to-list 'auto-mode-alist '("\\.ad$". xrdb-mode))
(add-to-list 'auto-mode-alist '("/app-defaults/". xrdb-mode))
(add-to-list 'auto-mode-alist '("/Xresources/". xrdb-mode))

; autoloads for map-lines.el
(autoload 'map-lines "map-lines"
  "Map COMMAND over lines matching REGEX."
  t)

(provide 'emacs-goodies-el)

;;; emacs-goodies-el.el ends here
