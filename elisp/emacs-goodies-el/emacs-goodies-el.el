;;; emacs-goodies-el.el --- startup file for the emacs-goodies-el package

;;; Commentary:
;; 
;; This file is loaded from /etc/emacs/site-start.d/50emacs-goodies-el.el

;;; History:
;;
;; 2003-05-14 - Peter Galbraith - Created from 50emacs-goodies-el.el contents.

;;; Code:

(defgroup emacs-goodies-el nil
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

(defcustom emacs-goodies-el-use-ff-paths emacs-goodies-el-defaults
  "Whether to setup ff-paths for use.
find-file-using-paths searches certain paths to find files."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (require 'ff-paths)))
  :group 'emacs-goodies-el)

(defcustom emacs-goodies-el-use-ffap emacs-goodies-el-defaults
  "Whether to setup ffap for use, and add ff-paths as a helper to it.
ffap.el --- find file (or url) at point.
non-nil also setups the keybindings:
 C-x C-f       find-file-at-point (abbreviated as ffap)
 C-x 4 f       ffap-other-window
 C-x 5 f       ffap-other-frame
 S-mouse-3     ffap-at-mouse
 C-S-mouse-3   ffap-menu"
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (require 'ffap)
           (ffap-bindings)
           (require 'ff-paths)
           (ff-paths-in-ffap-install)))
  :group 'emacs-goodies-el)

;; autoloads for apt-utils.el
(autoload 'apt-utils-show-package "apt-utils"
  "Write APT package information to buffer.
With ARG, choose that package, otherwise prompt for one."
  t)
(autoload 'apt-utils-search "apt-utils"
  "Search Debian packages for regular expression.
With ARG, match names only."
  t)

; autoloads for boxquote.el
(autoload 'boxquote-title "boxquote"
  "Set the title of the current boxquote to TITLE."
  t)
(autoload 'boxquote-region "boxquote"
  "Draw a box around the left hand side of a region bounding START and END."
  t)
(autoload 'boxquote-buffer "boxquote"
  "Apply `boxquote-region' to a whole buffer."
  t)
(autoload 'boxquote-insert-file "boxquote"
  "Insert the contents of a file, boxed with `boxquote-region'."
  t)
(autoload 'boxquote-yank "boxquote"
  "Do a `yank' and box it in with `boxquote-region'."
  t)
(autoload 'boxquote-defun "boxquote"
  "Apply `boxquote-region' the current defun."
  t)
(autoload 'boxquote-paragraph "boxquote"
  "Apply `boxquote-region' to the current paragraph."
  t)
(autoload 'boxquote-boxquote "boxquote"
  "Apply `boxquote-region' to the current boxquote."
  t)
(autoload 'boxquote-describe-function "boxquote"
  "Call `describe-function' and boxquote the output into the current buffer."
  t)
(autoload 'boxquote-describe-variable "boxquote"
  "Call `describe-variable' and boxquote the output into the current buffer."
  t)
(autoload 'boxquote-describe-key "boxquote"
  "Call `describe-key' and boxquote the output into the current buffer."
  t)
(autoload 'boxquote-narrow-to-boxquote "boxquote"
  "Narrow the buffer to the current boxquote."
  t)
(autoload 'boxquote-kill "boxquote"
  "Kill the boxquote and its contents."
  t)
(autoload 'boxquote-unbox-region "boxquote"
  "Remove a box created with `boxquote-region'."
  t)
(autoload 'boxquote-unbox "boxquote"
  "Remove the boxquote that contains `point'."
  t)

; autoloads for thinks.el
(autoload 'thinks "thinks"
  "Insert TEXT wrapped in a think bubble."
  t)
(autoload 'thinks-region "thinks"
  "Bubble wrap region bounding START and END."
  t)
(autoload 'thinks-yank "thinks"
  "Do a `yank' and bubble wrap the yanked text."
  t)
(autoload 'thinks-maybe-region "thinks"
  "If region is active, bubble wrap region bounding START and END.
If not, query for text to insert in bubble."
  t)

; autoloads for bar-cursor.el
(autoload 'bar-cursor-mode "bar-cursor"
  "Toggle use of 'bar-cursor-mode'."
  t)

; autoloads for tld.el
(autoload 'tld "tld"
  "Perform a TLD lookup"
  t)

; autoloads for protocols.el
(autoload 'protocols-lookup "protocols"
  "Perform a protocol lookup"
  t)

; autoloads for services.el
(autoload 'services-lookup "services"
  "Perform a service lookup"
  t)

; autoloads for highlight-completion.el
(autoload 'highlight-completion-mode "highlight-completion"
  "Activate highlight-completion."
  t)

; autoloads for browse-kill-ring.el
(autoload 'browse-kill-ring "browse-kill-ring"
  "Display items in the `kill-ring' in another buffer."
  t)

; autoloads for coffee.el
(autoload 'coffee "coffee"
  "Submit a BREW request to an RFC2324-compliant coffee device"
  t)

; autoloads for twiddle.el
(autoload 'twiddle-start "twiddle"
  "Start a mode line display hack."
  t)
(autoload 'twiddle-compile "twiddle"
  "Like \\[compile], but run a twiddle hack during compilation."
  t)

; autoloads for whitespace.el
(autoload 'nuke-trailing-whitespace "nuke-trailing-whitespace"
  "Nuke all trailing whitespace in the buffer."
  t)

; autoloads for silly-mail.el
(autoload 'sm-add-random-header "silly-mail"
  "Insert a random silly mail header."
  t)
(autoload 'sm-add-all-headers "silly-mail"
  "Insert one of every kind of silly mail header defined."
  t)

; autoloads for obfusurl.el
(autoload 'obfuscate-url "obfusurl"
  "Obfuscate URL under point"
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
  
; autoloads for protbuf.el
(autoload 'protect-buffer-from-kill-mode "protbuf"
  "Protect buffer from being killed."
  t)
(autoload 'protect-process-buffer-from-kill-mode "protbuf"
  "Protect buffer from being killed as long as it has an active process."
  t)

; autoloads for setnu.el
(autoload 'setnu-mode "setnu"
  "Toggle setnu-mode."
  t)
(autoload 'turn-on-setnu-mode "setnu"
  "Turn on setnu-mode."
  t)

; autoloads for wdired.el
(autoload 'wdired-change-to-wdired-mode "wdired"
  "Put a dired buffer in a mode in which filenames are editable."
  t)
(add-hook
 'dired-load-hook
 '(lambda ()
    (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
    (define-key dired-mode-map
      [menu-bar immediate wdired-change-to-wdired-mode]
      '("Edit File Names" . wdired-change-to-wdired-mode))))

; autoloads for floatbg.el
(autoload 'floatbg-mode "floatbg"
  "Toggle floatbg mode."
  t)

; autoloads for clipper.el
(autoload 'clipper-create "clipper"
  "Create a new 'clip' for use within Emacs."
  t)
(autoload 'clipper-delete "clipper"
  "Delete an existing 'clip'."
  t)
(autoload 'clipper-insert "clipper"
  "Insert a new 'clip' into the current buffer."
  t)
(autoload 'clipper-edit-clip "clipper"
  "Edit an existing 'clip'."
  t)

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

; autoloads for df.el
(autoload 'df "df"
  "Enables display of space left on any filesystem in mode lines."
  t)

; autoloads for egocentric.el
(autoload 'egocentric-mode "egocentric"
  "Highlight your name or various keywords in buffers."
  t)

; autoloads for under.el
(autoload 'underline-region "under"
  "Underline the region."
  t)

; autoloads for highlight-current-line.el
(autoload 'highlight-current-line-on "highlight-current-line"
  "Switch highlighting of cursor-line on/off."
  t)

; autoloads for align-string.el
(autoload 'align-string "align-string"
  "Align first occurrence of REGEXP in each line of region."
  t)
(autoload 'align-all-strings "align-string"
  "Align all occurrences of REGEXP in each line of region."
  t)

; autoloads for diminish.el
(autoload 'diminish "diminish"
  "Diminish mode-line display of minor mode MODE to TO-WHAT (default \"\")."
  t)
(autoload 'diminish-undo "diminish"
  "Restore mode-line display of diminished mode MODE to its minor-mode value."
  t)
(autoload 'diminished-modes "diminish"
  "Echo all active diminished or minor modes as if they were minor."
  t)

; autoloads for htmlize.el
(autoload 'htmlize-buffer "htmlize"
  "Convert buffer to HTML, preserving the font-lock colorization."
  t)
(autoload 'htmlize-region "htmlize"
  "Convert region to HTML, preserving the font-lock colorization."
  t)
(autoload 'htmlize-file "htmlize"
  "HTML-ize FILE, and save the result."
  t)
(autoload 'htmlize-many-files "htmlize"
  "HTML-ize files specified by FILES, and save them to `.html' files."
  t)
(autoload 'htmlize-many-files-dired "htmlize"
  "HTMLize dired-marked files."
  t)

; autoloads for keydef.el
(autoload 'keydef "keydef"
  "Define the key sequence SEQ, written in kbd form, to run CMD."
  t)

; autoloads for all.el
(autoload 'all "all"
  "Edit all lines matching a given regexp."
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

; autoloads for table.el
(autoload 'table-insert "table"
  "Insert an editable text table."
  t)
(autoload 'table-recognize "table"
  "Recognize all tables within the current buffer and activate them."
  t)

; autoloads for tc.el
(autoload 'trivial-cite "tc"
  "trivial-cite is a simple citation function for use in news/mailreaders."
  t)

; autoloads and automode for apt-sources.el
(autoload 'apt-sources-mode "apt-sources"
  "Major mode for editing apt's sources.list file."
  t)
(add-to-list 'auto-mode-alist '("sources.list$" . apt-sources-mode))

; autoloads and automode for muttrc-mode.el
(autoload 'muttrc-mode "muttrc-mode"
  "Major mode to edit muttrc files."
  t)
(add-to-list 'auto-mode-alist '("muttrc" . muttrc-mode))

; autoloads for ibuffer.el
(autoload 'ibuffer "ibuffer"
  "Begin using `ibuffer' to edit a list of buffers."
  t)

; autoloads for home-end.el
(autoload 'home-end-home "home-end"
  "Go to beginning of line/window/buffer."
  t)
(autoload 'home-end-end "home-end"
  "Go to end of line/window/buffer."
  t)

; autoloads and automode for xrdb-mode.el
(autoload 'xrdb-mode "xrdb-mode"
  "Mode for editing X resource files"
  t)
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

; autoloads for sys-apropos.el
(autoload 'sys-apropos "sys-apropos"
  "Ask the system apropos command for man-pages matching QUERY."
  t)

(provide 'emacs-goodies-el)

;;; emacs-goodies-el.el ends here