;;	$Id: dict.el,v 1.1 2003/04/04 20:15:59 lolando Exp $
;;
;; dict.el - Emacs interface to dict client
;;

;; Copyright (c) 2002 Max Vasin
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Emacs DICT Client is an Emacs wrapper around `dict' command to provide
;; an easy and comfortable (from my point of view) access to the dictd server
;; from the Emacs. The package was written and tested under GNU Emacs 21 only 
;; but I hope it should work under other Emacs versions as well.
;;
;; The package provides several key bindings, which are customisation variables, 
;; so you can change them easily without recompiling the package:
;;     1. `C-c d d' for running dict with options defined by customisation
;;        variables described below.
;;     2. `C-c d r' for running dict on region as a single word.
;;     3. `C-c d m' for running dict on every word in the region.
;;     4. `C-c d s' for running dict to perform search on the given server.
;;     5. `C-c d w' for running wordinspect GUI dict client.
;;
;; Descriptions of all customisation variables are given below in their 
;; definitions, and of cause you can find them in the customisation buffer 
;; (External->Emacs Dict Client).
;;
;; I hope you find the program usefull. And also I would like to know your
;; opinion about the program, improvement suggestions and of cause bug reports.
;; Mail them to <max-appolo@mail.ru>

;;    $Log: dict.el,v $
;;    Revision 1.1  2003/04/04 20:15:59  lolando
;;    Initial revision
;;
;;    Revision 1.25  2002/10/11 09:41:42  max
;;    Name changed to dict.el
;;
;;    Revision 1.24  2002/10/10 10:33:04  max
;;    Added functions to display revision number.
;;
;;    Revision 1.23  2002/10/01 15:05:08  max
;;    dict-server: Changed customisation type.
;;    Added a brief package description.
;;
;;    Revision 1.22  2002/09/30 14:44:12  max
;;    dict-on-server: New function and key binding for it.
;;    dict-get-answer: Changed to run dict asynchronously.
;;
;;    Revision 1.21  2002/09/25 13:34:20  max
;;    Functions dict-databases and dict-strategies removed.
;;
;;    Revision 1.20  2002/09/25 13:30:05  max
;;    Added forward declarations of dict-database and dict-strategy customisation
;;    variables to preserve their relative position in the customisation buffer.
;;    Later they are redefined to use generated sets of values.
;;
;;    Revision 1.19  2002/09/25 13:18:38  max
;;    Changed customisation types for dict-strategy and dict-database.
;;
;;    Revision 1.18  2002/09/23 16:47:12  max
;;    Type of dict-server and dict-database customisation variables
;;    changed to the list of strings.
;;
;;    Revision 1.17  2002/09/22 15:24:22  max
;;    dict-get-answer: Corrected to clear buffer before running dict
;;    and to set point in that buffer to the beginning.
;;
;;    Revision 1.16  2002/09/22 10:36:42  max
;;    - dict-process-key-binding: New function.
;;    - Key bindings changed to work with dict-process-key-binding.
;;    - Call to global-set-key changed to use dict-process-key-binding.
;;
;;    Revision 1.15  2002/09/22 06:44:27  max
;;    - dict-get-answer: New function.
;;    - dict: Changed to use dict-get-answer.
;;    - Now the answer for the dict is stored in the buffer
;;      named *DICT <word>*, where <word> is dict parameters.
;;
;;    Revision 1.14  2002/09/15 07:26:08  max
;;    - dict-region function takes two params
;;    - added dict-multiple function and bindings for it
;;
;;    Revision 1.13  2002/09/13 15:28:52  max
;;    Added simple dict-region function
;;    and a key binding for it.
;;
;;    Revision 1.12  2002/09/03 16:33:39  max
;;    Group name changed to `Emacs Dict client'
;;
;;    Revision 1.11  2002/09/02 12:53:22  max
;;    Customisation type string changed to sexp.
;;    Customisation group moved to the external supergroup.
;;
;;    Revision 1.10  2002/09/02 09:14:04  max
;;    Functions names prefix changed to dict.
;;    Customisation group changed to dict.
;;    Added customisation variables for key bindings.
;;
;;    Revision 1.9  2002/09/02 08:46:06  max
;;    Name changed to emacs-dict-client.el
;;
;;    Revision 1.8  2002/08/30 07:18:53  max
;;    Added documentation to functions
;;
;;    Revision 1.7  2002/08/25 12:54:25  max
;;    Package now provides itself.
;;
;;    Revision 1.6  2002/08/25 12:41:41  max
;;    Added key bindings for running edict and wordinspect.
;;
;;    Revision 1.5  2002/08/25 11:14:25  max
;;    - Added customisation support.
;;    - Removed dependency of man module.
;;
;;    Revision 1.4  2002/08/19 10:32:02  max
;;    edict: Added default value for word.
;;    edict-region: Removed.
;;
;;    Revision 1.3  2002/08/19 10:10:07  max
;;    Log added
;;

(defgroup Emacs-DICT-client nil
  "Browse DICT dictionaries."
  :prefix "dict-"
  :group 'external)

;;;;
;;;; Definitions of dict client parameter variables
;;;;

(defcustom dict-server nil
  "Specifies the hostname for the  DICT  server. Server/port combinations
can be specified in the configuration file. If no servers are specified
the  default  behavior  is  to  try dict.org, alt0.dict.org, alt1.dict.org,
and alt2.dict.org, in that order. If IP lookup for a server expands to a
list of IP addresses (as dict.org does currently), then each IP will be tried
in the order listed. "
  :type '(string)
  :group 'Emacs-DICT-client)

;; Forward declarations
(defcustom dict-database nil
  "foo"
  :type 'string
  :group 'Emacs-DICT-client)

(defcustom dict-strategy nil
  "bar"
  :type 'string
  :group 'Emacs-DICT-client)

(defcustom dict-service nil
  "Specifies the port (e.g., 2628) or service (e.g., dict) for connections. The 
default is 2628, as specified in the DICT Protocol RFC. Server/port combinations 
can be specified in the configuration file."
  :type 'sexp
  :group 'Emacs-DICT-client)

(defcustom dict-match nil
  "Instead of printing a definition, perform a match using the specified strategy."
  :type 'boolean
  :group 'Emacs-DICT-client)

(defcustom dict-nocorrect nil
  "Usually, if a definition is requested and the word cannot be found, spelling correction
is requested from the server, and a list of possible words are provided. This option
disables the generation of this list."
  :type 'sexp
  :group 'Emacs-DICT-client)

(defcustom dict-config-file nil
  "Specify the configuration file. The default is to try ~/.dictrc and /etc/dict.conf, 
using the first file that exists. If a specific configuration file is specified, 
then the defaults will not be tried."
  :type 'sexp
  :group 'Emacs-DICT-client)

(defcustom dict-noauth nil
  "Disable authentication (i.e., don't send an AUTH command)."
  :type '(boolean)
  :group 'Emacs-DICT-client)

(defcustom dict-user nil 
  "Specifies the username for authentication."
  :type 'sexp
  :group 'Emacs-DICT-client)

(defcustom dict-key nil
  "Specifies the shared secret for authentication."
  :type 'sexp
  :group 'Emacs-DICT-client)

(defcustom dict-verbose nil
  "Be verbose."
  :type '(boolean)
  :group 'Emacs-DICT-client)

(defcustom dict-raw nil
  "Be very verbose: show the raw client/server interaction."
  :type 'boolean
  :group 'Emacs-DICT-client)

(defcustom dict-pipesize 256
  "Specify the buffer size for pipelineing commands. The default is 256, which should 
be sufficient for general tasks and be below the MTU for most transport media.  
Larger values may provide faster or slower throughput, depending on MTU. If the 
buffer is too small, requests will be serialized. Values less than 0 and greater 
than one million are silently changed to something more reasonable."
  :type 'integer
  :group 'Emacs-DICT-client)

(defcustom dict-original-server ""
  "Specifies original server name for the dict-on-server function"
  :type 'sexp
  :group 'Emacs-DICT-client)

(defcustom dict-client nil
  "Specifies additional text to be sent using the CLIENT command."
  :type 'sexp
  :group 'Emacs-DICT-client)

(defcustom dict-key-binding "\\C-cdd"
  "Specifies a key binding to run dict and display the results in the Emacs buffer."
  :type 'sexp
  :group 'Emacs-DICT-client)

(defcustom dict-region-key-binding "\\C-cdr"
  "Specifies a key binding to run dict on the region and display the results in the Emacs buffer."
  :type 'sexp
  :group 'Emacs-DICT-client)

(defcustom dict-multiple-key-binding "\\C-cdm"
  "Specifies a key binding to run dict on every word from the region and display the results in 
the Emacs buffer."
  :type 'sexp
  :group 'Emacs-DICT-client)

(defcustom dict-on-server-key-binding "\\C-cds"
  "Specifies a key binding to run dict to search word on the given server and display the results 
in the Emacs buffer."
  :type 'sexp
  :group 'Emacs-DICT-client)

(defcustom dict-wordinspect-key-binding "\\C-cdw"
  "Specifies a key binding to run wordinspect."
  :type 'sexp
  :group 'Emacs-DICT-client)

;;;;
;;;; Service functions
;;;;

(defun dict-get-databases ()
  "Get a list of available databases."
  (let ((dbs (shell-command-to-string "dict -D | tail -n $(expr $(dict -D | wc -l) - 1) | cut -f 3 -d ' '")))
    (sort (read (concat "(" dbs ")")) 'string<)))

(defun dict-get-stategies ()
  "Get a list of strategies."
  (let ((strats (shell-command-to-string "dict -S | tail -n $(expr $(dict -S | wc -l) - 1) | cut -f 3 -d ' '")))
    (sort (read (concat "(" strats ")")) 'string<)))

(defun dict-generate-consts (values)
  "Generate a list of constants for customisation types."
  (if (null values) 
      nil
    (cons `(const ,(car values)) (dict-generate-consts (cdr values)))))

(defcustom dict-strategy nil
  "Specify a matching strategy.  By default, the server default match strategy is used.
This is usually \"exact\" for definitions, and some form of spelling-correction strategy
for matches (\".\" fromthe DICT protocol). The available strategies are dependent on
the server implemenation."
  :type `(choice ,@(dict-generate-consts (dict-get-stategies)) (const :tag "default" nil))
  :group 'Emacs-DICT-client)

(defcustom dict-database nil
  "Specifies a specific database to search. The default is to search all databases 
(a * from the DICT protocol). Note that a ! in the DICT protocol means to 
search all of the databases until a match is found, and then stop searching."
  :type  `(set ,@(dict-generate-consts (dict-get-databases)))
  :group 'Emacs-DICT-client) ;"

(defun dict-generate-options-list (seq prefix)
  "Generate a list of options of the form `prefix seq[0] prefix seq[1] ...'"
  (if (null seq) 
      ""
      (concat prefix (car seq) (dict-generate-options-list (cdr seq) prefix))))

(defun dict-mkseq (string)
  "Make a list from a string"
  (read (concat "(\"" string "\")")))

(defun dict-generate-options ()
  "Generate dict's command line options based on the parameter variables' values"
  (concat
   (if dict-server (dict-generate-options-list (dict-mkseq dict-server) " --host ") "")
   (if dict-service (concat " --port " dict-service) "")
   (if dict-database (dict-generate-options-list (dict-mkseq dict-database) " --database ") "")
   (if dict-match " --match" "")
   (if dict-strategy (concat " --strategy " dict-strategy) "")
   (if dict-nocorrect " --nocorrect " "")
   (if dict-config-file (concat " --config " dict-config-file) "")
   (if dict-noauth " --noauth" "")
   (if dict-user (concat " --user " dict-user) "")
   (if dict-key (concat " --key " dict-key) "")
   (if dict-verbose " --verbose" "")
   (if dict-raw " --raw" "")
   (if (not (= dict-pipesize 256)) (concat " --pipesize " (number-to-string dict-pipesize)) "")
   (if dict-client (concat " --client " dict-client) "")
   " "))

(defun dict-get-answer (word)
  "Recieve the answer from the dict and inserts in ther buffer"
  (let* ((buffer-name (concat "*DICT " word "*"))
	(buffer (get-buffer buffer-name)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (kill-region (point-min) (point-max)))
      (setq buffer (generate-new-buffer buffer-name)))
    (message "Invoking dict %s in the background" word)
    (set-process-sentinel
     (start-process "dict" buffer "sh" "-c"
		    (format "dict %s %s" (dict-generate-options) word))
     'dict-bgproc-sentinel)))

(defun dict-bgproc-sentinel (process msg)
  "Dict background process sentinel."
  (let ((buffer (process-buffer process)))
    (cond 
     ((string= "finished\n" msg) 
      (save-excursion (set-buffer buffer)
		      (goto-char (point-min))
		      (display-buffer buffer)))
     ((string-match "exited abnormally with code" msg)
      (message msg)))))

(defsubst dict-default-dict-entry ()
  "Make a guess at a default dict entry.
This guess is based on the text surrounding the cursor."
  (let (word)
    (save-excursion
      (setq word (current-word))
      (if (string-match "[._]+$" word)
	  (setq word (substring word 0 (match-beginning 0))))
      word)))

(defun dict-process-key-binding (string)
  "Process a string representation of key binding to allow easy key binding
customisation."
  (read (concat "\"" string "\"")))

;;;;
;;;; Lookup functions
;;;;

(defun dict (word)
  "Lookup a word in the dictionary"
  (interactive (list (let* ((default-entry (dict-default-dict-entry)) 
	     (input (read-string 
		     (format "Dict entry%s: " 
			     (if (string= default-entry "") 
				 "" 
			       (format " (default %s)" default-entry)))))) 
	(if (string= input "") 
	    (if (string= default-entry "") 
		(error "No dict args given") default-entry) input))))
  (dict-get-answer word))

(defun dict-region (from to)
  "Lookup a region in the dictionary"
  (interactive (list (region-beginning) 
		     (region-end)))
  (dict (concat "\"" (buffer-substring-no-properties from to) "\"")))

(defun dict-multiple (from to)
  "Lookup every word from the region in the dictionary"
  (interactive (list (region-beginning) 
		     (region-end)))
  (dict (buffer-substring-no-properties from to)))

(defun dict-on-server (word server)
  "Lookup a word in the dictionary on the given server"
  (interactive (list 
		(let* ((default-entry (dict-default-dict-entry)) 
		       (input (read-string 
			       (format "Dict entry%s: " 
				       (if (string= default-entry "") 
					   "" 
					 (format " (default %s)" default-entry)))))) 
		  (if (string= input "") 
		      (if (string= default-entry "") 
			  (error "No dict args given") default-entry) input))
		(read-string "DICT server: " nil)))
  (if (not (string= server ""))
      (let ((dict-server server))
	(dict word))
    (dict word)))

(defun dict-word-inspect (word)
  "Run wordinspect GUI dict client"
  (interactive (list (let* ((default-entry (dict-default-dict-entry)) 
	     (input (read-string 
		     (format "Dict entry%s: " 
			     (if (string= default-entry "") 
				 "" 
			       (format " (default %s)" default-entry)))))) 
	(if (string= input "") 
	    (if (string= default-entry "") 
		(error "No dict args given") default-entry) input))))
  (shell-command (concat "wordinspect " word "&"))
  (delete-other-windows))

;;;;
;;;; Informational functions
;;;;

(defun dict-version ()
  "Display dict version information."
  (interactive)
  (shell-command "dict --version"))

(defconst dict-self-version
  "$Revision: 1.1 $"
  "Version number for 'emacs-dict-client' package.")

(defun dict-version-number ()
  "Return 'emacs-dict-client' version number."
  (string-match "[0123456789.]+" dict-self-version)
  (match-string 0 dict-self-version))

(defun dict-display-version ()
  "Display 'emacs-dict-client' version."
  (interactive)
  (message "Emacs DICT client version <%s>." (dict-version-number)))

;; Setup global key binding `C-c d d' for running dict...
(global-set-key (dict-process-key-binding dict-key-binding) 'dict)
;; ... `C-c d r' for running dict on the region...
(global-set-key (dict-process-key-binding dict-region-key-binding) 'dict-region)
;; ... `C-c d m' for running dict on every word in the region...
(global-set-key (dict-process-key-binding dict-multiple-key-binding) 'dict-multiple)
;; ... `C-c d s' for running dict to perform search on the given server...
(global-set-key (dict-process-key-binding dict-on-server-key-binding) 'dict-on-server)
;; ... and `C-c d w' for running wordinspect.
(global-set-key (dict-process-key-binding dict-wordinspect-key-binding) 'dict-word-inspect)

(provide 'dict)

; LocalWords:  dict dictd wordinspect appolo ru
