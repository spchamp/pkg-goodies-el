(defun insert-missing-autoloads ()
  "Scan emacs-goodies-el.el for autoloads and check if there are in files."
  (interactive)
  (while (re-search-forward "(autoload '\\([^ ]+\\) \"\\(.*\\)\"" nil t)
    (let* ((command (match-string-no-properties 1))
           (efile (concat (match-string-no-properties 2) ".el")))
      (save-excursion
        (find-file efile)
        (goto-char (point-min))
        (when (re-search-forward (concat "^(defun " command "[ (]") nil t)
          (forward-line -1)
          (when (not (looking-at "^;;;###autoload"))
            (end-of-line)
            (insert "\n;;;###autoload")
            (message "Inserted for %s in %s" command efile)
            (save-buffer)))))))



(defun delete-tagged-autoloads ()
  "Scan emacs-goodies-el.el for autoloads and delete those that are marked."
  (interactive)
  (while (re-search-forward "(autoload '\\([^ ]+\\) \"\\(.*\\)\"" nil t)
    (let* ((start (match-beginning 0))
           (command (match-string-no-properties 1))
           (efile (concat (match-string-no-properties 2) ".el"))
           (deleteit))
;;           (the-buffer (create-file-buffer efile)))
      (save-excursion
;;        (set-buffer the-buffer)
;;        (insert-file-contents efile)
        (find-file efile)
        (goto-char (point-min))
        (when (re-search-forward (concat "^(defun " command "[ (]") nil t)
          (forward-line -1)
          (when (looking-at "^;;;###autoload")
            (setq deleteit t))))
      (when deleteit
        (goto-char start)
        (forward-sexp 1)
        (forward-line 1)
        (delete-region start (point))
        (message "***Deleting %s in %s" command efile)))))



(defun insert-defgroup ()
  "Scan buffer for defgroup statements and merge in emacs-goodies-custom.el
Add a :link '
Add a :group 'emacs-goodies-el"
  (interactive)
  (save-excursion
    (when (re-search-forward "^(defgroup \\([^ ]+\\)" nil t)
      (beginning-of-line)
      (let ((filename (file-name-nondirectory (buffer-file-name)))
            (defname (match-string 1))
            (text (buffer-substring (point)(progn (forward-sexp 1)(point)))))
        (if (string-match "^\\(.*\\)\\.el$" filename)
            (setq filename (match-string 1 filename)))
        (find-file "emacs-goodies-custom.el")
        (goto-char (point-max))
        (narrow-to-region (point)(point))
        (insert (format ";; %s\n" filename))
        (insert text)
        (delete-backward-char 1)
        (insert (format "\n  :link '(custom-manual \"(emacs-goodies-el)%s\")\n"
                        filename))
        (insert (format "  :load '%s\n" filename))
        (insert (format "  :require '%s\n" filename))
        (insert         "  :group 'emacs-goodies-el)\n\n")
        (widen)
        (save-buffer)))))

(defun insert-defgroup-dired ()
  "Run through list of elisp files in dired."
  (interactive)
  (while (= 0 (forward-line 1))
    (when (and (looking-at ".*el$")
               (not (looking-at ".*emacs-goodies-el.el$"))
               (not (looking-at ".*emacs-goodies-custom.el$")))
      (save-excursion
        (dired-find-file)
        (emacs-lisp-mode)
        (insert-defgroup))))
  (find-file "emacs-goodies-custom.el")
  (goto-char (point-min))
  (insert ";;; emacs-goodies-custom.el --- Automatically harvested defgroups\n")
  (insert ";;\n")
  (insert ";;  Peter S Galbraith <psg@debian.org>\n")
  (insert ";;  License of copied code applies to this combined work (GPL V2)\n")
  (insert ";;\n")
  (insert ";;; Code:\n\n")
  (goto-char (point-max))
  (insert "(provide 'emacs-goodies-custom)\n"))

      
