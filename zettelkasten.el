;;; Zettelkasten --- Helper functions to organise notes.
;;; Commentary:
;;;
;;; Used to organise notes using the Zettelkasten method.
;;;
;;; Code:

(defgroup zettelkasten nil
  "Helper to work with zettelkasten notes."
  :group 'applications)

(defcustom zettelkasten-prefix [(control ?c) ?k]
  "Prefix key to use for Zettelkasten commands in Zettelkasten minor mode.
The value of this variable is checked as part of loading Zettelkasten mode.
After that, changing the prefix key requires manipulating keymaps."
  :type 'key-sequence
  :group 'zettelkasten)

(defcustom zettelkasten-directory "~/Projects/zettelkasten"
  "Main zettelkasten directory."
  :type 'string
  :group 'zettelkasten)

(defcustom zettelkasten-file-format "%y%W%u%%03d.org"
  "Format for new zettelkasten files.

For supported options, please consult `format-time-string'."
  :type 'string
  :group 'zettelkasten)

(defcustom zettelkasten-top-level "index.org"
  "Top level zettelkasten file that starts the DAG hierarchy."
  :type 'string
  :group 'zettelkasten)

(defun zettelkasten-make-filename (filename)
  "Make a filename using the default directory and the FILENAME passed to it."
  (expand-file-name (concat zettelkasten-directory "/" filename)))

(defun zettelkasten-filename-to-id (filename)
  "Convert FILENAME to id."
  (progn (string-match "\\(.*\\)\\.org\\'" filename)
         (match-string 1 filename)))

(defun zettelkasten-list-notes-by-id ()
  "Return all the ids that are currently available."
  (mapcar #'zettelkasten-filename-to-id
          (directory-files
           (expand-file-name zettelkasten-directory) nil "\\.org$" t)))

(defun zettelkasten-list-notes ()
  "Return all the ids and titles of notes in the `zettelkasten-directory'."
  (progn (shell-command (concat "grep -i \"#+TITLE:\" " zettelkasten-directory "/*"))
         (setq match-list nil)
         (with-current-buffer "*Shell Command Output*"
           (progn
             (set-buffer "*Shell Command Output*")
             (setq morelines t)
             (goto-char 1)
             (while morelines
               (setq current-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (when (string-match "\\([0-9]*\\)\\.org:#\\+TITLE: \\(.*\\)" current-string)
                   (setq matched-string (concat (match-string 1 current-string) " - " (match-string 2 current-string)))
                 (setq match-list (append match-list (list matched-string))))
               (setq morelines (= 0 (forward-line 1))))))
         (kill-buffer "*Shell Command Output*")
         match-list))

(defun zettelkasten-find-new-note-name (file iteration)
  "Iterate on ITERATION until a usable file based on FILE is found."
  (let ((expanded-name
         (zettelkasten-make-filename
          (format file iteration))))
    (if (file-exists-p expanded-name)
        (zettelkasten-find-new-note-name file (+ iteration 1))
      expanded-name)))

(defun zettelkasten-generate-note-name ()
  "Create the new note name."
  (zettelkasten-find-new-note-name
   (format-time-string zettelkasten-file-format) 0))

(defun zettelkasten-create-new-note (title)
  "Create a new zettelkasten note using the TITLE it is passed."
  (interactive "MNote title: ")
  (progn (find-file (zettelkasten-generate-note-name))
         (insert (concat "#+TITLE: " title
                         (format-time-string "\n#+DATE: %c\n#+TAGS:\n\n")))
         (save-buffer)))

(defun zettelkasten-insert-link (note)
  "Insert a link to another NOTE in the current note."
  (interactive
   (list (completing-read "Notes: "
                          (zettelkasten-list-notes) nil 'match)))
  (let ((note-id
         (progn (string-match "\\([0-9]*\\)" note)
                (match-string 1 note))))
    (insert (concat "[[./" note-id ".org][" note "]]"))))

(defvar zettelkasten-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'zettelkasten-insert-link)
    (define-key map "n" 'zettelkasten-create-new-note)
    map))

(defvar zettelkasten-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map zettelkasten-prefix zettelkasten-mode-map)
    map)
  "Keymap used for binding footnote minor mode.")

(define-minor-mode zettelkasten-mode
  "Enable the keymaps to be used with zettelkasten."
  :lighter " zettelkasten"
  :keymap zettelkasten-minor-mode-map)

(provide 'zettelkasten)

;;; zettelkasten.el ends here
