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

(defcustom zettelkasten-file-format "%y%W%u%%02d"
  "Format for new zettelkasten files.

For supported options, please consult `format-time-string'."
  :type 'string
  :group 'zettelkasten)

(defcustom zettelkasten-link-format "[[./%2$s.%3$s][%1$s]]"
  "Zettelkasten link format."
  :type 'string
  :group 'zettelkasten)

(defcustom zettelkasten-extension "org"
  "Default extension for notes."
  :type 'string
  :group 'zettelkasten)

(defun zettelkasten-make-filename (note)
  "Make a filename using the default directory and the NOTE passed to it."
  (expand-file-name (concat zettelkasten-directory "/" note "." zettelkasten-extension)))

(defun zettelkasten-filename-to-id (filename)
  "Convert FILENAME to id."
  (string-match (format "\\(.*\\)\\.%s\\'" zettelkasten-extension) filename)
  (match-string 1 filename))

(defun zettelkasten-list-notes-by-id ()
  "Return all the ids that are currently available."
  (mapcar #'zettelkasten-filename-to-id
          (directory-files
           (expand-file-name zettelkasten-directory) nil
           (format "\\.%s$" zettelkasten-extension) t)))

(defun zettelkasten-list-notes ()
  "Return all the ids and titles of notes in the `zettelkasten-directory'."
  (shell-command (concat "grep -i \"#+TITLE:\" " zettelkasten-directory "/*"))
  (setq match-list nil)
  (with-current-buffer "*Shell Command Output*"
    (set-buffer "*Shell Command Output*")
    (setq morelines t)
    (goto-char 1)
    (while morelines
      (setq current-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      (when (string-match
             (format "\\([0-9]*\\)\\.%s:#\\+TITLE: \\(.*\\)" zettelkasten-extension)
             current-string)
        (setq matched-string (concat (match-string 1 current-string) " - " (match-string 2 current-string)))
        (setq match-list (append match-list (list matched-string))))
      (setq morelines (= 0 (forward-line 1)))))
  (kill-buffer "*Shell Command Output*")
  match-list)

(defun zettelkasten-find-new-note-name (note iteration)
  "Iterate on ITERATION until a usable file based on NOTE is found."
  (let ((expanded-name
         (zettelkasten-make-filename
          (format note iteration))))
    (if (file-exists-p expanded-name)
        (zettelkasten-find-new-note-name note (+ iteration 1))
      (format note iteration))))

(defun zettelkasten-generate-note-name ()
  "Create the new note name."
  (zettelkasten-find-new-note-name
   (format-time-string zettelkasten-file-format) 0))

(defun zettelkasten-get-id (note)
  "Return the id for a NOTE."
  (string-match "\\([0-9]*\\)" note)
  (match-string 1 note))

(defun zettelkasten-format-link (note)
  "Format a link to a NOTE."
  (format zettelkasten-link-format note (zettelkasten-get-id note) zettelkasten-extension))

(defun zettelkasten-add-link-to-parent (note parent)
  "Add a link to NOTE from PARENT."
  (with-temp-file (zettelkasten-make-filename parent)
    (insert-file-contents-literally (zettelkasten-make-filename parent))
    (goto-char (point-max))
    (insert (concat "\n" (zettelkasten-format-link note)))))

(defun zettelkasten-create-new-note-non-interactive (title parent)
  "Create a new note based on the TITLE and it's PARENT note.

If PARENT is nil, it will not add a link from a parent."
  (let ((note (zettelkasten-generate-note-name)))
    (zettelkasten-add-link-to-parent note (zettelkasten-get-id parent))
    (find-file (zettelkasten-make-filename note))
    (insert (concat "#+TITLE: " title
                    (format-time-string "\n#+DATE: %c\n#+TAGS:\n\n")))
    (save-buffer)))

(defun zettelkasten-create-new-note (prefix)
  "Create a new zettelkasten.

If PREFIX is used, does not create a parent."
  (interactive "P")
  (let ((title (read-string "Note title: "))
        (parent (unless prefix (completing-read "Parent note: "
                                 (zettelkasten-list-notes) nil 'match))))
    (zettelkasten-create-new-note-non-interactive title parent)))

(defun zettelkasten-insert-link (note)
  "Insert a link to another NOTE in the current note."
  (interactive
   (list (completing-read "Notes: "
                          (zettelkasten-list-notes) nil 'match)))
  (insert (zettelkasten-format-link note)))

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
  :keymap zettelkasten-minor-mode-map
  :global t)

(provide 'zettelkasten)

;;; zettelkasten.el ends here
