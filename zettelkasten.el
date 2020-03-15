;;; Zettelkasten --- Helper functions to organise notes.
;;; Commentary:
;;;
;;; Used to organise notes using the Zettelkasten method.
;;;
;;; Copyright (C) 2020  Yann Herklotz
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
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

(defcustom zettelkasten-directory "~/Dropbox/org/zettelkasten"
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
  (expand-file-name (concat zettelkasten-directory "/"
                            note "." zettelkasten-extension)))

(defun zettelkasten-filename-to-id (filename)
  "Convert FILENAME to id."
  (string-match (format "\\([0-9]*\\)\\.%s\\'" zettelkasten-extension) filename)
  (match-string 1 filename))

(defun zettelkasten-note-regexp (note regexp &optional num)
  "Return the REGEXP first match in the NOTE.

Return the NUMth match.  If NUM is nil, return the 0th match."
  (with-temp-buffer
    (insert-file-contents-literally
     (zettelkasten-make-filename note))
    (when (re-search-forward regexp nil t)
      (match-string (if num num 0)))))

(defun zettelkasten-display-id-title (note)
  "Dispaly the NOTE's title and id."
  (format "%s: %s" note (zettelkasten-note-regexp
                         note "#\\+TITLE: \\(.*\\)" 1)))

(defun zettelkasten-match-link (current note)
  "Return t if the link to CURRENT is in NOTE."
  (when (zettelkasten-note-regexp
         note
         (let ((zk-link-format (replace-regexp-in-string
                                "\\\\\\$" "$"
                                (regexp-quote zettelkasten-link-format))))
           (format zk-link-format
                   ".*" current
                   zettelkasten-extension)))
    (zettelkasten-display-id-title note)))

(defun zettelkasten-list-notes-by-id ()
  "Return all the ids that are currently available."
  (mapcar 'zettelkasten-filename-to-id
          (directory-files
           (expand-file-name zettelkasten-directory) nil
           (format "\\.%s$" zettelkasten-extension) t)))

(defun zettelkasten-list-notes-grep ()
  "Return all the ids and titles of notes in the `zettelkasten-directory'."
  (shell-command (concat "grep -i \"#+TITLE:\" " zettelkasten-directory "/*"))
  (let (match-list morelines current-string matched-string)
    (with-current-buffer "*Shell Command Output*"
      (set-buffer "*Shell Command Output*")
      (setq morelines t)
      (goto-char 1)
      (while morelines
        (setq current-string
              (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position)))
        (when (string-match
               (format "\\([0-9]*\\)\\.%s:#\\+TITLE: \\(.*\\)"
                       zettelkasten-extension)
               current-string)
          (setq matched-string (concat (match-string 1 current-string) ": "
                                       (match-string 2 current-string)))
          (setq match-list (append match-list (list matched-string))))
        (setq morelines (= 0 (forward-line 1)))))
    (kill-buffer "*Shell Command Output*")
    match-list))

(defun zettelkasten-list-notes ()
  "Return all the ids and titles of notes in the `zettelkasten-directory'."
  (mapcar 'zettelkasten-display-id-title
          (zettelkasten-list-notes-by-id)))

(defun zettelkasten-find-new-note-name (note)
  "Iterate on ITERATION until a usable file based on NOTE is found."
  (let ((expanded-name-it
         (lambda (i) ))
        (iteration 0)
        (maxcount 10000))
    (while (and (< iteration maxcount)
                (file-exists-p
                 (zettelkasten-make-filename (format note iteration))))
      (setq iteration (+ iteration 1)))
    (format note iteration)))

(defun zettelkasten-generate-note-name ()
  "Create the new note name."
  (zettelkasten-find-new-note-name
   (format-time-string zettelkasten-file-format)))

(defun zettelkasten-get-id (note)
  "Return the id for a NOTE."
  (string-match "\\([0-9]*\\)" note)
  (match-string 1 note))

(defun zettelkasten-format-link (note)
  "Format a link to a NOTE."
  (format zettelkasten-link-format
          note
          (zettelkasten-get-id note)
          zettelkasten-extension))

(defun zettelkasten-add-link-to-parent (note parent)
  "Add a link to NOTE from a PARENT."
  (with-temp-file (zettelkasten-make-filename parent)
    (insert-file-contents-literally (zettelkasten-make-filename parent))
    (goto-char (point-max))
    (insert (concat "\n" (zettelkasten-format-link note)))))

(defun zettelkasten-create-new-note-ni (title &optional parent)
  "Create a new note based on the TITLE and it's optional PARENT note.

If PARENT is nil, it will not add a link from a PARENT."
  (let ((note (zettelkasten-generate-note-name)))
    (when parent
      (zettelkasten-add-link-to-parent note (zettelkasten-get-id parent)))
    (find-file (zettelkasten-make-filename note))
    (insert (concat "#+TITLE: " title
                    (format-time-string "\n#+DATE: %c\n#+TAGS:\n\n")))
    (save-buffer)))

(defun zettelkasten-create-new-note (prefix)
  "Create a new zettelkasten.

If PREFIX is used, or if the `zettelkasten-directory' is empty,
does not create a parent.

Also see `zettelkasten-create-new-note-ni' for more information."
  (interactive "P")
  (let ((title (read-string "Note title: "))
        (notes (zettelkasten-list-notes)))
    (zettelkasten-create-new-note-ni
     title
     (unless (or prefix (not notes))
       (completing-read "Parent note: " notes nil 'match)))))

(defun zettelkasten-insert-link (note)
  "Insert a link to another NOTE in the current note."
  (interactive
   (list (completing-read "Notes: "
                          (zettelkasten-list-notes) nil 'match)))
  (insert (zettelkasten-format-link note)))

(defun zettelkasten-open-parent (&optional note)
  "Find the parent notes to the NOTE that is given.

The format of the NOTE is anything that can be ready by
  `zettelkasten-get-id'."
  (interactive)
  (let* ((act-note
          (if note (zettelkasten-get-id note)
            (zettelkasten-filename-to-id (buffer-file-name))))
         (selected (completing-read
                    "Notes: "
                    (delete
                     nil
                     (mapcar #'(lambda (el) (zettelkasten-match-link
                                             act-note el))
                             (zettelkasten-list-notes-by-id)))
                    nil 'match)))
    (find-file (zettelkasten-make-filename (zettelkasten-get-id selected)))))

(defvar zettelkasten-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'zettelkasten-insert-link)
    (define-key map "n" 'zettelkasten-create-new-note)
    (define-key map "p" 'zettelkasten-open-parent)
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
