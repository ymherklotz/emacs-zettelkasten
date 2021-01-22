;;; org-zettelkasten.el --- Helper functions to use Zettelkasten in Org.

;; Author: Yann Herklotz
;; Keywords: notes
;; Package-Requires: ((emacs "24.1"))
;; Version: 1.0.0

;;; Commentary:

;; These functions allow for the use of the zettelkasten method in org-mode.
;;
;; It uses the CUSTOM_ID property to store a permanent ID to the note,
;; which are organised in the same fashion as the notes by Luhmann.
;;
;; Copyright (C) 2020  Yann Herklotz
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'org)
(require 'counsel)

(defgroup org-zettelkasten nil
  "Helper to work with zettelkasten notes."
  :group 'applications)

(defcustom org-zettelkasten-directory "~/Dropbox/zk"
  "Main zettelkasten directory."
  :type 'string
  :group 'org-zettelkasten)

(defun ymhg/incr-id (ident)
  "Simple function to increment any IDENT.

This might result in duplicate IDs though."
  (let* ((ident-list (append nil ident nil))
         (last-ident (last ident-list)))
    (setcar last-ident (+ (car last-ident) 1))
    (concat ident-list)))

(defun ymhg/incr-id-total (ident)
  "A better way to incement numerical IDENT.

This might still result in duplicate IDENTs for an IDENT that
ends with a letter."
  (if (string-match-p "\\(.*[a-z]\\)\\([0-9]+\\)$" ident)
      (progn
        (string-match "\\(.*[a-z]\\)\\([0-9]+\\)$" ident)
        (let ((pre (match-string 1 ident))
              (post (match-string 2 ident)))
          (concat pre (number-to-string (+ 1 (string-to-number post))))))
    (ymhg/incr-id ident)))

(defun ymhg/branch-id (ident)
  "Create a branch ID from IDENT."
  (if (string-match-p ".*[0-9]$" ident)
      (concat ident "a")
    (concat ident "1")))

(defun ymhg/org-zettelkasten-create (incr newheading)
  "Creat a new heading according to INCR and NEWHEADING.

INCR: function to increment the ID by.
NEWHEADING: function used to create the heading and set the current
            POINT to it."
  (let* ((current-id (org-entry-get nil "CUSTOM_ID"))
         (next-id (funcall incr current-id)))
    (funcall newheading)
    (org-set-property "CUSTOM_ID" next-id)))

(defun org-zettelkasten-create-next ()
  "Create a heading at the same level as the current one."
  (ymhg/org-zettelkasten-create
   'ymhg/incr-id 'org-insert-heading))

(defun org-zettelkasten-create-branch ()
  "Create a branching heading at a level lower than the current."
  (ymhg/org-zettelkasten-create
   'ymhg/branch-id '(lambda () (org-insert-subheading ""))))

(defun org-zettelkasten-create-dwim ()
  "Create the right type of heading based on current position."
  (interactive)
  (let ((current-point (save-excursion
                         (org-back-to-heading)
                         (point)))
        (next-point (save-excursion
                      (org-forward-heading-same-level 1 t)
                      (point))))
    (if (= current-point next-point)
        (org-zettelkasten-create-next)
      (org-zettelkasten-create-branch))))

(defun org-zettelkasten-search-current-id ()
  "Use counsel-rg to search for the current ID in all files."
  (interactive)
  (let ((current-id (org-entry-get nil "CUSTOM_ID")))
    (counsel-rg (concat "#" current-id) org-zettelkasten-directory "-g *.org" "ID: ")))

(define-key org-mode-map (kbd "C-c y n") #'org-zettelkasten-create-dwim)
(define-key org-mode-map (kbd "C-c y s") #'org-zettelkasten-search-current-id)

(provide 'org-zettelkasten)
;;; org-zettelkasten.el ends here
