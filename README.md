# Zettelkasten mode for Emacs

| Package | Melpa |
|---|---|
| `zettelkasten` | [![MELPA](https://melpa.org/packages/zettelkasten-badge.svg)](https://melpa.org/#/zettelkasten) |
| `org-zettelkasten` | [![MELPA](https://melpa.org/packages/org-zettelkasten-badge.svg)](https://melpa.org/#/org-zettelkasten) |

[Zettelkasten](https://zettelkasten.de/) is a note-taking technique designed to keep, and create new
links between all the notes as they are written. This allows them to develop over time, link to
various different topics and allow the notes to grow into a network over time. This helps draw
connections between different fields.

This emacs mode is meant to allow for a very simple wrapper over linked text files. By default,
`org` files are used, which are linked through simple file links. The name of the file that is
created is just a unique ID.

Each file can then link to other files and they can easily be browsed through in emacs.

This mode is completely standalone, it does not require any other tools so is easy to install, use
and edit appropriately.

## `org-zettelkasten` and `zettelkasten`

This repository contains two packages which are also on Melpa, and are separate from each other,
giving two different ways to use the Zettelkasten method in Emacs.  One (`org-zettelkasten`)
leverages emacs' [`org-mode`](https://orgmode.org/), and the other (`zettelkasten`) is an
implementation from scratch, which can either use `org-mode` files or markdown files as a base.

### Tag search

Tag search can be implemented using your favourite completion framework.  These will use the `ID` at
the current heading and will look for any other notes that reference this heading (i.e. it will find
all the back links of the current heading).

#### Tag search with counsel

``` emacs-lisp
(defun org-zettelkasten-search-current-id ()
    "Use `counsel-rg' to search for the current ID in all files."
    (interactive)
    (let ((current-id (org-entry-get nil "CUSTOM_ID")))
      (counsel-rg (concat "#" current-id) org-zettelkasten-directory "-g *.org" "ID: ")))
```

#### Tag search with consult

``` emacs-lisp
(defun org-zettelkasten-search-current-id ()
  (interactive)
  (let ((current-id (org-entry-get nil "CUSTOM_ID")))
    (consult-ripgrep org-zettelkasten-directory (concat "[\\[:]." current-id "\\]#"))))
```

#### Add search to keymap

``` emacs-lisp
(define-key org-zettelkasten-mode-map (kbd "s") #'org-zettelkasten-search-current-id)
```

## How to use `org-zettelkasten`

The method implemented in `org-zettelkasten` has been described in detail in a [blog
article](https://yannherklotz.com/blog/2020-12-21-introduction-to-luhmanns-zettelkasten.html).  It
leverages `org-mode` features such as `CUSTOM_ID`,

**Manual Installation**

``` emacs-lisp
(add-to-list 'load-path "/path/to/org-zettelkasten.el")
(require 'org-zettelkasten)
(add-hook 'org-mode-hook #'org-zettelkasten-mode)
```

**`use-package` from Melpa**

``` emacs-lisp
(use-package org-zettelkasten
  :require t
  :config
  (add-hook 'org-mode-hook #'org-zettelkasten-mode))
```

## How to use `zettelkasten`

To use Zettelkasten, first create a directory which will contain all your notes. This will be a flat
directory, as tags are used to place notes into specific categories.

``` shell
mkdir ~/zettelkasten
```

Then, you can activate the mode as follows:

**Manual Installation**

```emacs-lisp
(add-to-list 'load-path "/path/to/zettelkasten.el")
(require 'zettelkasten)
(zettelkasten-mode t)
```

**`use-package` from Melpa**

``` emacs-lisp
(use-package zettelkasten
  :require t
  :config
  (zettelkasten-mode t))
```

### Creating new notes

A new note can be created using

``` text
M-x zettelkasten-create-new-note
```

### Linking to a note

To link to a note from the current note, use the following command:

``` text
M-x zettelkasten-insert-link
```

which will open a list of available notes which you can choose to link to.

### Opening a parent note

To open a parent note of the current note, the following command can be used:

``` text
M-x zettelkasten-find-parent
```

This opens the chosen parent note from a list of available notes. This is bound to `C-c k p` by
default.

### Default bindings

The default keymap for the mode is `C-c k`, this can easily be changed though by editing
`zettelkasten-prefix`.

| Function | Key | Description |
|---|---|---|
| `zettelkasten-create-new-note` | `n` | Create a new note and optionally link it to a parent. This can be disabled by using a prefix argument. |
| `zettelkasten-insert-link` | `i` | Insert a link to a note. |
| `zettelkasten-find-parent` | `p` | Choose from a list of parents of the current note and open the note. |
| `zettelkasten-open-note` | `o` | Open a note from anywhere, using auto complete on the ID or TITLE of the note. |
| `zettelkasten-open-note-by-tag` | `t` | Open a note using a tag as the first identifier. |

## Alternatives

An alternative to use Zettelkasten in emacs is [Zetteldeft](https://github.com/EFLS/zetteldeft),
which uses Deft as a backend to search files.

Another beefier alternative is [org-roam](https://github.com/jethrokuan/org-roam/), which is a fully
integrated note taking system based on a wiki-system.

Finally, [org-brain](https://github.com/Kungsgeten/org-brain) is a similar note-taking system that
is meant for concept mapping in Emacs.
