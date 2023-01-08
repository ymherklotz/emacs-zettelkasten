# Zettelkasten mode for Emacs

[![melpazoid](https://github.com/ymherklotz/emacs-zettelkasten/actions/workflows/melpazoid.yml/badge.svg)](https://github.com/ymherklotz/emacs-zettelkasten/actions/workflows/melpazoid.yml)
[![MELPA](https://melpa.org/packages/zettelkasten-badge.svg)](https://melpa.org/#/zettelkasten)

| **Important:** The `org-zettelkasten` package has now moved to [sourcehut](https://sr.ht/~ymherklotz/org-zettelkasten).  The packages have been separated as they do not share any code anymore. |
|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|

[Zettelkasten](https://zettelkasten.de/) is a note-taking technique designed to keep, and create new
links between all the notes as they are written. This allows them to develop over time, link to
various different topics and allow the notes to grow into a network over time. This helps draw
connections between different fields.

The idea of this mode is to integrate fully into Emacs, trying to leverage most of its preexisting
features.  This package contains `zettelkasten` which is a minimal implementation of existing
Zettelkasten modes. `org-zettelkasten` is another implementation which is hosted on
[sourcehut](https://sr.ht/~ymherklotz/org-zettelkasten) and used to be included in this repository,
it provides helper functions to turn standard [Org](https://orgmode.org/) into a Zettelkasten.

I am currently actively using `org-zettelkasten`, but will still be maintaining `zettelkasten` too.

## `org-zettelkasten` and `zettelkasten`

This repository contains two packages which are also on Melpa, and are separate from each other,
giving two different ways to use the Zettelkasten method in Emacs.  One (`org-zettelkasten`)
leverages emacs' [`org-mode`](https://orgmode.org/), and the other (`zettelkasten`) is an
implementation from scratch, which can either use `org-mode` files or markdown files as a base.

## How to use `zettelkasten`

To use Zettelkasten, first create a directory which will contain all your notes. This will be a flat
directory, as tags are used to place notes into specific categories.

``` shell
mkdir ~/zettelkasten
```

Then, you can activate the mode as follows:

**Manual Installation**

```emacs-lisp
(add-to-list 'load-path "/path/to/emacs-zettelkasten")
(require 'zettelkasten)
(zettelkasten-mode t)
```

**`use-package` from Melpa**

``` emacs-lisp
(use-package zettelkasten
  :ensure t
  :config
  (zettelkasten-mode t))
```

### Creating new notes

A new note can be created using:

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

| Function                        | Key | Description                                                                                            |
|---------------------------------|-----|--------------------------------------------------------------------------------------------------------|
| `zettelkasten-create-new-note`  | `n` | Create a new note and optionally link it to a parent. This can be disabled by using a prefix argument. |
| `zettelkasten-insert-link`      | `i` | Insert a link to a note.                                                                               |
| `zettelkasten-find-parent`      | `p` | Choose from a list of parents of the current note and open the note.                                   |
| `zettelkasten-open-note`        | `o` | Open a note from anywhere, using auto complete on the ID or TITLE of the note.                         |
| `zettelkasten-open-note-by-tag` | `t` | Open a note using a tag as the first identifier.                                                       |

## Alternatives

An alternative to use Zettelkasten in emacs is [Zetteldeft](https://github.com/EFLS/zetteldeft),
which uses Deft as a backend to search files.

Another beefier alternative is [org-roam](https://github.com/jethrokuan/org-roam/), which is a fully
integrated note taking system based on a wiki-system.

Finally, [org-brain](https://github.com/Kungsgeten/org-brain) is a similar note-taking system that
is meant for concept mapping in Emacs.
