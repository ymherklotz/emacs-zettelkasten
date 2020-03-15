# Zettelkasten mode for Emacs

[Zettelkasten](https://zettelkasten.de/) is a note-taking technique designed to keep, and create new links between all the notes as they are written. This allows them to develop over time, link to various different topics and allow the notes to grow into a network over time. This helps draw connections between different fields.

This emacs mode is meant to allow for a very simple wrapper over linked text files. By default, `org` files are used, which are linked through simple file links. The name of the file that is created is just a unique ID.

Each file can then link to other files and they can easily be browsed through in emacs.

## How to use Zettelkasten

To use Zettelkasten, first create a directory which will contain all your notes. This will be a flat directory, as tags are used to place notes into specific categories.

``` shell
mkdir ~/zettelkasten
```

Then, you can activate the mode as follows.

```emacs-lisp
(add-to-list 'load-path "/path/to/zettelkasten")
(require 'zettelkasten)
(zettelkasten-mode t)
```

### Creating new notes

A new note can be created using

``` text
zettelkasten-create-new-note
```

which is bound to `C-c k n` by default.

### Linking to a note

To link to a note from the current note, use the following command:

``` text
zettelkasten-insert-link
```

which will open a list of available notes which you can choose to link to.

## Alternatives

An alternative to use Zettelkasten in emacs is [Zetteldeft](https://github.com/EFLS/zetteldeft), which uses Deft as a backend to search files.
