# Helm-backup [![Build Status](https://travis-ci.org/antham/helm-backup.png?branch=master)](https://travis-ci.org/antham/helm-backup) [![MELPA](http://melpa.org/packages/helm-backup-badge.svg)](http://melpa.org/#/helm-backup)

This project aims to create a backup system for emacs using both power of git to store changes and power of helm to fetch backups easily.

![](http://antham.github.io/helm-backup/pictures/helm-backup.gif)

## Features

- Diff view to revert changes easily
- Open backup in buffer or replace current one through helm
- Exclude files/folders from backup
- Customize storage path

## Getting started

### Dependencies

* git (>= 1.5) (binary)
* helm (emacs library)
* s (emacs library)

### Install

You can use melpa:

    M-x package-install RET helm-backup

or you can pull it from here and you have to add in your emacs config :

```elisp
(add-to-list 'load-path "/path/to/helm-backup-directory")
(require 'helm-backup)
```

If you want to store every change each time you save a file add :

```elisp
(add-hook 'after-save-hook 'helm-backup-versioning)
```

or

    M-x customize-variable RET after-save-hook RET [INS] helm-backup-versioning

### Usage

You can map `helm-backup` command to key to retrieve easily backup as follow :

```elisp
(global-set-key (kbd "C-c b")   'helm-backup)
```

After that, open a file, press `C-c b` and a list of backup will be displayed if any, you can choose to see backup in new buffer (default) or override current buffer with backup.
