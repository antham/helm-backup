# Helm-backup [![Build Status](https://travis-ci.org/antham/helm-backup.png?branch=master)](https://travis-ci.org/antham/helm-backup)

This project aims to create a backup system for emacs using both power of git to store changes and power of helm to fetch backups easily.

![alt tag](http://antham.github.io/helm-backup/pictures/screenshot.png)

## Getting started

### Dependencies

* git (>= 1.5) (binary)
* helm (emacs library)
* s (emacs library)

### Install

So you need to get sources and after that in your emacs config add :

  ```elisp
  (add-to-list 'load-path "/path/to/helm-backup-directory")
  (require 'helm-backup)
  ```

If you want to store every change each time you save a file add : 

  ```elisp
  (add-hook 'after-save-hook 'helm-backup-versioning)
  ```
or 

    M-x customize-variable > after-save-hook > [INS] helm-backup-versioning

### Usage

You can map `helm-backup` command to key to retrieve easily backup as follow :

  ```elisp
  (global-set-key (kbd "C-c b")   'helm-backup)
  ```

After that, open a file, press `C-c b` and a list of backup will be displayed if any, you can choose to see backup in new buffer (default) or override current buffer with backup.

### Customize

You can easily customize some stuffs through `M-x customize-group > helm-group`, like storage path for instance.
