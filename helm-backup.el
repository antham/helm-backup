;;; helm-backup.el --- Backup each files changes

;; Copyright (C) 2013 Anthony HAMON

;; Author: Anthony HAMON
;; URL: http://github.com/antham/helm-backup

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl)
(require 'helm)
(require 'helm-utils)

(defgroup helm-backup nil
  "Backup system using helm."
  :group 'helm)

(defcustom helm-backup-path "~/.helm-backup"
  "Default path to save backup"
  :group 'helm-backup
  :type 'string)

(defcustom helm-backup-git-binary "/usr/bin/git"
  "Git binary path"
  :group 'helm-backup
  :type 'string)

(defun init-git-repository ()
  "Initialize git repository"
  (unless (file-directory-p helm-backup-path)
    (call-process-shell-command helm-backup-git-binary nil nil nil "init" helm-backup-path)
    )
  )

(defun exec-git-command (command)
  "Execute a git command inside backup repository"
  (when (file-directory-p (concat helm-backup-path "/.git"))
    (type-of helm-backup-path)
    (shell-command (combine-and-quote-strings (append (list "cd" helm-backup-path "&&" "git") command)))
    )
  )

(defun copy-file-to-repository (filename)
  "Create folder in repository and copy file in it"
  (let ((directory (concat helm-backup-path (file-name-directory filename))))
    (make-directory directory t)
    (copy-file filename directory t t t)
    )
  )

(provide 'helm-backup)
