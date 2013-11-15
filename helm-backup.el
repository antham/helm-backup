;;; helm-backup.el --- Backup each file change using git

;; Copyright (C) 2013 Anthony HAMON

;; Author: Anthony HAMON <hamon.anth@gmail.com>
;; URL: http://github.com/antham/helm-backup
;; Version: 0.1.0
;; Package-Requires: ((helm "1.5.5") (s "1.8.0"))
;; Keywords: backup

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

;;; Commentary:

;; To store change every time you save a file add :
;; (add-hook 'after-save-hook 'helm-backup-versioning)
;; or from emacs you can do :
;; M-x customize-variable > after-save-hook > [INS] helm-backup-versioning

;; To retrieve file backup, from buffer call `helm-backup' :
;; M-x helm-backup
;; for convenience you can define key binding as follow :
;; (global-set-key (kbd "C-c b") 'helm-backup)

;; Code:

(require 'helm)
(require 'helm-utils)
(require 's)

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

(defcustom helm-backup-git-log-format "%cd, %ar"
  "Format use to display entries in helm buffer"
  :group 'helm-backup
  :type 'string)

(defun helm-backup-init-git-repository ()
  "Initialize git repository"
  (unless (file-directory-p helm-backup-path)
    (call-process-shell-command helm-backup-git-binary nil nil nil "init" helm-backup-path)
    (helm-backup-exec-git-command (list "config" "--local" "user.email" "noemail@noemail.com"))
    (helm-backup-exec-git-command (list "config" "--local" "user.name" "noname"))
    )
  )

(defun helm-backup-exec-git-command (command &optional strip-last-newline)
  "Execute a git command inside backup repository"
  (when (file-directory-p (concat helm-backup-path "/.git"))
    (let ((output (shell-command-to-string (combine-and-quote-strings (append (list "cd" helm-backup-path "&&" "git") command)))))
      (if strip-last-newline
          (s-chomp output)
        output
        )
      )
    )
  )

(defun helm-backup-transform-filename-for-git (filename)
  "Transform filename to be used in git repository"
  (when (and filename (helm-backup-is-absolute-filename filename))
    (substring filename 1)
    )
  )

(defun helm-backup-is-absolute-filename (filename)
  "Check if a filename is absolute or not"
  (when (and filename (string= (substring filename 0 1) "/"))
    t)
  )

(defun helm-backup-copy-file-to-repository (filename)
  "Create folder in repository and copy file in it"
  (let ((directory (concat helm-backup-path (file-name-directory filename))))
    (make-directory directory t)
    (copy-file filename directory t t t)
    )
  )

(defun helm-backup-version-file (filename)
  "Version file in backup repository"
  (when (and filename (helm-backup-is-absolute-filename filename) (file-exists-p filename))
    (helm-backup-init-git-repository)
    (helm-backup-copy-file-to-repository filename)
    (helm-backup-exec-git-command (list "add" (helm-backup-transform-filename-for-git filename)) t)
    (helm-backup-exec-git-command '("commit" "-m" "' '") t)
    t
    )
  )

(defun helm-backup-list-file-change-time (filename)
  "Build assoc list using commit id and message rendering format"
  (let ((filename-for-git (helm-backup-transform-filename-for-git filename)))
    (when (and filename (string= (helm-backup-exec-git-command (list "ls-files" filename-for-git) t) filename-for-git) t)
      (mapcar*
       'cons
       (split-string (helm-backup-exec-git-command (list "log" (format "--pretty=format:%s" helm-backup-git-log-format) filename-for-git) t) "\n")
       (split-string (helm-backup-exec-git-command (list "log" "--pretty=format:%h" filename-for-git) t) "\n")
       )
      )
    )
  )

(defun helm-backup-fetch-backup-file (commit-id filename)
  "Retrieve content file from backup repository"
  (let ((filename-for-git (helm-backup-transform-filename-for-git filename)))
    (when (and commit-id filename (not (string= (helm-backup-exec-git-command (list "log" "--ignore-missing" "-1" commit-id "--" filename-for-git) t) "")))
      (helm-backup-exec-git-command (list "show" (concat commit-id ":" filename-for-git)))
      )
    )
  )

;;;###autoload
(defun helm-backup-versioning ()
  "Helper to add easily versionning"
  (helm-backup-version-file (buffer-file-name))
  )

(defun helm-backup-open-in-new-buffer (commit-id filename)
  "Open backup in new buffer"
  (let ((buffer-name (concat filename " | " (helm-backup-exec-git-command (list "diff-tree" "-s" "--pretty=format:%cd" commit-id) t)))
        (mode (with-current-buffer (current-buffer) major-mode))
        )
    (set-buffer (generate-new-buffer buffer-name))
    (insert (helm-backup-fetch-backup-file commit-id filename))
    (switch-to-buffer buffer-name)
    (funcall mode)
    )
  )

(defun helm-backup-replace-current-buffer (commit-id filename)
  "Replace current buffer with backup"
  (erase-buffer)
  (insert (helm-backup-fetch-backup-file commit-id filename))
  )

;;;###autoload
(defun helm-backup-source ()
  "Source used to populate buffer"
  `((name . ,((lambda ()
                (format "Backup for %s" (buffer-file-name)))))
    (candidates . ,((lambda ()
                      (helm-backup-list-file-change-time (buffer-file-name)))))
    (action
     ("Open in new buffer" . (lambda (candidate)
                               (helm-backup-open-in-new-buffer candidate (buffer-file-name))))
     ("Replace current buffer" . (lambda (candidate)
                                   (with-helm-current-buffer
                                     (helm-backup-replace-current-buffer candidate (buffer-file-name)))))
     )))

;;;###autoload
(defun helm-backup ()
  "Main function used to call helm-backup"
  (interactive)
  (let ((helm-quit-if-no-candidate (lambda ()
                                     (error "No filename associated with buffer or file has no backup yet")
                                     )))
    (helm-other-buffer (helm-backup-source) "*Helm Backup*")
    )
  )

(provide 'helm-backup)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-backup.el ends here
