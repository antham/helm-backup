;;; env.el --- Environment use with ecukes

;; Copyright (C) 2013 Anthony HAMON

;; Author: Anthony HAMON <hamon.anth@gmail.com>
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

;;; Commentary:

;;; Code:

(require 'f)

(defvar helm-backup-support-path (f-dirname load-file-name))

(defvar helm-backup-features-path (f-parent helm-backup-support-path))

(defvar helm-backup-root-path (f-parent helm-backup-features-path))

(defvar helm-backup-folder-repository-original-path nil)

(defvar helm-backup-folder-test "/tmp/helm-backup-test")

(defvar helm-backup-folder-test-repository "/tmp/helm-backup-test/helm-backup")

(add-to-list 'load-path helm-backup-root-path)

(require 'helm-backup)
(require 'espuds)
(require 'ert)

(Setup
 (setq helm-backup-folder-repository-original-path helm-backup-path)
 (setq helm-backup-path helm-backup-folder-test-repository)
 (add-hook 'after-save-hook 'helm-backup-versioning)
 (make-directory helm-backup-folder-test t)
 (ignore-errors
   (delete-directory helm-backup-folder-test t)))

(Before
 (setq helm-mode-line-string
       "\<helm-map>\[helm-help]:Help \[helm-select-action]:Act \[helm-exit-minibuffer]/\[helm-select-2nd-action-or-end-of-line]/\[helm-select-3rd-action]:NthAct")
 (ignore-errors
   (make-directory helm-backup-folder-test t)))

(After
 (ignore-errors
   (delete-directory helm-backup-folder-test t)))

(Teardown
 (setq helm-backup-path helm-backup-folder-repository-original-path))

(provide 'env)

;;; env.el ends here
