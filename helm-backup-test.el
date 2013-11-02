;;; helm-backup-test.el --- Test for helm-backup

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

(require 'ert)
(require 'helm-backup)

(defmacro test-wrapper (body)
  `(let ((path helm-backup-path))
     (unwind-protect
         (progn
           (ignore-errors
             (setq helm-backup-path "/tmp/helm-backup-fake-repository")
             (delete-directory "/tmp/helm-backup-fake-repository" t)
             (delete-file "/tmp/fake-file")
             (delete-file "/tmp/fake-file-1"))
           (,body))
       (ignore-errors
         (delete-directory "/tmp/helm-backup-fake-repository" t)
         (delete-file "/tmp/fake-file")
         (delete-file "/tmp/fake-file-1"))
       (setq helm-backup-path path))
     )
  )

(ert-deftest helm-backup-init-git-repository-test ()
  (test-wrapper 
   (lambda ()
     ;; invoking command create a git repository
     (helm-backup-init-git-repository)
     (should (eql (file-exists-p helm-backup-path) t))
     (should (eql (file-exists-p (concat helm-backup-path "/.git")) t))
     (delete-directory helm-backup-path t)
     ;; if directory exists nothing is done
     (make-directory helm-backup-path)
     (helm-backup-init-git-repository)
     (should (eql (file-exists-p (concat helm-backup-path "/.git")) nil))
     )
   )
  )


(ert-deftest helm-backup-transform-filename-for-git-test ()
  (test-wrapper
   (lambda ()
     ;; nil filename
     (should (eql (helm-backup-transform-filename-for-git nil) nil))
     ;; relative filename
     (should (eql (helm-backup-transform-filename-for-git "relative/path") nil))
     ;; absolute filename
     (should (equal-including-properties (helm-backup-transform-filename-for-git "/absolute/path") "absolute/path"))
     )
   )
  )

(ert-deftest helm-backup-is-absolute-filename-test ()
  (test-wrapper
   (lambda ()
     ;; nil filename
     (should (eql (helm-backup-is-absolute-filename nil) nil))
     ;; relative filename
     (should (eql (helm-backup-is-absolute-filename "relative/path") nil))
     ;; absolute filename
     (should (equal-including-properties (helm-backup-is-absolute-filename "/absolute/path") t))
     )
   )
  )

(ert-deftest helm-backup-exec-git-command-test ()
  (test-wrapper 
   (lambda ()
     ;; we can do any git command in backup repository
     (shell-command (combine-and-quote-strings (list "git" "init" helm-backup-path)))
     (write-region "" nil (concat helm-backup-path "/test"))
     (helm-backup-exec-git-command (list "add" "test"))
     (should (equal-including-properties (helm-backup-exec-git-command (list "status" "-s")) "A  test"))
     )
   )
  )

(ert-deftest helm-backup-copy-file-to-repository-test ()
  (test-wrapper 
   (lambda ()
     ;; copy a file to backup repository recreating tree
     (shell-command (combine-and-quote-strings (list "git" "init" helm-backup-path)))
     (write-region "" nil "/tmp/fake-file")
     (helm-backup-copy-file-to-repository "/tmp/fake-file")
     (should (eql (file-exists-p (concat helm-backup-path "/tmp/fake-file")) t))
     )
   )
  )

(ert-deftest helm-backup-version-file-test ()
  (test-wrapper 
   (lambda ()
     (shell-command (combine-and-quote-strings (list "git" "init" helm-backup-path)))
     ;; version file
     (write-region "" nil "/tmp/fake-file")
     (should (eql (helm-backup-version-file "/tmp/fake-file") t))
     (should (eql (file-exists-p (concat helm-backup-path "/tmp/fake-file")) t))
     (should (equal-including-properties (shell-command (combine-and-quote-strings (list "cd" helm-backup-path "&&" "git" "status" "-s"))) 0))
     ;; version file with relative path
     (write-region "" nil "/tmp/fake-file-1")
     (should (eql (helm-backup-version-file "tmp/fake-file-1") nil))
     (should (eql (file-exists-p (concat helm-backup-path "/tmp/fake-file")) t))
     (should (equal-including-properties (shell-command (combine-and-quote-strings (list "cd" helm-backup-path "&&" "git" "status" "-s"))) 0))
     ;; version non existing file
     (should (eql (helm-backup-version-file "/tmp/fake-fake-fake-fake") nil))
     (should-not (eql (file-exists-p (concat helm-backup-path "/tmp/fake-fake-fake-fake")) t))
     ;; version crap
     (should (eql (helm-backup-version-file "fake-fake-fake") nil))
     (should (eql (file-exists-p (concat helm-backup-path "/tmp/fake-fake-fake-fake")) nil))
     )
   )
  )

(ert-deftest helm-backup-list-file-change-time-test ()
  (test-wrapper 
   (lambda ()
     ;; nil value
     (should (eq (helm-backup-list-file-change-time nil) nil))
     ;; non existing repository
     (should (eq (helm-backup-list-file-change-time "/fake-file") nil))
     ;; add several modifications to a file
     (shell-command (combine-and-quote-strings (list "git" "init" helm-backup-path)))
     ;; non existing file in repository
     (should (eq (helm-backup-list-file-change-time "/fake-file") nil))
     ;; add a file, change and version it several time
     (write-region "" nil (concat helm-backup-path "/fake-file"))
     (shell-command (combine-and-quote-strings (list "cd" helm-backup-path "&&" "git" "add" "fake-file" "&&" "git" "commit" "--allow-empty-message" "-m" "''")))
     (write-region "data" nil (concat helm-backup-path "/fake-file"))
     (shell-command (combine-and-quote-strings (list "cd" helm-backup-path "&&" "git" "add" "fake-file" "&&" "git" "commit" "--allow-empty-message" "-m" "''")))
     (write-region "data data" nil (concat helm-backup-path "/fake-file"))
     (shell-command (combine-and-quote-strings (list "cd" helm-backup-path "&&" "git" "add" "fake-file" "&&" "git" "commit" "--allow-empty-message" "-m" "''")))
     (should (eq (safe-length (helm-backup-list-file-change-time "/fake-file")) 3))
     (dolist (row (helm-backup-list-file-change-time "/fake-file"))
       (should (eq (string-match "[0-9a-f]+" (cdr row)) 0))
       )
     )
   )
  )
