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
  `(let ((path helm-backup-path)
         (backup-folder-test (file-truename "/tmp/helm-backup-test"))
         (backup-folder-test-repository (file-truename (concat (file-truename "/tmp/helm-backup-test") "/helm-backup"))))
     (unwind-protect
         (progn
           (ignore-errors
             (delete-directory backup-folder-test t)
             (make-directory backup-folder-test))
           (setq helm-backup-path backup-folder-test-repository)
           (,body))
       (ignore-errors
         (delete-directory backup-folder-test t))
       (setq helm-backup-path path)
       )
     )
  )

(ert-deftest helm-backup-init-git-repository-test ()
  (test-wrapper 
   (lambda ()
     ;; invoking command create a git repository
     (helm-backup-init-git-repository)
     (should (eql (file-exists-p backup-folder-test-repository) t))
     (should (eql (file-exists-p (concat backup-folder-test-repository "/.git")) t))
     (delete-directory backup-folder-test-repository t)
     ;; if directory exists nothing is done
     (make-directory backup-folder-test-repository)
     (helm-backup-init-git-repository)
     (should (eql (file-exists-p (concat backup-folder-test-repository "/.git")) nil))
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
     (call-process-shell-command helm-backup-git-binary nil nil nil "init" backup-folder-test-repository)
     (write-region "" nil (concat backup-folder-test-repository "/test") nil 'nomessage)
     (helm-backup-exec-git-command (list "add" "test"))
     (should (equal-including-properties (helm-backup-exec-git-command (list "status" "-s")) "A  test\n"))
     (should (equal-including-properties (helm-backup-exec-git-command (list "status" "-s") t) "A  test"))
     )
   )
  )

(ert-deftest helm-backup-copy-file-to-repository-test ()
  (test-wrapper 
   (lambda ()
     ;; copy a file to backup repository recreating tree
     (call-process-shell-command helm-backup-git-binary nil nil nil "init" backup-folder-test-repository)
     (write-region "" nil (concat backup-folder-test "/fake-file") nil 'nomessage)
     (helm-backup-copy-file-to-repository (concat backup-folder-test "/fake-file"))
     (should (eql (file-exists-p (concat backup-folder-test-repository (concat backup-folder-test "/fake-file"))) t))
     )
   )
  )

(ert-deftest helm-backup-version-file-test ()
  (test-wrapper 
   (lambda ()
     (call-process-shell-command helm-backup-git-binary nil nil nil "init" backup-folder-test-repository nil nil)
     ;; version file
     (write-region "" nil (concat backup-folder-test "/fake-file") nil 'nomessage)
     (should (eql (helm-backup-version-file (concat backup-folder-test "/fake-file")) t))
     (should (eql (file-exists-p (concat backup-folder-test-repository (concat backup-folder-test "/fake-file"))) t))
     (should (equal-including-properties (shell-command-to-string (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" helm-backup-git-binary "status" "-s"))) ""))
     ;; version file with relative path
     (write-region "" nil (concat backup-folder-test "/fake-file-1") nil 'nomessage)
     (should (eql (helm-backup-version-file (substring (concat backup-folder-test "/fake-file-1") 1)) nil))
     (should (eql (file-exists-p (concat backup-folder-test-repository "/fake-file")) nil))
     (should (equal-including-properties (shell-command-to-string (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" helm-backup-git-binary "status" "-s"))) ""))
     ;; version non existing file
     (should (eql (helm-backup-version-file (concat backup-folder-test "/fake-fake-fake-fake")) nil))
     (should-not (eql (file-exists-p (concat backup-folder-test-repository (concat backup-folder-test "fake-fake-fake-fake"))) t))
     ;; version crap
     (should (eql (helm-backup-version-file (concat backup-folder-test "/fake-fake-fake")) nil))
     (should (eql (file-exists-p (concat backup-folder-test-repository (concat backup-folder-test "/fake-fake-fake-fake"))) nil))
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
     (call-process-shell-command helm-backup-git-binary nil nil nil "init" backup-folder-test-repository)
     ;; non existing file in repository
     (should (eq (helm-backup-list-file-change-time "/fake-file") nil))
     ;; add a file, change and version it several time
     (write-region "" nil (concat backup-folder-test-repository "/fake-file") nil 'nomessage)
     (shell-command (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" helm-backup-git-binary "add" "fake-file" "&&" helm-backup-git-binary "commit" "-m" "' '")))
     (write-region "data" nil (concat backup-folder-test-repository "/fake-file") nil 'nomessage)
     (shell-command (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" helm-backup-git-binary "add" "fake-file" "&&" helm-backup-git-binary "commit" "-m" "' '")))
     (write-region "data data" nil (concat backup-folder-test-repository "/fake-file") nil 'nomessage)
     (shell-command (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" helm-backup-git-binary "add" "fake-file" "&&" helm-backup-git-binary "commit" "-m" "' '")))
     (should (eq (safe-length (helm-backup-list-file-change-time "/fake-file")) 3))
     (dolist (row (helm-backup-list-file-change-time "/fake-file"))
       (should (eq (string-match "[0-9a-f]+" (cdr row)) 0))
       )
     )
   )
  )

(ert-deftest helm-backup-fetch-backup-file-test ()
  (test-wrapper
   (lambda ()
     (call-process-shell-command helm-backup-git-binary nil nil nil "init" backup-folder-test-repository)
     (write-region "data" nil (concat backup-folder-test-repository "/fake-file") nil 'nomessage)
     (shell-command (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" helm-backup-git-binary "add" "fake-file" "&&" helm-backup-git-binary "commit" "-m" "' '")))
     (let ((commit-id (car (split-string (shell-command-to-string (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" helm-backup-git-binary "log" "-1" "--oneline"))) " "))))
       ;; nil value
       (should (eql (helm-backup-fetch-backup-file nil nil) nil))
       ;; wrong id
       (should (eql (helm-backup-fetch-backup-file "9090909" "/fake-file") nil))
       ;; wrong file
       (should (eql (helm-backup-fetch-backup-file commit-id "/non-existing-file") nil))
       ;; existing commit and file
       (should (equal-including-properties (helm-backup-fetch-backup-file commit-id "/fake-file") "data"))
       )
     )
   )
  )
