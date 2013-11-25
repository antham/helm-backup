(require 'f)

(defvar helm-backup-support-path
  (f-dirname load-file-name))

(defvar helm-backup-features-path
  (f-parent helm-backup-support-path))

(defvar helm-backup-root-path
  (f-parent helm-backup-features-path))

(defvar helm-backup-folder-repository-original-path
  nil)

(defvar helm-backup-folder-test
  "/tmp/helm-backup-test")

(defvar helm-backup-folder-test-repository
  "/tmp/helm-backup-test/helm-backup")

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
   (delete-directory helm-backup-folder-test t)
   )
 )

(Before
 (setq helm-mode-line-string "\<helm-map>\[helm-help]:Help \[helm-select-action]:Act \[helm-exit-minibuffer]/\[helm-select-2nd-action-or-end-of-line]/\[helm-select-3rd-action]:NthAct")
 (ignore-errors
   (make-directory helm-backup-folder-test t)
   )
 )

(After
 (ignore-errors
   (delete-directory helm-backup-folder-test t)
   )
 )

(Teardown
 (setq helm-backup-path helm-backup-folder-repository-original-path)
 )
