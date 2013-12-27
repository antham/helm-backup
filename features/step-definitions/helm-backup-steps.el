;;; helm-backup-steps.el --- Steps used with ecukes

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

(Given "^I open file \"\\(.+\\)\"$" "Open given filename"
       (lambda (filename)
         (find-file filename)))

(Then "^I insert \"\\(.+\\)\" and save" "Insert given content and save"
      (lambda (data)
        (insert data)
        (save-buffer)))

(Then "^I should be in buffer matching regexp \"\\(.+\\)\"$" "Match REGEXP against buffer-name"
      (lambda (expected)
        (let ((message "Expected to be in buffer '%s', but was in '%s'"))
          (string-match expected (buffer-name)))))

(When "^I run \"\\(.+\\)\"$"
      "Run an emacs command"
      (lambda (command)
        (execute-extended-command command)))

(provide 'helm-backup-steps)

;;; helm-backup-steps.el ends here
