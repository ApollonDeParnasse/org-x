;;; org-x.el --- Extensions for org-mode -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Earl Chase
;; Maintainer: Earl Chase
;; Version: 0.0.0
;; Keywords: tools
;; Package-Requires: ((emacs "30.1") (org "9.7") (dash "2.20.0") (s "1.13.1") (compat "29"))
;; Homepage: https://github.com/ApollonDeParnasse/org-x

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Collections of extensions for org-mode.

;;; Code:

(require 'org)
(require 'org-table)
(require 'dash)
(require 's)

(defun org-summary-todo (n-done n-not-done)
  (message "done: %s not done: %s" n-done n-not-done)
  (when (= n-not-done 0)
    (org-todo "DONE")))

(defun org-insert-test-function-todo-headings ()
  (interactive)
  (org-insert-todo-subheading '(4))
  (insert "test")
  (org-insert-todo-heading-respect-content)
  (insert "function"))

(cl-defun org-table-toggle-all-timestamps-in-all-tables-helper (&optional (arg 'active))
  (lambda ()
    (let ((re (org-re-timestamp arg)))
      (dotimes (_ (count-matches re (org-table-begin) (org-table-end)))
	(re-search-forward re (org-table-end))
	(goto-char (1+ (match-beginning 0)))
	(print (org-at-timestamp-p 'lax))
	(org-toggle-timestamp-type)))))


(cl-defun org-table-toggle-all-timestamps-in-all-tables (&optional (arg 'active))
  (interactive)
  (progn
    (org-table-map-tables (org-table-toggle-all-timestamps-in-all-tables-helper arg))
    (save-buffer)))

(defun org--create-org-todo-in-region-command (arg)
  (lambda (beg end)
    (interactive (and (use-region-p) (list (region-beginning) (region-end))))
    (org-map-region (lambda () (org-todo arg)) (region-beginning) (region-end))))

(defalias 'org-mark-todos-in-region-done (org--create-org-todo-in-region-command "DONE"))

(defalias 'org-cancel-todos-in-region (org--create-org-todo-in-region-command "CANCELED"))

(defalias 'org-mark-all-headings-in-region-as-todo (org--create-org-todo-in-region-command "TODO"))

(defun org-x-create-file-for-org-agenda-files (org-directories file-name)
  (let* ((files-list (flatten-list (mapcar (lambda (directory) (directory-files-recursively directory "\.org$"))
  					org-directories)))
	 (string (string-join files-list "\n")))
    (with-temp-buffer
      (insert string)
      (write-file file-name))))

(provide 'org-x)
;;; org-x.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ox-" . "org-x-"))
;; End:
