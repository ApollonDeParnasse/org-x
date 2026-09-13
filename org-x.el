;;; org-x.el --- Extensions for org-mode -*- lexical-binding: t; -*-

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
(require 'org-id)
(require 'org-element)
(require 'org-element-ast)
(require 'thunk)
(require 'treesit)
(require 'dash)
(require 's)


(declare-function org-id-locations-load "org-id")
(declare-function org-toggle-timestamp-type "org")
(declare-function org-element-lineage "org-element-ast" (blob &optional types with-self))
(defvar org-id-track-globally)
(defvar org-element-clock-line-re)

(defalias 'org-x--partial-starts-with (lambda (x) (-partial #'s-starts-with-p x)))
(defalias 'org-x--partial-ends-with  (lambda (x) (-partial #'s-ends-with-p x)))
(defalias 'org-x--partial-equal (lambda (x) (-partial #'equal x)))
(defalias 'org-x--partial-string-equal (lambda (x) (-partial #'string-equal x)))
(defalias 'org-x--partial-member (lambda (x) (-partial #'member x)))
(defalias 'org-x--partial-s-contains (lambda (x) (-partial #'s-contains-p x)))
(defalias 'org-x--partial-negate-s-contains (lambda (x) (-not (-partial #'s-contains-p x))))

(defalias 'org-x--is-directory (lambda (x) (string-equal (file-name-sans-extension x) x)))
(defalias 'org-x--is-gitignore (org-x--partial-equal ".gitignore"))
(defalias 'org-x--is-d-ts-file (org-x--partial-ends-with ".d.ts"))
(defalias 'org-x--is-elc (org-x--partial-ends-with ".elc"))
(defalias 'org-x--is-png (org-x--partial-ends-with "png"))
(defalias 'org-x--is-image (-orfn #'is-png))
(defalias 'org-x--is-Changelog (org-x--partial-string-equal "Changelog.md"))
(defalias 'org-x--is-License (org-x--partial-starts-with "LICENSE"))
(defalias 'org-x--is-dotfile (-orfn (org-x--partial-starts-with ".") (org-x--partial-equal "..")))
(defalias 'org-x--ignored-files (-not (-orfn #'org-x--is-dotfile #'org-x--is-d-ts-file #'org-x--is-gitignore #'org-x--is-elc #'org-x--is-Changelog #'org-x--is-License)))

;;;###autoload
(defun org-x-summary-todo (n-done n-not-done)
  (message "done: %s not done: %s" n-done n-not-done)
  (when (= n-not-done 0)
    (org-todo "DONE")))

(cl-defun org-x--close-all-timestamps-in-all-tables-helper (&optional (arg 'active))
  (lambda ()
    (let ((re (org-re-timestamp arg)))
      (dotimes (_ (count-matches re (org-table-begin) (org-table-end)))
	(re-search-forward re (org-table-end))
	(goto-char (1+ (match-beginning 0)))
	(org-toggle-timestamp-type)))))


(cl-defun org-x-close-all-timestamps-in-all-tables (&optional (arg 'active))
  (interactive)
  (progn
    (org-table-map-tables (org-x--close-all-timestamps-in-all-tables-helper arg))
    (save-buffer)))

(defun org-x--create-org-todo-in-region-command (arg)
  (lambda ()
    (interactive)
    (org-map-region (lambda () (org-todo arg)) (region-beginning) (region-end))))

(defalias 'org-x-cancel-all-todos-in-region (org-x--create-org-todo-in-region-command "CANCELED"))

(defalias 'org-x-mark-all-headlines-in-region-as-todo (org-x--create-org-todo-in-region-command "TODO"))

;;;###autoload
(defun org-x-insert-test-function-todo-headlines ()
  (interactive)
  (org-insert-todo-subheading '(4))
  (insert "test")
  (org-insert-todo-heading "TODO")
  (insert " function"))

;;;###autoload
(defun org-x-insert-update-test-function-todo-headlines ()
  (interactive)
  (org-insert-todo-subheading '(4))
  (insert "update tests")
  (org-insert-todo-heading "TODO")
  (insert " update function")
  (org-insert-todo-heading "TODO")
  (insert " update callers"))

(define-error 'org-x--no-headline "You can convert this text into subheadlines if it is not under another headline.")
(defsubst org-x--next-headline-level (&optional subheadlinep)
  (let* ((previous-headline (org-element-lineage
			     (org-element-at-point)
			     'headline 'with-self))
	 (current-level (org-element-property :level previous-headline)))
    (cond
     ((and subheadlinep (not current-level)) (signal 'org-x--no-headline nil))
     ((and subheadlinep current-level) (1+ current-level))
     ((and (not subheadlinep) current-level) current-level)
     ((and (not subheadlinep) (not current-level)) 1))))

(defalias 'org-x--create-headline-stars (-rpartial #'make-string ?*))

(defun org-x--maybe-recover-file (file)
  (cl-letf (((symbol-function 'read-answer) (lambda (&rest _)  "yes")))
    (ignore-errors (progn (recover-file file) (save-buffer)))))

;;;###autoload
(defun org-x-recover-all-org-files ()
  (interactive)
  (save-window-excursion
    (mapc #'org-x--maybe-recover-file (org-files-list))))

(defun org-x--maybe-revert-org-buffer (buffer)
  "Maybe revert BUFFER.
Buffer is only reverted if hasn't
been changed since its file was
last read or saved and it has
changed on disk."
  (with-current-buffer buffer
    (when (and
	   buffer-file-name
	   (file-exists-p buffer-file-name)
	   (derived-mode-p 'org-mode)
	   (not (buffer-modified-p))
	   (not (verify-visited-file-modtime)))
      (revert-buffer t 'no-confirm)
      (save-buffer))))

;;;###autoload
(defun org-x-revert-some-org-buffers ()
  "Revert some Org buffers.
Buffer is reverted if it has
no unsaved changes and it the
file has changed on disk."
  (interactive)
  (mapc #'org-x--maybe-revert-org-buffer (buffer-list))
  (when (and (featurep 'org-id) org-id-track-globally)
    (org-id-locations-load)))

;;;###autoload
(defun org-x-create-file-for-org-agenda-files (org-directories file-name)
  (let* ((files-list (flatten-list (mapcar (lambda (directory) (directory-files-recursively directory "\.org$"))
  					org-directories)))
	 (string (string-join files-list "\n")))
    (with-temp-buffer
      (insert string)
      (write-file file-name))))

(cl-defun org-x--build-headline-element (title &key
					       level
					       priority
					       tags
					       todo-keyword
					       todo-type)
  (let* ((level-value (or level 1))
	 (stars (org-x--create-headline-stars level-value))
	 (raw-value (format "%s %s" stars title))
	 (props `(:raw-value ,raw-value
		  :title ,title
		  :level ,level-value
		  ,@(when tags
		      `(:tags ,tags))
		  ,@(when todo-keyword
		      `(:todo-keyword ,todo-keyword))
		  ,@(when todo-type
		      `(:todo-type ,todo-type)))))
    (org-element-create
     'headline
     props)))

(cl-defun org-x--build-src-block-element (&key
					  language
					  switches
					  parameters
					  value)
  (let* ((block-value (or value ""))
	 (props `(,@(when language
		     `(:language ,language))
		 ,@(when switches
		       `(:switches ,switches))
		 ,@(when parameters
		       `(:parameters ,parameters))
		 ,@(when value
		     `(:value ,value)))))
    (org-element-create
     'src-block
     props)))

(defalias 'org-x--build-src-block-string (-compose
					  (-rpartial #'org-element-src-block-interpreter nil)
					  #'org-x--build-src-block-element))

(cl-defun org-x--build-headline-string (title &key
					      level
					      priority
					      tags
					      todo-keyword
					      todo-type
					      contents)
  (let* ((headline-element (org-x--build-headline-element
			    title
			    :level level
			    :priority priority
			    :tags tags
			    :todo-keyword todo-keyword
			    :todo-type todo-type)))
    (org-element-headline-interpreter headline-element contents)))

(cl-defun org-x-create-headline-with-src-block (title &key
						      level
						      priority
						      tags
						      todo-keyword
						      todo-type
						      language
						      switches
						      parameters
						      value)
  (let* ((src-block-string (org-x--build-src-block-string
			    :language language
			    :value value
			    :switches switches
			    :parameters parameters)))
    (org-x--build-headline-string
     title
     :level level
     :priority priority
     :tags tags
     :todo-keyword todo-keyword
     :todo-type todo-type
     :contents src-block-string)))

(defun org-x-create-list-of-headlines-with-src-blocks-helper (level lang parameters)
  (-lambda ((title code))
    (org-x-create-headline-with-src-block
     title
     :level level
     :language lang
     :parameters parameters
     :value code)))

(defun org-x-create-list-of-headlines-with-src-blocks (level lang parameters title-code-pairs)
  "Create a headline with a src-block that will contain the provided TEXT.
LEVEL should be the level of the headlines that you would like to create.
LANG should be the language of the code that will be included in your
src-block.  TITLE-CODE-TUPLES should be a list of pairs where the car
should be a title for a headline and the cadr should be a string of
code."
  (mapcar (org-x-create-list-of-headlines-with-src-blocks-helper level lang parameters)
	  title-code-pairs))

(defalias 'org-x--convert-list-into-lines-of-x (-rpartial #'mapconcat "\n"))

(defun org-x--convert-block-of-text-into-lines-of-x (formatter text)
  (->> text
       (s-split "\n")
       (seq-map #'s-trim)
       (seq-filter (lambda (line) (length> line 0)))
       (org-x--convert-list-into-lines-of-x formatter)))

(defun org-x--create-convert-x-into-headlines-function (converter)
  (lambda (keyword &optional subheadlinep)
    (lambda (text)
      (let* ((headline-level (org-x--next-headline-level subheadlinep))
	     (stars (org-x--create-headline-stars headline-level))
	     (formatter (if keyword
			    (lambda (line) (format "%s %s %s" stars keyword line))
			  (lambda (line) (format "%s %s" stars line)))))
	(funcall converter formatter text)))))

(defalias 'org-x--convert-block-of-text-into-headlines
  (org-x--create-convert-x-into-headlines-function
   #'org-x--convert-block-of-text-into-lines-of-x))

(defalias 'org-x--convert-list-into-headlines
  (org-x--create-convert-x-into-headlines-function
   #'org-x--convert-list-into-lines-of-x))

(defun org-x--create-convert-buffer-lines-function (converter)
  (lambda ()
    (interactive)
    (let* ((old-text (buffer-substring-no-properties (region-beginning) (region-end)))
	   (new-text (funcall converter old-text)))
      (delete-region (region-beginning) (region-end))
      (insert new-text))))

(defun org-x--create-convert-buffer-lines-into-headlines-function (keyword &optional subheadlinep)
  (let ((converter (org-x--convert-block-of-text-into-headlines keyword subheadlinep)))
    (org-x--create-convert-buffer-lines-function converter)))

(defalias 'org-x-convert-buffer-lines-into-headlines (org-x--create-convert-buffer-lines-into-headlines-function nil))

(defalias 'org-x-convert-buffer-lines-into-subheadlines (org-x--create-convert-buffer-lines-into-headlines-function nil t))

(defalias 'org-x-convert-buffer-lines-into-todos (org-x--create-convert-buffer-lines-into-headlines-function "TODO"))

(defalias 'org-x-convert-buffer-lines-into-subtodos (org-x--create-convert-buffer-lines-into-headlines-function "TODO" t))

(defun org-x--create-yank-last-kill-as-function (converter)
  (lambda ()
    (interactive)
    (let* ((old-text (current-kill 0))
	   (new-text (funcall converter old-text)))
      (kill-new new-text)
      (yank))))

;;;###autoload
(defun org-x--create-yank-last-kill-as-headlines-function (keyword &optional subheadlinep)
  (let ((converter (org-x--convert-block-of-text-into-headlines keyword subheadlinep)))
    (org-x--create-yank-last-kill-as-function converter)))

(defalias 'org-x-yank-last-kill-as-headlines (org-x--create-yank-last-kill-as-headlines-function nil))

(defalias 'org-x-yank-last-kill-as-subheadlines (org-x--create-yank-last-kill-as-headlines-function nil t))

;;;###autoload
(defalias 'org-x-yank-last-kill-as-todos (org-x--create-yank-last-kill-as-headlines-function "TODO"))

;;;###autoload
(defalias 'org-x-yank-last-kill-as-subtodos (org-x--create-yank-last-kill-as-headlines-function "TODO" t))

(cl-defun org-x--create-yank-last-list-as-headlines-function (keyword &optional subheadlinep (preprocessor #'read))
  (let* ((base-converter (org-x--convert-list-into-headlines keyword subheadlinep))
	(converter (-compose base-converter preprocessor)))
    (org-x--create-yank-last-kill-as-function converter)))

(defalias 'org-x-yank-last-elisp-list-as-headlines (org-x--create-yank-last-list-as-headlines-function nil))

(defalias 'org-x-yank-last-elisp-list-as-subheadlines (org-x--create-yank-last-list-as-headlines-function nil t))

;;;###autoload
(defalias 'org-x-yank-last-elisp-list-as-todos (org-x--create-yank-last-list-as-headlines-function "TODO"))

;;;###autoload
(defalias 'org-x-yank-last-elisp-list-as-subtodos (org-x--create-yank-last-list-as-headlines-function "TODO" t))

(cl-defun org-x--tree-sitter-list-parser (lang text)
  (when (not (treesit-available-p))
    (user-error "You need to use a version of emacs with treesit."))
  (when (not (treesit-language-available-p lang))
    (user-error "You need to install the tree-sitter grammar for %s" lang))
  (with-temp-buffer
    (progn (insert text)
    (goto-char (point-min))
    (treesit-parser-create lang)
    (mapcar #'treesit-node-text
	    (--> (treesit-node-at (point))
		 (treesit-node-parent it)
		 (treesit-node-children it t))))))

(defun org-x--create-yank-last-kill-as-headlines-tree-sitter-function (lang keyword subheadlinep)
  (let* ((preprocessor (-partial #'org-x--tree-sitter-list-parser lang)))
    (org-x--create-yank-last-list-as-headlines-function
     keyword
     subheadlinep
     preprocessor)))

(defun org-x--indent-elisp-code (code)
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert code)
    (indent-region (point-min) (point))
    (buffer-substring-no-properties (point-min) (point))))

(defun org-x--create-text-of-tree-sitter-yank-last-as-x-functions-for-lang (lang)
  (-lambda ((keyword subheadlinep type))
    (let* ((alias-name (format "org-x-yank-last-%s-list-as-%s" lang type))
     	   (docstring-1 (format "Convert the last killed %s list into a list of org headlines." lang))
	   (docstring-2 (format "The resulting list will be added to the kill ring and then yanked at point."))
	   (docstring (concat docstring-1 "\n" docstring-2))
	   (calling-convention "\\(fn)")
	   (func-call (format
		       "(org-x--create-yank-last-kill-as-headlines-tree-sitter-function
		       '%s %s %s)"
		       lang keyword subheadlinep))
	   (alias (format "(defalias '%s\n%s\n\"%s\n\n%s\")"
			  alias-name
			  func-call
			  docstring
			  calling-convention))
	   (indented-alias (org-x--indent-elisp-code alias)))
      (list alias-name indented-alias))))

(cl-defun org-x--create-list-of-tree-sitter-yank-last-as-functions-for-lang (lang &optional (top-headline-level 4))
  (let* ((subheadline-level (1+ top-headline-level))
	 (top-headline (format "%s %s"
			       (org-x--create-headline-stars top-headline-level)
			       lang))
	 (subheadlines-creator (org-x--create-text-of-tree-sitter-yank-last-as-x-functions-for-lang
				lang))
	 (title-code-pairs
	  (mapcar
	   subheadlines-creator
	   (list
	    (list nil nil "headlines")
	    (list nil t "subheadlines")
	    (list "\"TODO\"" nil "todos")
	    (list "\"TODO\"" t "subtodos"))))
	 (subheadlines (org-x-create-list-of-headlines-with-src-blocks
			subheadline-level
			"elisp"
			":tangle yes"
			title-code-pairs)))
    (append (list top-headline) subheadlines)))

(defalias 'org-x--create-string-of-tree-sitter-yank-last-as-functions
  (-compose (-partial #'s-join "\n")
	    #'flatten-list
	    (-partial #'mapcar #'org-x--create-list-of-tree-sitter-yank-last-as-functions-for-lang)))

;; The following functions are programmatically generated

(defalias 'org-x-yank-last-typescript-list-as-headlines
  (org-x--create-yank-last-kill-as-headlines-tree-sitter-function
   'typescript nil nil)
  "Convert the last killed typescript list into a list of org headlines.
The resulting list will be added to the kill ring and then yanked at point.

\(fn)")

(defalias 'org-x-yank-last-typescript-list-as-subheadlines
  (org-x--create-yank-last-kill-as-headlines-tree-sitter-function
   'typescript nil t)
  "Convert the last killed typescript list into a list of org headlines.
The resulting list will be added to the kill ring and then yanked at point.

\(fn)")

(defalias 'org-x-yank-last-typescript-list-as-todos
  (org-x--create-yank-last-kill-as-headlines-tree-sitter-function
   'typescript "TODO" nil)
  "Convert the last killed typescript list into a list of org headlines.
The resulting list will be added to the kill ring and then yanked at point.

\(fn)")

(defalias 'org-x-yank-last-typescript-list-as-subtodos
  (org-x--create-yank-last-kill-as-headlines-tree-sitter-function
   'typescript "TODO" t)
  "Convert the last killed typescript list into a list of org headlines.
The resulting list will be added to the kill ring and then yanked at point.

\(fn)")

(defalias 'org-x-yank-last-javascript-list-as-headlines
  (org-x--create-yank-last-kill-as-headlines-tree-sitter-function
   'javascript nil nil)
  "Convert the last killed javascript list into a list of org headlines.
The resulting list will be added to the kill ring and then yanked at point.

\(fn)")

(defalias 'org-x-yank-last-javascript-list-as-subheadlines
  (org-x--create-yank-last-kill-as-headlines-tree-sitter-function
   'javascript nil t)
  "Convert the last killed javascript list into a list of org headlines.
The resulting list will be added to the kill ring and then yanked at point.

\(fn)")

(defalias 'org-x-yank-last-javascript-list-as-todos
  (org-x--create-yank-last-kill-as-headlines-tree-sitter-function
   'javascript "TODO" nil)
  "Convert the last killed javascript list into a list of org headlines.
The resulting list will be added to the kill ring and then yanked at point.

\(fn)")

(defalias 'org-x-yank-last-javascript-list-as-subtodos
  (org-x--create-yank-last-kill-as-headlines-tree-sitter-function
   'javascript "TODO" t)
  "Convert the last killed javascript list into a list of org headlines.
The resulting list will be added to the kill ring and then yanked at point.

\(fn)")

(defalias 'org-x-yank-last-json-list-as-headlines
  (org-x--create-yank-last-kill-as-headlines-tree-sitter-function
   'json nil nil)
  "Convert the last killed json list into a list of org headlines.
The resulting list will be added to the kill ring and then yanked at point.

\(fn)")

(defalias 'org-x-yank-last-json-list-as-subheadlines
  (org-x--create-yank-last-kill-as-headlines-tree-sitter-function
   'json nil t)
  "Convert the last killed json list into a list of org headlines.
The resulting list will be added to the kill ring and then yanked at point.

\(fn)")

(defalias 'org-x-yank-last-json-list-as-todos
  (org-x--create-yank-last-kill-as-headlines-tree-sitter-function
   'json "TODO" nil)
  "Convert the last killed json list into a list of org headlines.
The resulting list will be added to the kill ring and then yanked at point.

\(fn)")

(defalias 'org-x-yank-last-json-list-as-subtodos
  (org-x--create-yank-last-kill-as-headlines-tree-sitter-function
   'json "TODO" t)
  "Convert the last killed json list into a list of org headlines.
The resulting list will be added to the kill ring and then yanked at point.

\(fn)")

(defun org-x--get-file-clock-strings (file)
  (--> file
   (org-babel-eval-read-file it)
   (s-split "\n" it)
   (mapcar #'s-trim it)
   (seq-filter (-partial #'s-match org-element-clock-line-re) it)))

;;(org-x--get-file-clock-strings "/home/earl/org/la-mentale.org")

(defalias 'org-x--get-file-clock-strings-for-list-of-files (-partial #'mapcar #'org-x--get-file-clock-strings))

(defun org-x--diff (new old &optional archive)
  (if archive
      (-difference new (-union old archive))
    (-difference new old)))



(provide 'org-x)
;;; org-x.el ends here
