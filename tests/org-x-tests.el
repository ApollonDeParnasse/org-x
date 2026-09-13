;;; org-x-tests.el --- Test for org-x  -*- lexical-binding: t; -*-

;; Author: Earl Chase
;; Maintainer: Earl Chase
;; Version: 0.0
;; Keywords: tools
;; Package-Requires: ((emacs "30") (org "9.7") (dash "2.20.0") (s "1.13.1") (compat "29"))
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

;;; Commentary: Tests for org-x

;;

;;; Code:

(require 'ert)
(require 'ert-x)
(require 's)
(require 'org-element)
(require 'generate)
(require 'org-x)

;; copied directly from org-test
;; eventually replace with generate-with-buffer-with-text
(defmacro org-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with Org mode as the active
mode holding TEXT.  If the string \"<point>\" appears in TEXT
then remove it and place the point there before running BODY,
otherwise place the point at the beginning of the inserted text."
  (declare (indent 1) (debug t))
  (org-with-gensyms (inside-text)
    `(let ((,inside-text (if (stringp ,text) ,text (eval ,text)))
           (org-mode-hook nil))
       (with-temp-buffer
         (org-mode)
         (let ((point (string-match "<point>" ,inside-text)))
           (if point
               (progn
                 (insert (replace-match "" nil nil ,inside-text))
                 (goto-char (1+ (match-beginning 0))))
             (insert ,inside-text)
             (goto-char (point-min))))
         (font-lock-ensure (point-min) (point-max))
         ,@body))))

(defun org-x--assert-is-element-type (type interpreter)
  (lambda (actual-value)
    (should (org-element-type-p actual-value type))
    (should (stringp (funcall interpreter actual-value nil)))))

(defalias 'org-x--assert-is-headline-element (org-x--assert-is-element-type
					      'headline
					      #'org-element-headline-interpreter))

(defalias 'org-x--assert-is-src-block-element (org-x--assert-is-element-type
					       'src-block
					       #'org-element-src-block-interpreter))

(generate-ert-deftest-n-times org-x--create-headline-stars ()
  :num-runs 100
  (let* ((test-headline-level (generate-random-nat-number-in-range (list 1 15))))
    (should (equal (s-count-matches (regexp-quote "*")
				    (org-x--create-headline-stars test-headline-level))
		   test-headline-level))))

;; replace with ert tests
(generate-ert-deftest-n-times org-x--next-headline-level/with-subheadline/empty-buffer ()
  :num-runs 1
  (org-test-with-temp-text ""
    (should-error (org-x--next-headline-level t) :type 'org-x--no-headline)))

(generate-ert-deftest-n-times org-x--next-headline-level/with-subheadline/before-first-headline ()
  :num-runs 100
  (let* ((lines-with-content (generate-random-list-of-sentences))
	 (lines-with-content-count (length lines-with-content))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 lines-with-content-count)))
	 (blank-lines (make-string blank-lines-count ?\n))
	 (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	 (test-tree (->> test-headline-level
		     (generate-list-of-n-strings)
		     (seq-map-indexed (lambda (x i) (format "%s %s" (make-string (1+ i) ?*) x)))
		     (s-join "\n")))
	 (test-buffer-text (format "%s%s" blank-lines  test-tree))
	 (test-line-number (generate-random-nat-number-in-range (list 1 blank-lines-count))))
    (org-test-with-temp-text test-buffer-text

      (goto-line test-line-number)
      (should-error (org-x--next-headline-level t) :type 'org-x--no-headline))))

(generate-ert-deftest-n-times org-x--next-headline-level/with-subheadline/after-a-headline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	  (lines-with-content-count (length lines-with-content))
	  (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	  (blank-lines (make-list blank-lines-count ""))
	  (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	  (test-tree (--> test-headline-level
		     (generate-list-of-n-strings it)
		     (seq-map-indexed (lambda (x i) (format "%s %s" (make-string (1+ i) ?*) x)) it)
		     (append it blank-lines)
		     (s-join "\n" it)))
	  (test-buffer-text (concat test-tree "\n")))
    (org-test-with-temp-text test-buffer-text

      (goto-char (point-max))
      (should (equal (org-x--next-headline-level t) (1+ test-headline-level))))))

(generate-ert-deftest-n-times org-x-convert-buffer-lines-into-headlines/without-subheadline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	 (lines-with-content-count (length lines-with-content))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count ""))
	 (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	 (test-tree (--> test-headline-level
		     (generate-list-of-n-strings it)
		     (seq-map-indexed (lambda (x i) (format "%s %s" (make-string (1+ i) ?*) x)) it)
		     (append it blank-lines)
		     (s-join "\n" it)))
	 (test-buffer-text (concat test-tree "\n")))
    (org-test-with-temp-text test-buffer-text

      (goto-char (point-max))
      (should (equal (org-x--next-headline-level) test-headline-level)))))

(generate-ert-deftest-n-times org-x--next-headline-level/without-subheadline/empty-buffer ()
  :num-runs 1
  (org-test-with-temp-text ""
    (should (equal (org-x--next-headline-level) 1))))

(generate-ert-deftest-n-times org-x--next-headline-level/without-subheadline/before-first-headline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	 (lines-with-content-count (length lines-with-content))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count ""))
	 (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	 (test-tree (->> test-headline-level
			 (generate-list-of-n-strings)
			 (seq-map-indexed (lambda (x i) (format "%s %s" (make-string (1+ i) ?*) x)))
			 (append blank-lines)
			 (s-join "\n")))
	 (test-line-number (generate-random-nat-number-in-range (list 1 blank-lines-count))))
    (org-test-with-temp-text test-tree

      (goto-line test-line-number)
      (should (equal (org-x--next-headline-level) 1)))))

(generate-ert-deftest-n-times org-x--build-headline-element ()
  :num-runs 100
  (let* ((test-title (generate-random-word))
	 (test-level (generate-random-org-headline-level))
	 (actual-element (org-x--build-headline-element
			  test-title
			  :level test-level)))
    (org-x--assert-is-headline-element actual-element)
    (should (equal (org-element-property :level actual-element) test-level))))

(generate-ert-deftest-n-times org-x--build-src-block-element ()
  :num-runs 100
  (let* ((test-language (generate-random-language-name))
	 (test-code (generate-random-multiline-string))
	 (test-parameters (generate-random-string-of-src-block-parameters))
	 (actual-element (org-x--build-src-block-element
			  :parameters test-parameters
			  :language test-language
			  :value test-code)))
    (org-x--assert-is-src-block-element actual-element)
    (should (equal (org-element-property :value actual-element) test-code))))

(generate-ert-deftest-n-times org-x--build-src-block-string ()
  :num-runs 100
  (let* ((test-language (generate-random-language-name))
	 (test-code (generate-random-multiline-string))
	 (test-parameters (generate-random-string-of-src-block-parameters))
	 (actual-string (org-x--build-src-block-string
			  :parameters test-parameters
			  :language test-language
			  :value test-code)))
    (should (s-starts-with-p "#+begin_src" actual-string))
    (should (s-ends-with-p "#+end_src" actual-string))))

(generate-ert-deftest-n-times org-x--build-headline-string/simple ()
  :num-runs 100
  (-let* ((test-title (generate-random-word))
	 (test-level (generate-random-org-headline-level))
	 (actual-string (org-x--build-headline-string
			  test-title
			  :level test-level))
	 ((actual-stars actual-title)
	  (s-split " " actual-string)))
    (should (length= actual-stars test-level))
    (should (equal (s-trim actual-title) test-title))))

(generate-ert-deftest-n-times org-x--build-headline-string/with-src-block ()
  :num-runs 100
  (-let* ((test-title (generate-random-word))
	 (test-level (generate-random-org-headline-level))
	 (test-language (generate-random-language-name))
	 (test-code (generate-random-multiline-string))
	 (test-parameters (generate-random-string-of-src-block-parameters))
	 (test-contents (org-x--build-src-block-string
			  :parameters test-parameters
			  :language test-language
			  :value test-code))
	 (actual-string (org-x--build-headline-string
			  test-title
			  :level test-level
			  :contents test-contents))
	 (actual-headline-part (car (s-split "\n" actual-string)))
	 ((actual-stars actual-title) (s-split " " actual-headline-part)))
    (should (length= actual-stars test-level))
    (should (equal actual-title test-title))
    (should (s-contains-p test-contents actual-string))))

(generate-ert-deftest-n-times org-x-create-headline-with-src-block ()
  :num-runs 100
  (let* ((test-sentence (generate-random-sentence))
	 (test-level (generate-random-org-headline-level))
	 (test-code (generate-random-sentence))
	 (test-title (generate-random-word))
	 (test-lang (generate-random-language-name))
	 (test-parameters (generate-random-string-of-src-block-parameters))
	 (actual-headline (org-x-create-headline-with-src-block
		       test-title
		       :level test-level
		       :language test-lang
		       :value test-code
		       :parameters test-parameters)))
  (should (s-contains-p test-title actual-headline))
  (should (s-contains-p "#+begin_src" actual-headline))
  (should (s-contains-p test-code actual-headline))
  (should (s-contains-p test-lang actual-headline))
  (should (s-contains-p test-parameters actual-headline))
  (should (s-ends-with-p "#+end_src" actual-headline))))

(generate-ert-deftest-n-times org-x-create-list-of-headlines-with-src-blocks ()
  :num-runs 100
  (let* ((test-count (generate-random-nat-number-in-range (list 1 15)))
	 (test-level (generate-random-org-headline-level))
	 (test-code (generate-list-of-n-sentences test-count))
	 (test-titles (generate-list-of-n-words test-count))
	 (test-tuples (-zip-lists test-titles test-code))
	 (test-lang (generate-random-language-name))
	 (test-parameters (generate-random-string-of-src-block-parameters))
	 (actual-list (org-x-create-list-of-headlines-with-src-blocks
		       test-level
		       test-lang
		       test-parameters
		       test-tuples))
	 (actual-random-headline (generate-seq-take-random-value-from-seq
				  actual-list)))
    (should (length= actual-list test-count))
    (should (s-starts-with-p (org-x--create-headline-stars test-level)
			     actual-random-headline))
    (should (s-contains-p "#+begin_src" actual-random-headline))
    (should (s-ends-with-p "#+end_src" actual-random-headline))))

(generate-ert-deftest-n-times org-x--convert-block-of-text-into-lines-of-x ()
  :num-runs 100
  (let* ((lines-with-content (generate-random-list-of-sentences))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count ""))
	 (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (actual-headlines (org-x--convert-block-of-text-into-lines-of-x #'identity test-lines))
	 (actual-lines (s-split "\n" actual-headlines)))
    (should (length= actual-lines (length lines-with-content)))
    (should-not (seq-difference actual-lines lines-with-content))))

(generate-ert-deftest-n-times org-x--convert-list-into-lines-of-x ()
  :num-runs 100
  (let* ((test-list (generate-random-list-of-words))
	 (actual-lines (org-x--convert-list-into-lines-of-x #'reverse test-list))
	 (actual-split-lines (s-split "\n" actual-lines)))
    (should (length= actual-split-lines (length test-list)))
    (should (seq-contains-p actual-split-lines (reverse (generate-seq-take-random-value-from-seq test-list))))))

(generate-ert-deftest-n-times org-x--create-convert-buffer-lines-function ()
  :num-runs 100
  (let* ((lines-with-content (generate-random-list-of-sentences))
	 (blank-lines-count (generate-random-nat-number-in-range
			     (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count ""))
	 (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (test-lambda (lambda (text) (->> text
					  (s-split "\n")
					  (mapcar #'reverse)
					  (s-join "\n"))))
	 (actual-text (org-test-with-temp-text test-lines
			(push-mark)
			(goto-char (point-max))
			(funcall (org-x--create-convert-buffer-lines-function test-lambda))
			(buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (s-split "\n" actual-text)))
    (should (length= actual-lines (+ (length lines-with-content) blank-lines-count)))
    (should (seq-contains-p actual-lines (reverse (generate-seq-take-random-value-from-seq lines-with-content))))))

(generate-ert-deftest-n-times org-x--convert-block-of-text-into-headlines ()
  :num-runs 100
  (let ((test-keyword (generate-seq-take-random-value-from-seq (list nil "TODO")))
	 (test-subheadlinep (generate-random-boolean)))
    (should (functionp (org-x--convert-block-of-text-into-headlines test-keyword test-subheadlinep)))))

(generate-ert-deftest-n-times org-x--convert-list-into-headlines ()
  :num-runs 100
  (let ((test-keyword (generate-seq-take-random-value-from-seq (list nil "TODO")))
	 (test-subheadlinep (generate-random-boolean)))
    (should (functionp (org-x--convert-list-into-headlines test-keyword test-subheadlinep)))))

(generate-ert-deftest-n-times org-x-convert-buffer-lines-into-headlines/empty-buffer ()
  :num-runs 100
  (let* ((lines-with-content (generate-random-list-of-sentences))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count ""))
	 (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (expected-line (format "* %s" (generate-seq-take-random-value-from-seq lines-with-content)))
	 (actual-headlines
	  (org-test-with-temp-text test-lines
	    (push-mark)
	    (goto-char (point-max))
	    (org-x-convert-buffer-lines-into-headlines)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (s-split "\n" actual-headlines))
	 (actual-random-line (generate-seq-take-random-value-from-seq actual-lines)))
    (should (length= actual-lines (length lines-with-content)))
    (should (seq-contains-p actual-lines expected-line))))

(generate-ert-deftest-n-times org-x-convert-buffer-lines-into-headlines/before-first-headline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	 (lines-with-content-count (length lines-with-content))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count "\n"))
	 (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (test-lines-count (+ blank-lines-count lines-with-content-count))
	 (test-headline (format "* %s" (generate-random-word)))
	 (test-buffer-text (concat test-lines "<point>\n" test-headline))
	 (expected-line (format "* %s" (generate-seq-take-random-value-from-seq lines-with-content)))
	 (actual-headlines
	  (org-test-with-temp-text test-buffer-text
	    (push-mark)
	    (goto-char (point-min))
	    (org-x-convert-buffer-lines-into-headlines)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 ((actual-lines actual-former-first-headline) (funcall (-compose (-juxt #'butlast #'-last-item) #'s-split) "\n" actual-headlines))
	 (actual-random-line (generate-seq-take-random-value-from-seq actual-lines)))
    (should (equal actual-former-first-headline test-headline))
    (should (length= actual-lines lines-with-content-count))
    (should (seq-contains-p actual-lines expected-line))))

(generate-ert-deftest-n-times org-x-convert-buffer-lines-into-headlines/underneath-a-headline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	  (lines-with-content-count (length lines-with-content))
	  (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	  (blank-lines (make-list blank-lines-count "\n"))
	  (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	  (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	  ((test-tree expected-former-first-lines) (->> test-headline-level
									   (generate-list-of-n-strings)
									   (seq-map-indexed (lambda (x i) (format "%s %s" (make-string (1+ i) ?*) x)))
									   (funcall (-juxt (-partial #'s-join "\n") #'identity))))
	  (test-buffer-text (concat test-tree "\n" "<point>" test-lines))
	  (expected-stars (make-string test-headline-level ?*))
	  (expected-line (format "%s %s" expected-stars (generate-seq-take-random-value-from-seq lines-with-content)))
	  (actual-headlines
	   (org-test-with-temp-text test-buffer-text
	     (push-mark)
	     (goto-char (point-max))
	     (org-x-convert-buffer-lines-into-headlines)
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (let ((actual-new-headlines (seq-difference (s-split "\n" actual-headlines) expected-former-first-lines)))
      (should (length= actual-new-headlines lines-with-content-count))
      (should (seq-contains-p actual-new-headlines expected-line)))))

(generate-ert-deftest-n-times org-x-convert-buffer-lines-into-subheadlines/empty-buffer ()
  :num-runs 100
  (let* ((lines-with-content (generate-random-list-of-sentences))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count ""))
	 (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content))))
     (org-test-with-temp-text test-lines
       (push-mark)
       (goto-char (point-max))
       (should-error (org-x-convert-buffer-lines-into-subheadlines) :type 'org-x--no-headline))))

(generate-ert-deftest-n-times org-x-convert-buffer-lines-into-subheadlines/before-first-headline ()
  :num-runs 100
  (let* ((lines-with-content (generate-random-list-of-sentences))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count ""))
	 (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (test-first-headline (format "* %s" (generate-random-sentence)))
	 (test-buffer-text (concat test-lines "<point>\n" test-first-headline)))
     (org-test-with-temp-text test-buffer-text
       (push-mark)
       (goto-char (point-min))
       (should-error (org-x-convert-buffer-lines-into-subheadlines) :type 'org-x--no-headline))))

(generate-ert-deftest-n-times org-x-convert-buffer-lines-into-subheadlines/underneath-a-headline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	 (lines-with-content-count (length lines-with-content))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count ""))
	 (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	 ((test-tree expected-former-first-lines) (->> (generate-list-of-n-strings test-headline-level)
			 (seq-map-indexed (lambda (x i) (format "%s %s" (make-string (1+ i) ?*) x)))
			 (funcall (-juxt (-partial #'s-join "\n") #'identity))))
	 (test-buffer-text (concat test-tree "\n<point>" test-lines))
	 (expected-stars (make-string (1+ test-headline-level) ?*))
	 (expected-line (format "%s %s" expected-stars (generate-seq-take-random-value-from-seq lines-with-content)))
	 (actual-headlines
	  (org-test-with-temp-text test-buffer-text
	    (push-mark)
	    (goto-char (point-max))
	    (org-x-convert-buffer-lines-into-subheadlines)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 ((actual-former-first-lines actual-lines-at-last-level) (->> actual-headlines
								      (s-split "\n")
								      (-split-with
								       (lambda (x) (s-starts-with-p (format "%s " expected-stars) x)))))
    (should (equal expected-former-first-lines actual-former-first-lines))
    (should (length= actual-lines-at-last-level lines-with-content-count))
    (should (seq-contains-p actual-lines-at-last-level expected-line)))))

(generate-ert-deftest-n-times org-x-convert-buffer-lines-into-todos/empty-buffer ()
  :num-runs 100
  (let* ((lines-with-content (generate-random-list-of-sentences))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count "\n"))
	 (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (expected-line (format "* TODO %s" (generate-seq-take-random-value-from-seq lines-with-content)))
	 (actual-headlines
	  (org-test-with-temp-text test-lines
	    (push-mark)
	    (goto-char (point-max))
	    (org-x-convert-buffer-lines-into-todos)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (s-split "\n" actual-headlines))
	 (actual-random-line (generate-seq-take-random-value-from-seq actual-lines)))
    (should (length= actual-lines (length lines-with-content)))
    (should (seq-contains-p actual-lines expected-line))))

(generate-ert-deftest-n-times org-x-convert-buffer-lines-into-todos/before-first-headline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	 (lines-with-content-count (length lines-with-content))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count "\n"))
	 (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (test-headline (format "* TODO %s" (generate-random-word)))
	 (test-buffer-text (concat test-lines "<point>\n" test-headline))
	 (expected-line (format "* TODO %s" (generate-seq-take-random-value-from-seq lines-with-content)))
	 (actual-headlines
	  (org-test-with-temp-text test-buffer-text

	    (push-mark)
	    (goto-char (point-min))
	    (org-x-convert-buffer-lines-into-todos)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 ((actual-lines actual-former-first-headline) (funcall (-compose (-juxt #'butlast #'-last-item) #'s-split)
							       "\n" actual-headlines))
	 (actual-random-line (generate-seq-take-random-value-from-seq actual-lines)))
    (should (equal actual-former-first-headline test-headline))
    (should (length= actual-lines lines-with-content-count))
    (should (seq-contains-p actual-lines expected-line))))

(generate-ert-deftest-n-times org-x-convert-buffer-lines-into-todos/underneath-a-headline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	  (lines-with-content-count (length lines-with-content))
	  (blank-lines-count (generate-random-nat-number-in-range (list 1 lines-with-content-count)))
	  (blank-lines (make-list blank-lines-count ""))
	  (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	  (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	  (test-strings (generate-list-of-n-strings test-headline-level))
	  (test-booleans (generate-list-of-n-booleans test-headline-level))
	  (test-pairs (-zip-lists test-strings test-booleans))
	  ((test-tree expected-former-first-lines) (->> test-pairs
							(seq-map-indexed (-lambda ((x todo) i)
									   (if todo
									       (format "%s TODO %s" (make-string (1+ i) ?*) x)
									     (format "%s %s" (make-string (1+ i) ?*) x))))
							(funcall (-juxt (-partial #'s-join "\n") #'identity))))
	  (test-buffer-text (concat test-tree "\n<point>" test-lines))
	  (expected-stars (make-string test-headline-level ?*))
	  (expected-line (format "%s TODO %s" expected-stars
				 (generate-seq-take-random-value-from-seq lines-with-content)))
	  (actual-headlines
	   (org-test-with-temp-text test-buffer-text
	     (push-mark)
	     (goto-char (point-max))
	     (org-x-convert-buffer-lines-into-todos)
	     (buffer-substring-no-properties (point-min) (point-max))))
	  ((actual-first-lines actual-lines-at-last-level) (->> actual-headlines
								(s-split "\n")
								(-split-with (lambda (x)
									       (s-starts-with-p
										(format "%s " expected-stars) x))))))
    (let ((actual-new-headlines (seq-difference (s-split "\n" actual-headlines) expected-former-first-lines)))
      (should (length= actual-new-headlines lines-with-content-count))
      (should (seq-contains-p actual-new-headlines expected-line)))))

(generate-ert-deftest-n-times org-x-convert-buffer-lines-into-subtodos/empty-buffer ()
  :num-runs 100
  (let* ((lines-with-content (generate-random-list-of-sentences))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count "\n"))
	 (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content))))
     (org-test-with-temp-text test-lines
       (push-mark)
       (goto-char (point-max))
       (should-error (org-x-convert-buffer-lines-into-subtodos) :type 'org-x--no-headline))))

(generate-ert-deftest-n-times org-x-convert-buffer-lines-into-subtodos/before-first-headline ()
  :num-runs 100
  (let* ((lines-with-content (generate-random-list-of-sentences))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count "\n"))
	 (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (test-first-headline (format "* %s" (generate-random-sentence)))
	 (test-buffer-text (concat test-lines "\n" test-first-headline)))
     (org-test-with-temp-text test-buffer-text
       (push-mark)
       (goto-char (point-max))
       (forward-line -1)
       (should-error (org-x-convert-buffer-lines-into-subtodos) :type 'org-x--no-headline))))

(generate-ert-deftest-n-times org-x-convert-buffer-lines-into-subtodos/underneath-a-headline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	  (lines-with-content-count (length lines-with-content))
	  (blank-lines-count (generate-random-nat-number-in-range (list 1 lines-with-content-count)))
	  (blank-lines (make-list blank-lines-count ""))
	  (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	  (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	  (test-strings (generate-list-of-n-strings test-headline-level))
	  (test-booleans (generate-list-of-n-booleans test-headline-level))
	  (test-pairs (-zip-lists test-strings test-booleans))
	  ((test-tree expected-former-first-lines) (->> test-pairs
							(seq-map-indexed (-lambda ((x todo) i)
									   (if todo
									       (format "%s TODO %s" (make-string (1+ i) ?*) x)
									     (format "%s %s" (make-string (1+ i) ?*) x))))
							(funcall (-juxt (-partial #'s-join "\n") #'identity))))
	  (test-buffer-text (concat test-tree "\n<point>" test-lines))
	  (expected-stars (make-string (1+ test-headline-level) ?*))
	  (expected-line (format "%s TODO %s" expected-stars
				 (generate-seq-take-random-value-from-seq lines-with-content)))
	  (actual-headlines
	   (org-test-with-temp-text test-buffer-text
	     (push-mark)
	     (goto-char (point-max))
	     (org-x-convert-buffer-lines-into-subtodos)
	     (buffer-substring-no-properties (point-min) (point-max))))
	  ((actual-first-lines actual-lines-at-last-level) (->> actual-headlines
								(s-split "\n")
								(-split-with (lambda (x)
									       (s-starts-with-p
										(format "%s " expected-stars) x))))))
    (let ((actual-new-headlines (seq-difference (s-split "\n" actual-headlines) expected-former-first-lines)))
      (should (length= actual-new-headlines lines-with-content-count))
      (should (seq-contains-p actual-new-headlines expected-line)))))

(generate-ert-deftest-n-times org-x--create-yank-last-kill-as-function ()
  :num-runs 100
  (let* ((lines-with-content (generate-random-list-of-sentences))
	 (blank-lines-count (generate-random-nat-number-in-range
			     (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count ""))
	 (test-lines (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (test-lambda (lambda (text) (->> text
					  (s-split "\n")
					  (mapcar #'upcase)
					  (s-join "\n"))))
	 (actual-text (org-test-with-temp-text ""
			(kill-new test-lines)
			(funcall (org-x--create-yank-last-kill-as-function test-lambda))
			(buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (s-split "\n" actual-text)))
    (should (length= actual-lines (+ (length lines-with-content) blank-lines-count)))
    (should (seq-contains-p actual-lines (upcase (generate-seq-take-random-value-from-seq lines-with-content))))))

(generate-ert-deftest-n-times org-x-yank-last-kill-as-headlines/empty-buffer ()
  :num-runs 100
  (let* ((lines-with-content (generate-random-list-of-sentences))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count ""))
	 (test-kill (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (expected-line (format "* %s" (generate-seq-take-random-value-from-seq lines-with-content)))
	 (actual-headlines
	  (org-test-with-temp-text ""
	    (kill-new test-kill)
	    (org-x-yank-last-kill-as-headlines)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (s-split "\n" actual-headlines))
	 (actual-random-line (generate-seq-take-random-value-from-seq actual-lines)))
    (should (length= actual-lines (length lines-with-content)))
    (should (seq-contains-p actual-lines expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-kill-as-headlines/before-first-headline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	 (lines-with-content-count (length lines-with-content))
	 (blank-kill-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-kill-lines (make-list blank-kill-lines-count "\n"))
	 (test-kill (s-join "\n" (generate-append-and-shuffle blank-kill-lines lines-with-content)))
	 (test-headline (format "* %s" (generate-random-word)))
	 (blank-bluffer-lines-count (generate-random-nat-number-in-range (list 1 15)))
	 (random-point-line (generate-random-nat-number-in-range (list 1 blank-bluffer-lines-count)))
	 (test-blank-buffer-lines (--> blank-bluffer-lines-count
				       (make-list it "\n")
				       (-insert-at random-point-line "<point>" it)
				       (s-join "\n" it)))
	 (test-buffer-text (concat test-blank-buffer-lines "\n" test-headline))
	 (expected-line (format "* %s" (generate-seq-take-random-value-from-seq lines-with-content)))
	 (actual-headlines
	  (org-test-with-temp-text test-buffer-text
	    (kill-new test-kill)
	    (org-x-yank-last-kill-as-headlines)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (->> actual-headlines
			    (s-split "\n")
			    (seq-filter (-rpartial #'length> 0))))
	 (actual-new-headlines (seq-difference actual-lines (list test-headline))))
    (should (length= actual-new-headlines lines-with-content-count))
    (should (seq-contains-p actual-new-headlines expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-kill-as-headlines/underneath-a-headline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	  (lines-with-content-count (length lines-with-content))
	  (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	  (blank-lines (make-list blank-lines-count "\n"))
	  (test-kill (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	  (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	  ((test-tree expected-former-first-lines) (->> test-headline-level
							(generate-list-of-n-strings)
							(seq-map-indexed (lambda (x i) (format "%s %s" (make-string (1+ i) ?*) x)))
							(funcall (-juxt (-partial #'s-join "\n") #'identity))))
	  (test-buffer-text (concat test-tree "\n" "<point>"))
	  (expected-stars (make-string test-headline-level ?*))
	  (expected-line (format "%s %s" expected-stars (generate-seq-take-random-value-from-seq lines-with-content)))
	  (actual-headlines
	   (org-test-with-temp-text test-buffer-text
	     (kill-new test-kill)
	     (org-x-yank-last-kill-as-headlines)
	     (buffer-substring-no-properties (point-min) (point-max))))
	  (actual-new-headlines (seq-difference (s-split "\n" actual-headlines) expected-former-first-lines)))
    (should (length= actual-new-headlines lines-with-content-count))
    (should (seq-contains-p actual-new-headlines expected-line))))

(setq org-x-failing-test-case-1 "assertions




contexts




filters




formatters




moves




clone.ts




date-reviver.ts




diffpatcher.ts




index.ts




pipe.ts




processor.ts




types.ts




with-text-diffs.ts
")

(generate-ert-deftest-n-times org-x-yank-last-kill-as-headlines/fail ()
  :num-runs 1
  (-let* ((test-kill org-x-failing-test-case-1)
	  (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	  ((test-tree expected-former-first-lines) (->> test-headline-level
							(generate-list-of-n-strings)
							(seq-map-indexed (lambda (x i) (format "%s %s" (make-string (1+ i) ?*) x)))
							(funcall (-juxt (-partial #'s-join "\n") #'identity))))
	  (test-buffer-text (concat test-tree "\n" "<point>"))
	  (expected-stars (make-string test-headline-level ?*))
	  (expected-line (format "%s %s" expected-stars "types.ts"))
	  (actual-headlines
	   (org-test-with-temp-text test-buffer-text
	     (kill-new test-kill)
	     (org-x-yank-last-kill-as-headlines)
	     (buffer-substring-no-properties (point-min) (point-max))))
	  (actual-new-headlines (seq-difference (s-split "\n" actual-headlines) expected-former-first-lines)))
    (should (seq-contains-p actual-new-headlines expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-kill-as-subheadlines/empty-buffer ()
  :num-runs 1
  (org-test-with-temp-text ""
    (kill-new (generate-random-sentence))
    (should-error (org-x-yank-last-kill-as-subheadlines) :type 'org-x--no-headline)))

(generate-ert-deftest-n-times org-x-yank-last-kill-as-subheadlines/before-first-headline ()
  :num-runs 100
  (let* ((blank-bluffer-lines-count (generate-random-nat-number-in-range (list 1 15)))
	 (random-point-line (generate-random-nat-number-in-range (list 1 blank-bluffer-lines-count)))
	 (test-blank-buffer-lines (--> blank-bluffer-lines-count
				       (make-list it "\n")
				       (-insert-at random-point-line "<point>" it)
				       (s-join "\n" it)))
	 (test-headline (format "* %s" (generate-random-sentence)))
	 (test-buffer-text (concat test-blank-buffer-lines "\n" test-headline)))
     (org-test-with-temp-text test-buffer-text
       (kill-new (generate-random-sentence))
       (should-error (org-x-yank-last-kill-as-subheadlines) :type 'org-x--no-headline))))

(generate-ert-deftest-n-times org-x-yank-last-kill-as-subheadlines/underneath-a-headline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	 (lines-with-content-count (length lines-with-content))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count ""))
	 (test-kill (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	 ((test-tree expected-former-first-lines) (->> (generate-list-of-n-strings test-headline-level)
			 (seq-map-indexed (lambda (x i) (format "%s %s" (make-string (1+ i) ?*) x)))
			 (funcall (-juxt (-partial #'s-join "\n") #'identity))))
	 (test-buffer-text (concat test-tree "\n<point>"))
	 (expected-stars (make-string (1+ test-headline-level) ?*))
	 (expected-line (format "%s %s" expected-stars (generate-seq-take-random-value-from-seq lines-with-content)))
	 (actual-headlines
	  (org-test-with-temp-text test-buffer-text
	    (kill-new test-kill)
	    (org-x-yank-last-kill-as-subheadlines)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (s-split "\n" actual-headlines))
	 (actual-new-headlines (seq-difference actual-lines expected-former-first-lines)))
    (should (length= actual-new-headlines lines-with-content-count))
    (should (seq-contains-p actual-new-headlines expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-kill-as-todos/empty-buffer ()
  :num-runs 100
  (let* ((lines-with-content (generate-random-list-of-sentences))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count "\n"))
	 (test-kill (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (expected-line (format "* TODO %s" (generate-seq-take-random-value-from-seq lines-with-content)))
	 (actual-headlines
	  (org-test-with-temp-text ""
	    (kill-new test-kill)
	    (org-x-yank-last-kill-as-todos)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (s-split "\n" actual-headlines)))
    (should (length= actual-lines (length lines-with-content)))
    (should (seq-contains-p actual-lines expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-kill-as-todos/before-first-headline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	 (lines-with-content-count (length lines-with-content))
	 (blank-kill-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-kill-lines (make-list blank-kill-lines-count "\n"))
	 (test-kill (s-join "\n" (generate-append-and-shuffle blank-kill-lines lines-with-content)))
	 (test-todo (format "* TODO %s" (generate-random-word)))
	 (blank-bluffer-lines-count (generate-random-nat-number-in-range (list 1 15)))
	 (random-point-line (generate-random-nat-number-in-range (list 1 blank-bluffer-lines-count)))
	 (test-blank-buffer-lines (--> blank-bluffer-lines-count
				       (make-list it "\n")
				       (-insert-at random-point-line "<point>" it)
				       (s-join "\n" it)))
	 (test-buffer-text (concat test-blank-buffer-lines "\n" test-todo))
	 (expected-line (format "* TODO %s" (generate-seq-take-random-value-from-seq lines-with-content)))
	 (actual-todos
	  (org-test-with-temp-text test-buffer-text
	    (kill-new test-kill)
	    (org-x-yank-last-kill-as-todos)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (->> actual-todos
			    (s-split "\n")
			    (seq-filter (-rpartial #'length> 0))))
	 (actual-new-todos (seq-difference actual-lines (list test-todo))))
    (should (length= actual-new-todos lines-with-content-count))
    (should (seq-contains-p actual-new-todos expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-kill-as-todos/underneath-a-headline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	  (lines-with-content-count (length lines-with-content))
	  (blank-lines-count (generate-random-nat-number-in-range (list 1 lines-with-content-count)))
	  (blank-lines (make-list blank-lines-count ""))
	  (test-kill (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	  (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	  (test-strings (generate-list-of-n-strings test-headline-level))
	  (test-booleans (generate-list-of-n-booleans test-headline-level))
	  (test-pairs (-zip-lists test-strings test-booleans))
	  ((test-tree expected-former-first-lines) (->> test-pairs
							(seq-map-indexed (-lambda ((x todo) i)
									   (if todo
									       (format "%s TODO %s" (make-string (1+ i) ?*) x)
									     (format "%s %s" (make-string (1+ i) ?*) x))))
							(funcall (-juxt (-partial #'s-join "\n") #'identity))))
	  (test-buffer-text (concat test-tree "\n<point>"))
	  (expected-stars (make-string test-headline-level ?*))
	  (expected-line (format "%s TODO %s" expected-stars
				 (generate-seq-take-random-value-from-seq lines-with-content)))
	  (actual-headlines
	   (org-test-with-temp-text test-buffer-text
	     (kill-new test-kill)
	     (org-x-yank-last-kill-as-todos)
	     (buffer-substring-no-properties (point-min) (point-max))))
	  ((actual-first-lines actual-lines-at-last-level) (->> actual-headlines
								(s-split "\n")
								(-split-with (lambda (x)
									       (s-starts-with-p
										(format "%s " expected-stars) x)))))
	  (actual-new-headlines (seq-difference (s-split "\n" actual-headlines) expected-former-first-lines)))
    (should (length= actual-new-headlines lines-with-content-count))
    (should (seq-contains-p actual-new-headlines expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-kill-as-subtodos/empty-buffer ()
  :num-runs 1
  (org-test-with-temp-text ""
    (kill-new (generate-random-sentence))
    (should-error (org-x-yank-last-kill-as-subtodos) :type 'org-x--no-headline)))

(generate-ert-deftest-n-times org-x-yank-last-kill-as-subtodos/before-first-headline ()
  :num-runs 100
  (let* ((blank-bluffer-lines-count (generate-random-nat-number-in-range (list 1 15)))
	 (random-point-line (generate-random-nat-number-in-range (list 1 blank-bluffer-lines-count)))
	 (test-blank-buffer-lines (--> blank-bluffer-lines-count
				       (make-list it "\n")
				       (-insert-at random-point-line "<point>" it)
				       (s-join "\n" it)))
	 (test-headline (format "* %s" (generate-random-sentence)))
	 (test-buffer-text (concat test-blank-buffer-lines "\n" test-headline)))
     (org-test-with-temp-text test-buffer-text
       (kill-new (generate-random-sentence))
       (should-error (org-x-yank-last-kill-as-subtodos) :type 'org-x--no-headline))))

(generate-ert-deftest-n-times org-x-yank-last-kill-as-subtodos/underneath-a-bheadline ()
  :num-runs 100
  (-let* ((lines-with-content (generate-random-list-of-sentences))
	 (lines-with-content-count (length lines-with-content))
	 (blank-lines-count (generate-random-nat-number-in-range (list 1 (length lines-with-content))))
	 (blank-lines (make-list blank-lines-count ""))
	 (test-kill (s-join "\n" (generate-append-and-shuffle blank-lines lines-with-content)))
	 (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	 ((test-tree expected-former-first-lines) (->> (generate-list-of-n-strings test-headline-level)
			 (seq-map-indexed (lambda (x i) (format "%s %s" (make-string (1+ i) ?*) x)))
			 (funcall (-juxt (-partial #'s-join "\n") #'identity))))
	 (test-buffer-text (concat test-tree "\n<point>"))
	 (expected-stars (make-string (1+ test-headline-level) ?*))
	 (expected-line (format "%s TODO %s" expected-stars (generate-seq-take-random-value-from-seq lines-with-content)))
	 (actual-todos
	  (org-test-with-temp-text test-buffer-text
	    (kill-new test-kill)
	    (org-x-yank-last-kill-as-subtodos)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (s-split "\n" actual-todos))
	 (actual-new-todos (seq-difference actual-lines expected-former-first-lines)))
    (should (length= actual-new-todos lines-with-content-count))
    (should (seq-contains-p actual-new-todos expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-elisp-list-as-headlines/empty-buffer ()
  :num-runs 100
  (let* ((test-list (generate-random-list-of-words))
	 (test-kill (prin1-to-string test-list))
	 (expected-line (format "* %s" (generate-seq-take-random-value-from-seq test-list)))
	 (actual-headlines
	  (org-test-with-temp-text ""
	    (kill-new test-kill)
	    (org-x-yank-last-elisp-list-as-headlines)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (s-split "\n" actual-headlines))
	 (actual-random-line (generate-seq-take-random-value-from-seq actual-lines)))
    (should (length= actual-lines (length test-list)))
    (should (seq-contains-p actual-lines expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-elisp-list-as-headlines/before-first-headline ()
  :num-runs 100
  (-let* ((test-list (generate-random-list-of-words))
	  (test-kill (prin1-to-string test-list))
	  (test-headline (format "* %s" (generate-random-word)))
	  (blank-bluffer-lines-count (generate-random-nat-number-in-range (list 1 15)))
	  (random-point-line (generate-random-nat-number-in-range (list 1 blank-bluffer-lines-count)))
	  (test-blank-buffer-lines (--> blank-bluffer-lines-count
					(make-list it "\n")
					(-insert-at random-point-line "<point>" it)
					(s-join "\n" it)))
	  (test-buffer-text (concat test-blank-buffer-lines "\n" test-headline))
	  (expected-line (format "* %s" (generate-seq-take-random-value-from-seq test-list)))
	  (actual-headlines
	   (org-test-with-temp-text test-buffer-text
				    (kill-new test-kill)
				    (org-x-yank-last-elisp-list-as-headlines)
				    (buffer-substring-no-properties (point-min) (point-max))))
	  (actual-lines (->> actual-headlines
			     (s-split "\n")
			     (seq-filter (-rpartial #'length> 0))))
	  (actual-new-headlines (seq-difference actual-lines (list test-headline))))
    (should (length= actual-new-headlines (length test-list)))
    (should (seq-contains-p actual-new-headlines expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-elisp-list-as-headlines/underneath-a-headline ()
  :num-runs 100
  (-let* ((test-list (generate-random-list-of-strings))
	  (test-kill (prin1-to-string test-list))
	  (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	  ((test-tree expected-former-first-lines) (->> test-headline-level
							(generate-list-of-n-strings)
							(seq-map-indexed (lambda (x i) (format "%s %s" (make-string (1+ i) ?*) x)))
							(funcall (-juxt (-partial #'s-join "\n") #'identity))))
	  (test-buffer-text (concat test-tree "\n" "<point>"))
	  (expected-stars (make-string test-headline-level ?*))
	  (expected-line (format "%s %s" expected-stars (generate-seq-take-random-value-from-seq test-list)))
	  (actual-headlines
	   (org-test-with-temp-text test-buffer-text
	     (kill-new test-kill)
	     (org-x-yank-last-elisp-list-as-headlines)
	     (buffer-substring-no-properties (point-min) (point-max))))
	  (actual-new-headlines (seq-difference (s-split "\n" actual-headlines) expected-former-first-lines)))
    (should (length= actual-new-headlines (length test-list)))
    (should (seq-contains-p actual-new-headlines expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-elisp-list-as-subheadlines/empty-buffer ()
  :num-runs 1
  (org-test-with-temp-text ""
    (kill-new (generate-random-sentence))
    (should-error (org-x-yank-last-elisp-list-as-subheadlines) :type 'org-x--no-headline)))

(generate-ert-deftest-n-times org-x-yank-last-elisp-list-as-subheadlines/before-first-headline ()
  :num-runs 100
  (let* ((blank-bluffer-lines-count (generate-random-nat-number-in-range (list 1 15)))
	 (random-point-line (generate-random-nat-number-in-range (list 1 blank-bluffer-lines-count)))
	 (test-blank-buffer-lines (--> blank-bluffer-lines-count
				       (make-list it "\n")
				       (-insert-at random-point-line "<point>" it)
				       (s-join "\n" it)))
	 (test-headline (format "* %s" (generate-random-sentence)))
	 (test-buffer-text (concat test-blank-buffer-lines "\n" test-headline)))
     (org-test-with-temp-text test-buffer-text
       (kill-new (generate-random-sentence))
       (should-error (org-x-yank-last-elisp-list-as-subheadlines) :type 'org-x--no-headline))))

(generate-ert-deftest-n-times org-x-yank-last-elisp-list-as-subheadlines/underneath-a-headline ()
  :num-runs 100
  (-let* ((test-list (generate-random-list-of-words))
	 (test-list-length (length test-list))
	 (test-kill (prin1-to-string test-list))
	 (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	 ((test-tree expected-former-first-lines) (->> (generate-list-of-n-strings test-headline-level)
			 (seq-map-indexed (lambda (x i) (format "%s %s" (make-string (1+ i) ?*) x)))
			 (funcall (-juxt (-partial #'s-join "\n") #'identity))))
	 (test-buffer-text (concat test-tree "\n<point>"))
	 (expected-stars (make-string (1+ test-headline-level) ?*))
	 (expected-line (format "%s %s" expected-stars (generate-seq-take-random-value-from-seq test-list)))
	 (actual-headlines
	  (org-test-with-temp-text test-buffer-text
	    (kill-new test-kill)
	    (org-x-yank-last-elisp-list-as-subheadlines)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (s-split "\n" actual-headlines))
	 (actual-new-headlines (seq-difference actual-lines expected-former-first-lines)))
    (should (length= actual-new-headlines test-list-length))
    (should (seq-contains-p actual-new-headlines expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-elisp-list-as-todos/empty-buffer ()
  :num-runs 100
  (let* ((test-list (generate-random-list-of-words))
	 (test-kill (prin1-to-string test-list))
	 (expected-line (format "* TODO %s" (generate-seq-take-random-value-from-seq test-list)))
	 (actual-todos
	  (org-test-with-temp-text ""
	    (kill-new test-kill)
	    (org-x-yank-last-elisp-list-as-todos)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (s-split "\n" actual-todos))
	 (actual-random-line (generate-seq-take-random-value-from-seq actual-lines)))
    (should (length= actual-lines (length test-list)))
    (should (seq-contains-p actual-lines expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-elisp-list-as-todos/before-first-headline ()
  :num-runs 100
  (-let* ((test-list (generate-random-list-of-sentences))
	  (test-list-length (length test-list))
	  (test-kill (prin1-to-string test-list))
	  (test-todo (format "* TODO %s" (generate-random-word)))
	  (blank-bluffer-lines-count (generate-random-nat-number-in-range (list 1 15)))
	  (random-point-line (generate-random-nat-number-in-range (list 1 blank-bluffer-lines-count)))
	  (test-blank-buffer-lines (--> blank-bluffer-lines-count
					(make-list it "\n")
					(-insert-at random-point-line "<point>" it)
					(s-join "\n" it)))
	  (test-buffer-text (concat test-blank-buffer-lines "\n" test-todo))
	  (expected-line (format "* TODO %s" (generate-seq-take-random-value-from-seq test-list)))
	  (actual-todos
	   (org-test-with-temp-text test-buffer-text
	     (kill-new test-kill)
	     (org-x-yank-last-elisp-list-as-todos)
	     (buffer-substring-no-properties (point-min) (point-max))))
	  (actual-lines (->> actual-todos
			     (s-split "\n")
			     (seq-filter (-rpartial #'length> 0))))
	  (actual-new-todos (seq-difference actual-lines (list test-todo))))
    (should (length= actual-new-todos test-list-length))
    (should (seq-contains-p actual-new-todos expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-elisp-list-as-todos/underneath-a-headline ()
  :num-runs 100
  (-let* ((test-list (generate-random-list-of-sentences))
	  (test-list-length (length test-list))
	  (test-kill (prin1-to-string test-list))
	  (test-headline-level (generate-random-nat-number-in-range (list 1 15)))
	  (test-strings (generate-list-of-n-strings test-headline-level))
	  (test-booleans (generate-list-of-n-booleans test-headline-level))
	  (test-pairs (-zip-lists test-strings test-booleans))
	  ((test-tree expected-former-first-lines) (->> test-pairs
							(seq-map-indexed (-lambda ((x todo) i)
									   (if todo
									       (format "%s TODO %s" (make-string (1+ i) ?*) x)
									     (format "%s %s" (make-string (1+ i) ?*) x))))
							(funcall (-juxt (-partial #'s-join "\n") #'identity))))
	  (test-buffer-text (concat test-tree "\n<point>"))
	  (expected-stars (make-string test-headline-level ?*))
	  (expected-line (format "%s TODO %s" expected-stars
				 (generate-seq-take-random-value-from-seq test-list)))
	  (actual-headlines
	   (org-test-with-temp-text test-buffer-text
	     (kill-new test-kill)
	     (org-x-yank-last-elisp-list-as-todos)
	     (buffer-substring-no-properties (point-min) (point-max))))
	  ((actual-first-lines actual-lines-at-last-level) (->> actual-headlines
								(s-split "\n")
								(-split-with (lambda (x)
									       (s-starts-with-p
										(format "%s " expected-stars) x)))))
	  (actual-new-headlines (seq-difference (s-split "\n" actual-headlines) expected-former-first-lines)))
    (should (length= actual-new-headlines test-list-length))
    (should (seq-contains-p actual-new-headlines expected-line))))

(generate-ert-deftest-n-times org-x-yank-last-elisp-list-as-subtodos/empty-buffer ()
  :num-runs 1
  (org-test-with-temp-text ""
    (kill-new (generate-random-sentence))
    (should-error (org-x-yank-last-elisp-list-as-subtodos) :type 'org-x--no-headline)))

(generate-ert-deftest-n-times org-x-yank-last-elisp-list-as-subtodos/before-first-headline ()
  :num-runs 100
  (let* ((blank-bluffer-lines-count (generate-random-nat-number-in-range (list 1 15)))
	 (random-point-line (generate-random-nat-number-in-range (list 1 blank-bluffer-lines-count)))
	 (test-blank-buffer-lines (--> blank-bluffer-lines-count
				       (make-list it "\n")
				       (-insert-at random-point-line "<point>" it)
				       (s-join "\n" it)))
	 (test-headline (format "* %s" (generate-random-sentence)))
	 (test-buffer-text (concat test-blank-buffer-lines "\n" test-headline)))
     (org-test-with-temp-text test-buffer-text
       (kill-new (generate-random-sentence))
       (should-error (org-x-yank-last-elisp-list-as-subtodos) :type 'org-x--no-headline))))

(generate-ert-deftest-n-times org-x-yank-last-elisp-list-as-subtodos/underneath-a-todo ()
  :num-runs 100
  (-let* ((test-list (generate-random-list-of-words))
	 (test-list-length (length test-list))
	 (test-kill (prin1-to-string test-list))
	 (test-todo-level (generate-random-nat-number-in-range (list 1 15)))
	 ((test-tree expected-former-first-lines) (->> (generate-list-of-n-strings test-todo-level)
			 (seq-map-indexed (lambda (x i) (format "%s %s" (make-string (1+ i) ?*) x)))
			 (funcall (-juxt (-partial #'s-join "\n") #'identity))))
	 (test-buffer-text (concat test-tree "\n<point>"))
	 (expected-stars (make-string (1+ test-todo-level) ?*))
	 (expected-line (format "%s TODO %s" expected-stars (generate-seq-take-random-value-from-seq test-list)))
	 (actual-todos
	  (org-test-with-temp-text test-buffer-text
	    (kill-new test-kill)
	    (org-x-yank-last-elisp-list-as-subtodos)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (actual-lines (s-split "\n" actual-todos))
	 (actual-new-todos (seq-difference actual-lines expected-former-first-lines)))
    (should (length= actual-new-todos test-list-length))
    (should (seq-contains-p actual-new-todos expected-line))))

(generate-ert-deftest-n-times org-x--tree-sitter-list-parser ()
  :num-runs 100
  (cl-flet ((test-runner (lang gen)
	      (-let* (((test-list test-base-list) (funcall gen))
		      (actual-list (org-x--tree-sitter-list-parser lang test-list)))
		(should (length= actual-list (length test-base-list)))
		(should (seq-contains-p actual-list
					(funcall (-compose #'prin1-to-string
							   #'generate-seq-take-random-value-from-seq)
						 test-base-list))))))
    (test-runner 'json (-compose (-juxt #'json-encode #'identity) #'generate-random-list-of-words))
    (test-runner 'typescript (-compose (-juxt #'json-encode #'identity) #'generate-random-list-of-words))
    (test-runner 'javascript (-compose (-juxt #'json-encode #'identity) #'generate-random-list-of-words))))

(generate-ert-deftest-n-times org-x--create-yank-last-kill-as-headlines-tree-sitter-function ()
  :num-runs 100
  (let* ((test-keyword (generate-seq-take-random-value-from-seq (list nil "TODO")))
	(test-subheadlinep (generate-random-boolean))
	(test-lang (generate-seq-take-random-value-from-seq (list 'typescript 'javascript 'json)))
	(actual-func (org-x--create-yank-last-kill-as-headlines-tree-sitter-function
		      test-lang
		      test-keyword
		      test-subheadlinep)))
    (should (functionp actual-func))))

(defconst org-x--TEST-TREE-SITTER-FUNC-CASES
  (list
   (list nil nil "headlines")
   (list nil t "subheadlines")
   (list "TODO" nil "todos")
   (list "TODO" t "subtodos")))

(generate-ert-deftest-n-times org-x--create-text-of-tree-sitter-yank-last-as-x-functions-for-lang ()
  :num-runs 100
  (-let* (((test-keyword test-subheadlinep test-type) (generate-seq-take-random-value-from-seq org-x--TEST-TREE-SITTER-FUNC-CASES))
	 (test-lang (generate-random-language-name))
	 (test-func (org-x--create-text-of-tree-sitter-yank-last-as-x-functions-for-lang
		     test-lang))
	 ((actual-alias-name actual-alias) (funcall test-func (list test-keyword test-subheadlinep test-type))))
    (should (s-contains-p test-lang actual-alias-name))
    (should (s-contains-p test-type actual-alias-name))
    (should (s-starts-with-p "(defalias" actual-alias))
    (should (s-ends-with-p ")" actual-alias))
    (should (s-contains-p test-lang actual-alias))
    (should (equal (s-contains-p "sub" actual-alias) test-subheadlinep))))

(generate-ert-deftest-n-times org-x--create-list-of-tree-sitter-yank-last-as-functions-for-lang ()
  :num-runs 100
  (-let* ((((_ __ test-type) expected-index) (generate-seq-random-value-with-position
							org-x--TEST-TREE-SITTER-FUNC-CASES))
	  (test-lang (generate-random-language-name))
	  (test-headline-level (generate-random-org-headline-level))
	  (actual-funcs (org-x--create-list-of-tree-sitter-yank-last-as-functions-for-lang
			 test-lang test-headline-level))
	  (actual-headline-with-src-block (nth (1+ expected-index) actual-funcs)))
    (should (s-starts-with-p (org-x--create-headline-stars
			      (1+ test-headline-level))
			     actual-headline-with-src-block))
    (should (s-ends-with-p "#+end_src" actual-headline-with-src-block))
    (should (length= actual-funcs 5))
    (should (s-contains-p test-lang actual-headline-with-src-block))))

(generate-ert-deftest-n-times org-x--create-string-of-tree-sitter-yank-last-as-functions ()
  :num-runs 100
  (let* ((test-lang (generate-random-language-name))
	 (expected-top-headline (format "**** %s" test-lang))
	 (actual-headlines (org-x--create-string-of-tree-sitter-yank-last-as-functions (list test-lang))))
    (should (s-starts-with-p expected-top-headline actual-headlines))
    (should (s-ends-with-p "#+end_src" actual-headlines))
    (should (equal (s-count-matches (regexp-quote "#+begin_src") actual-headlines) 4))
    (should (equal (s-count-matches (regexp-quote "#+end_src") actual-headlines) 4))))



(generate-ert-deftest-n-times org-x-get-directory-files-as-headlines ()
  :num-runs 0
  (let* (((test-directory expected-nonempty-folders expected-empty-folders expected-depth expected-total-length) (generate--random-list-of-directory-files))
	 (test-minimum-headline-level (generate-random-nat-number-in-range 1 10))
	 (test-minimum-headline-level-string (make-string test-minimum-headline-level-string (string-to-char "*")))
	 (actual-headlines (org-x-get-directory-files-as-headlines test-directory test-minimum-headline-level-string))
	 (actual-split-headlines (s-split "\n" actual-headlines))
	 (actual-min-headline-level)
	 (actual-max-headline-level))
    (should (length= actual-split-headlines expected-length))
    (should (equal actual-min-headline-level test-min-headline-level))
    (should (equal actual-max-headline-level expected-max-headline-level))
    (should (equal actual-random-nonempty-folder-child-depth expected-random-nonempty-folder-child-depth))
    (should (equal actual-random-empty-folder-children 0))))

;;; generate-check.el ends here
