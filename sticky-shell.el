;;; sticky-shell.el --- Minor mode to add a sticky header to your shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Andrew De Angelis

;; Author: Andrew De Angelis <bobodeangelis@gmail.com>
;; Maintainer: Andrew De Angelis <bobodeangelis@gmail.com>
;; URL: https://github.com/andyjda/sticky-shell
;; Version: 0.01.0
;; Package-Requires: ((eshell "2.4.2"))
;; Keywords: processes, terminals, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a minor mode that creates a header in a shell buffer.
;; The header shows a previous prompt according to the value of
;; `sticky-shell-get-prompt'.
;; This is most useful when working with many lines of output:
;; setting `sticky-shell-get-prompt' to `sticky-shell-prompt-above-visible'
;; will ensure that the command corresponding to the top output line
;; is always visible.
;; The look and properties of the prompt in the header can be changed
;; by the list of functions in `sticky-shell-prompt-modifiers'

;;; Code:
(eval-when-compile
  (require 'eshell)
  (require 'comint))

(declare-function eshell-previous-prompt "ext:eshell")
(declare-function comint-previous-prompt "ext:comint")

(defgroup sticky-shell nil
  "Display a sticky header with latest shell-prompt."
  :group 'terminals)


(defcustom sticky-shell-get-prompt
  #'sticky-shell-prompt-above-visible
  "Function used by sticky-shell-mode to pick the prompt to show in the header.
Available values are: `sticky-shell-latest-prompt',
`sticky-shell-prompt-above-visible',
`sticky-shell-prompt-above-cursor',
`sticky-shell-prompt-before-cursor'
or you can write your own function and assign it to this variable."
  :group 'sticky-shell
  :type 'function)

(defcustom sticky-shell-prompt-modifiers
  ()
  "List of functions modifying the prompt before it is displayed in the header.
See `sticky-shell-modified-prompt' for an explanation
on how the functions are applied."
  :group 'sticky-shell
  :type 'list)

(defun sticky-shell-prompt-current-line ()
  "Return the current line and remove the trailing newline char."
  (let ((prompt (thing-at-point 'line)))
    (aset prompt (- (length prompt) 1) 0) ; remove the newline ending char
    prompt))

(defun sticky-shell-latest-prompt ()
  "Get the latest prompt that was run."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (if (derived-mode-p 'eshell-mode)
        (eshell-previous-prompt 1)
      (comint-previous-prompt 1))
    (sticky-shell-prompt-current-line)))

(defun sticky-shell-prompt-above-visible ()
  "Get the prompt above the top visible line in the current window."
  (interactive)
  (save-excursion
    (goto-char (window-start))
    (if (derived-mode-p 'eshell-mode)
        (eshell-previous-prompt 1)
      (comint-previous-prompt 1))
    (sticky-shell-prompt-current-line)))

(defun sticky-shell-prompt-above-cursor ()
  "Get the prompt above the cursor's current line."
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (if (derived-mode-p 'eshell-mode)
        (eshell-previous-prompt 1)
      (comint-previous-prompt 1))
    (sticky-shell-prompt-current-line)))

(defun sticky-shell-prompt-before-cursor ()
  "Get the prompt before the cursor's current location.
This means that if the cursor is inside a prompt (even as it is being written),
this is the prompt that will be returned."
  (interactive)
  (save-excursion
    (if (derived-mode-p 'eshell-mode)
        (eshell-previous-prompt 1)
      (comint-previous-prompt 1))
    (sticky-shell-prompt-current-line)))

(defmacro sticky-shell-modified-prompt ()
  "Get the prompt, modify it, and return it.
Using `sticky-shell-get-prompt' and `sticky-shell-prompt-modifiers'.
The functions are applied in the order they appear in the list.
Note that since they are applied inside a `thread-first' macro,
they can be quoted functions accepting a single argument,
or quoted forms missing the first argument.
During evaluation, the argument will be the prompt.
eg: (#\\='upcase (propertize \\='face \\='minibuffer-prompt))
macro-expands to:

\(propertize
 (upcase
  (funcall sticky-shell-get-prompt))
 \\='face \\='minibuffer-prompt)"
  (if sticky-shell-prompt-modifiers
      `(thread-first
         (funcall sticky-shell-get-prompt)
         ,@sticky-shell-prompt-modifiers)
    (funcall sticky-shell-get-prompt)))

;;;###autoload
(define-minor-mode sticky-shell-mode
  "Minor mode to show the previous prompt as a sticky header."
  :group 'comint
  :global nil
  :lighter nil
  (if sticky-shell-mode
      (setq-local header-line-format
                  (list '(:eval
                          (sticky-shell-modified-prompt))))
    (setq-local header-line-format nil)))

(provide 'sticky-shell)
;;; sticky-shell.el ends here
