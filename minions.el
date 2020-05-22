;;; minions.el --- A minor-mode menu for the mode line  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/tarsius/minions

;; Package-Requires: ((emacs "25.2") (dash "2.13.0"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements a menu that lists enabled minor-modes, as
;; well as commonly but not currently enabled minor-modes.  It can be
;; used to toggle local and global minor-modes, to access mode-specific
;; menus, and to get help about modes.

;; This menu is intended as a replacement for the incomplete yet wide
;; list of enabled minor-modes that is displayed in the mode line by
;; default.  To use the menu like this, enable Minions mode.

;; Alternatively the menu can be bound globally, for example:
;;   (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu)

;;; Code:

(require 'cl-lib)
(require 'seq)

(eval-when-compile
  (require 'subr-x))

;;; Options

(defgroup minions nil
  "A minor-mode menu for the mode line."
  :group 'mode-line)

(defcustom minions-blacklist nil
  "List of minor-modes that are never shown in the mode menu.

These modes are not even displayed when they are enabled."
  :group 'minions
  :type '(repeat (symbol :tag "Mode")))

(defcustom minions-whitelist
  ;; Based on elements of `mode-line-mode-menu'.
  '((abbrev-mode . nil)
    (auto-fill-mode . nil)
    (auto-revert-mode . nil)
    (auto-revert-tail-mode . nil)
    (flyspell-mode . nil)
    (font-lock-mode . nil)
    (glasses-mode . nil)
    (hide-ifdef-mode . nil)
    (highlight-changes-mode . nil)
    (outline-minor-mode . nil)
    (overwrite-mode . nil)
    (ruler-mode . nil))
  "List of minor-modes that are always shown in the mode menu.

These modes are displayed even when they are not enabled,
provided they are at least autoloaded.  Elements have the
form (MODE . SCOPE), where SCOPE should be t if MODE is a
global minor-mode, nil otherwise."
  :group 'minions
  :type '(repeat (cons (symbol  :tag "Mode")
                       (boolean :tag "Scope"
                                :on "global (non-nil)"
                                :off "local (nil)"))))

(defcustom minions-direct nil
  "List of minor-modes that are shown directly in the mode line."
  :group 'minions
  :type '(repeat (symbol :tag "Mode")))

(defcustom minions-mode-line-face nil
  "Face used for the mode menu in the mode line."
  :package-version '(minions . "0.3.2")
  :group 'minions
  :group 'mode-line-faces
  :type '(choice (const :tag "No face" nil) face))

(defcustom minions-mode-line-lighter ";-"
  "Text used for the mode menu in the mode line."
  :package-version '(minions . "0.2.0")
  :group 'minions
  :type 'string)

(defcustom minions-mode-line-delimiters '("(" . ")")
  "Strings placed around mode elements in the mode line."
  :package-version '(minions . "0.3.1")
  :group 'minions
  :type '(choice (const :tag "No delimiters")
                 (cons (string :tag "Before string")
                       (string :tag "After string"))))

;;; Mode

;;;###autoload
(define-minor-mode minions-mode
  "Display a minor-mode menu in the mode line.

This replaces the likely incomplete and possibly cut off list of
minor-modes that is usually displayed directly in the mode line."
  :group 'minions
  :global t
  (if minions-mode
      (setq-default mode-line-format
                    (cl-subst 'minions-mode-line-modes
                              'mode-line-modes
                              (default-value 'mode-line-format)
                              :test #'equal))
    (cl-nsubst 'mode-line-modes
               'minions-mode-line-modes
               mode-line-format)))

;;; Menu

(defvar minions-mode-line-minor-modes-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'minions-minor-modes-menu)
    map))

(defvar minions-mode-line-modes
  (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
    (list (propertize "%[" 'help-echo recursive-edit-help-echo)
          '(:eval (car minions-mode-line-delimiters))
          `(:propertize ("" mode-name)
                        help-echo "Major mode
mouse-1: Display major mode menu
mouse-2: Show help for major mode
mouse-3: Toggle minor modes"
                        mouse-face mode-line-highlight
                        local-map ,mode-line-major-mode-keymap)
          '("" mode-line-process)
          (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                      'mouse-face 'mode-line-highlight
                      'local-map (make-mode-line-mouse-map
                                  'mouse-2 #'mode-line-widen))
          `(:propertize ("" (:eval (seq-filter (pcase-lambda (`(,mode))
                                                 (memq mode minions-direct))
                                               minor-mode-alist)))
			mouse-face mode-line-highlight
			help-echo "Minor mode
mouse-1: Display minor mode menu
mouse-2: Show help for minor mode
mouse-3: Toggle minor modes"
			local-map ,mode-line-minor-mode-keymap)
          " "
          '(:eval (propertize minions-mode-line-lighter
                              'face minions-mode-line-face
                              'mouse-face 'mode-line-highlight
                              'help-echo "Minions
mouse-1: Display minor modes menu"
                              'local-map minions-mode-line-minor-modes-map))
          '(:eval (cdr minions-mode-line-delimiters))
          (propertize "%]" 'help-echo recursive-edit-help-echo)
          " "))
  "Alternative mode line construct for displaying major and minor modes.
Similar to `mode-line-modes' but instead of showing (a subset
of) the enable minor modes directly in the mode line, list all
minor modes in a space conserving menu.")

(put 'minions-mode-line-modes 'risky-local-variable t)
(make-variable-buffer-local 'minions-mode-line-modes)

(defun minions-minor-modes-menu (event)
  "Pop up a menu with minor mode menus and toggles.

The menu has an entry for every enabled minor mode, except those
that are listed in `minions-blacklist'.  It also has entries for
modes that are not enabled but listed in `minions-whitelist'.
If a mode defines a menu, then its entry shows that as a submenu.
Otherwise the entry can only be used to toggle the mode.

EVENT has to be an input event."
  (interactive "@e")
  (pcase-let ((map (make-sparse-keymap))
              (`(,local ,global) (minions--modes)))
    (define-key map [minions--help-menu]
      (list 'menu-item "Describe..." (minions--help-menu)))
    (define-key map [describe-mode]
      (list 'menu-item "Describe modes" 'describe-mode))
    (define-key map [--help]  (list 'menu-item "Help"))
    (define-key map [--line1] (list 'menu-item "--double-line"))
    (dolist (mode global)
      (if-let (menu (and (symbol-value mode)
                         (minions--mode-menu mode)))
          (define-key map (vector mode) menu)
        (minions--define-toggle map mode)))
    (define-key map [--global] (list 'menu-item "Global Modes"))
    (define-key map [--line2]  (list 'menu-item "--double-line"))
    (dolist (mode local)
      (if-let (menu (and (symbol-value mode)
                         (minions--mode-menu mode)))
          (define-key map (vector mode) menu)
        (minions--define-toggle map mode)))
    (define-key map [--local] (list 'menu-item "Local Modes"))
    (x-popup-menu event map)))

(defun minions--modes ()
  (let (local global)
    (dolist (mode (cl-set-difference
                   (cl-union (cl-mapcan (pcase-lambda (`(,mode ,_))
                                          (and (boundp mode)
                                               (symbol-value mode)
                                               (list mode)))
                                        minor-mode-alist)
                             (cl-mapcan (pcase-lambda (`(,mode ,_))
                                          (and (boundp mode)
                                               (list mode)))
                                        minions-whitelist))
                   minions-blacklist))
      (if (or (local-variable-if-set-p mode)
              (let ((elt (assq mode minions-whitelist)))
                (and elt (not (cdr elt)))))
          (push mode local)
        (push mode global)))
    (list (sort local  #'string>)
          (sort global #'string>))))

(defun minions--mode-menu (mode)
  (let* ((map  (or (cdr (assq mode minor-mode-map-alist))
                   (cdr (assq mode minor-mode-overriding-map-alist))))
         (menu (and (keymapp map)
                    (lookup-key map [menu-bar])))
         (menu (and menu
                    (mouse-menu-non-singleton menu))))
    (and menu
         (let ((wrap (make-sparse-keymap)))
           (set-keymap-parent wrap menu)
           (define-key wrap [minions] (list 'menu-item "--double-line"))
           (minions--define-toggle wrap mode)
           (list 'menu-item (symbol-name mode) wrap)))))

(defun minions--define-toggle (map mode)
  (let ((fn (or (get mode :minor-mode-function) mode)))
    (when (functionp fn)
      (define-key map (vector mode)
        (list 'menu-item (symbol-name mode) fn
              :help (minions--documentation fn)
              :button (cons :toggle mode))))))

(defun minions--help-menu ()
  (pcase-let ((map (make-sparse-keymap))
              (`(,local ,global) (minions--modes)))
    (dolist (mode global)
      (minions--define-help map mode))
    (define-key map [--global] (list 'menu-item "Global Modes"))
    (define-key map [--line2]  (list 'menu-item "--double-line"))
    (dolist (mode local)
      (minions--define-help map mode))
    (define-key map [--local] (list 'menu-item "Local Modes"))
    map))

(defun minions--define-help (map mode)
  (let ((fn (or (get mode :minor-mode-function) mode)))
    (when (functionp fn)
      (define-key map (vector mode)
        (list 'menu-item
              (symbol-name mode)
              (lambda ()
                (interactive)
                (describe-minor-mode-from-symbol fn))
              :help (minions--documentation mode))))))

(defun minions--documentation (function)
  (let ((doc (documentation function t)))
    (and doc
         (string-match "\\`.+" doc)
         (match-string 0 doc))))

;;; Banana!
(provide 'minions)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; minions.el ends here
