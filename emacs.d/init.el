;;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2011-2018 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Prelude is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "25.1")
  (error "Prelude requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-core-dir (expand-file-name "core" prelude-dir)
  "The home of Prelude's core functionality.")
(defvar prelude-modules-dir (expand-file-name  "modules" prelude-dir)
  "This directory houses all of the built-in Prelude modules.")
(defvar prelude-personal-dir (expand-file-name "personal" prelude-dir)
  "This directory is for your personal configuration.

Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defvar prelude-personal-preload-dir (expand-file-name "preload" prelude-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Prelude.")
(defvar prelude-vendor-dir (expand-file-name "vendor" prelude-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-personal-dir)
  "This file contains a list of modules that will be loaded by Prelude.")
(defvar prelude-deprecated-modules-file
  (expand-file-name "prelude-modules.el" prelude-dir)
  (format "This file may contain a list of Prelude modules.

This is DEPRECATED, use %s instead." prelude-modules-file))

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

(defun prelude-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (prelude-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path prelude-vendor-dir)
(prelude-add-subfolders-to-load-path prelude-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `prelude-personal-preload-dir'
(when (file-exists-p prelude-personal-preload-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-preload-dir)
  (mapc 'load (directory-files prelude-personal-preload-dir 't "^[^#\.].*el$")))

(message "Loading Prelude's core...")

;; the core stuff
(require 'prelude-packages)
(require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui
(require 'prelude-ui)
(require 'prelude-core)
(require 'prelude-mode)
(require 'prelude-editor)
(require 'prelude-global-keybindings)

;; macOS specific settings
(when (eq system-type 'darwin)
  (require 'prelude-macos))

;; Linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'prelude-linux))

(message "Loading Prelude's modules...")

;; the modules
(if (file-exists-p prelude-modules-file)
    (progn
      (load prelude-modules-file)
      (if (file-exists-p prelude-deprecated-modules-file)
          (message "Loading new modules configuration, ignoring DEPRECATED prelude-module.el")))
  (if (file-exists-p prelude-deprecated-modules-file)
      (progn
        (load prelude-deprecated-modules-file)
        (message (format "The use of %s is DEPRECATED! Use %s instead!"
                         prelude-deprecated-modules-file
                         prelude-modules-file)))
    (message "Missing modules file %s" prelude-modules-file)
    (message "You can get started by copying the bundled example file from sample/prelude-modules.el")))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (delete
               prelude-modules-file
               (directory-files prelude-personal-dir 't "^[^#\.].*\\.el$"))))

(message "Prelude is ready to do thy bidding, Master %s!" current-user)

;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (with-eval-after-load "enriched"
    (defun enriched-decode-display-prop (start end &optional param)
      (list start end))))

(prelude-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'prelude-tip-of-the-day))

;;; init.el ends here


;;(require 'package)
;;(setq load-prefer-newer t
;;      package-enable-at-startup nil
;;      package-archives
;;      '(("gnu" . "https:")))
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;package loading and repos
(require 'package)
(setq load-prefer-newer t
      package-enable-at-startup nil
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t
      use-package-always-ensure t) ;; makes sure thst every package is available

;; default stuff
(use-package company :ensure t)
(use-package company-quickhelp :ensure t)
(company-quickhelp-mode)
(use-package company-box
  :hook (company-mode . company-box-mode))
(scroll-bar-mode -1)
;; some company tweaks
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .1)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;; company mode autocompletion on first character
(setq company-minimum-prefix-length 2)
;; disable only lowercase in company autocompletion
(setq company-dabbrev-downcase nil)
;; disable whitespace mode by default
(global-whitespace-mode 0)
(whitespace-mode 0)
;; default size
(when window-system
;;  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 150 50))
;; tramp settings
(use-package tramp
  :ensure t
  :config
  (setq tramp-verbose 9
        tramp-default-method "ssh"
        tramp-ssh-controlmaster-options
        (concat "-o ControlPath=/tmp/tramp.%%r@%%h:%%p "
                "-o ControlMaster=auto "
                "-o ControlPersist=no")))
;; osx fixes
(when (eq system-type 'darwin)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-key-is-meta t)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil))

;; for osx keyboards
;;(setq mac-command-modifier 'meta)
;;(setq mac-option-modifier 'super)
;; this is weird, fix apple keyboard
;;(global-set-key (kbd "s-l") '(lambda () (interactive) (insert "@")))
;;(global-set-key (kbd "s-5") '(lambda () (interactive) (insert "[")))
;;(global-set-key (kbd "s-6") '(lambda () (interactive) (insert "]")))
;;(global-set-key (kbd "s-7") '(lambda () (interactive) (insert "|")))
;;(global-set-key (kbd "s-8") '(lambda () (interactive) (insert "{")))
;;(global-set-key (kbd "s-9") '(lambda () (interactive) (insert "}")))
;; how to disable whitespace mode forever:
;; customize-group RET prelude
;; prelude-whitespace set to off (default is on)

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 12) ;; compile emacs with imagemagic

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))



(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

;;(use-package treemacs-magit
;;  :after treemacs magit
;;  :ensure t)

;; ruby
(use-package ruby-mode :ensure t)
(use-package flymake-ruby :ensure t)
(use-package projectile-rails :ensure t)

(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.zshrc && printf $PATH")))
(projectile-rails-global-mode)

(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(global-set-key (kbd "C-c r r") 'inf-ruby)

(setq seeing-is-believing-prefix "C-,")
(add-hook 'ruby-mode-hook 'seeing-is-believing)
(use-package seeing-is-believing :ensure t)

(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(use-package ruby-test-mode :ensure t)
(add-hook 'ruby-mode-hook 'ruby-test-mode)
;;(lambda (buf strg)
;;  (switch-to-buffer-other-window "*compilation*")
;;  (read-only-mode)
;;  (goto-char (point-max))
;;  (local-set-key (kbd "q")
;;                 (lambda () (interactive) (quit-restore-window)))))

;; robe for company and jump to
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; rubocop
(use-package rubocop :ensure t)

;; ruby settings
(setq ruby-insert-encoding-magic-comment nil)


;; python stuff
(use-package company-jedi :ensure t)
(defun my-python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi)
  development)
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; scala stuff
(use-package scala-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

;; node stuff (js3-mode)
(use-package js3-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))
(add-to-list 'company-backends 'company-tern)
(add-hook 'js3-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

;; webmode stuff
(use-package web-mode)
(defun my-web-mode-hook ()
  "Hook for `web-mode'."
  (set (make-local-variable 'company-backends)
       '(company-tern company-web-html company-yasnippet company-files)))
(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html))
                          (company-mode t)))

;; Enable JavaScript completion between <script>...</script> etc.
(advice-add 'company-tern :before
            #'(lambda (&rest _)
                (if (equal major-mode 'web-mode)
                    (let ((web-mode-cur-language
                           (web-mode-language-at-pos)))
                      (if (or (string= web-mode-cur-language "javascript")
                              (string= web-mode-cur-language "jsx"))
                          (unless tern-mode (tern-mode))
                        (if tern-mode (tern-mode -1)))))))

;; go stuff
(use-package go-mode)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

;; some sql mode fixes
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; some .tern-project file example for ~/.tern-project
;;{
;; "libs": [
;;          "browser",
;;          "jquery",
;;          "react",
;;          "chai",
;;          "ecma5",
;;          "ecma6"
;;          ],
;; "defs": [
;;          "browser",
;;          "ecma5",
;;          "ecma6"
;;          ],
;; "plugins": {
;; "doc_comment": {
;; },
;; "angular": {
;; },
;; "node": {
;; },
;; "commonjs": {
;; },
;; "complete_strings": {
;; },
;; "es_modules": {
;; },
;; "modules": {
;; },
;; "node_resolve": {
;; },
;; "requirejs": {
;; },
;; "webpack": {
;; }
;; },
;; "ecmaVersion": 6
;; }
