;;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2011-2017 Bozhidar Batsov
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

(when (version< emacs-version "24.4")
  (error "Prelude requires at least GNU Emacs 24.4, but you're running %s" emacs-version))

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
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-dir)
  "This files contains a list of modules that will be loaded by Prelude.")

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

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'prelude-osx))

(message "Loading Prelude's modules...")

;; the modules
(if (file-exists-p prelude-modules-file)
    (load prelude-modules-file)
  (message "Missing modules file %s" prelude-modules-file)
  (message "You can get started by copying the bundled example file"))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (directory-files prelude-personal-dir 't "^[^#\.].*el$")))

(message "Prelude is ready to do thy bidding, Master %s!" current-user)

(prelude-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'prelude-tip-of-the-day))

;;; init.el ends here

;;; CUSTOM INIT

;; melpa stuff
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)


;;; Uncomment the modules you'd like to use and restart Prelude afterwards
;(require 'prelude-c)
;(require 'prelude-clojure)
;; (require 'prelude-coffee)
;; (require 'prelude-common-lisp)
;; (require 'prelude-css)
;(require 'prelude-emacs-lisp)
;(require 'prelude-erc)
;; (require 'prelude-erlang)
;; (require 'prelude-elixir)
;; (require 'prelude-haskell)
;; (require 'prelude-js)
;; (require 'prelude-latex)
;(require 'prelude-lisp)
;; (require 'prelude-mediawiki)
;(require 'prelude-org)
;(require 'prelude-perl)
;(require 'prelude-python)
;(require 'prelude-ruby)
;(require 'prelude-scala)
;(require 'prelude-scheme)
;; (require 'prelude-scss)
;; (require 'prelude-web)
;(require 'prelude-xml)


;; check if packages are installed and if not install
(defvar own-packages
  '(go-mode go-autocomplete android-mode flycheck js2-mode json-mode web-mode auto-complete tern-auto-complete jedi-core jedi typescript-mode tss rjsx-mode))

(defun own-packages-installed-p ()
  (loop for p in own-packages
        when (not (packages-installed-p p)) do (return nil)
        finally (return t)))

;; install if not installed
(dolist (p own-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; dirty fix for having AC everywhere
;(define-globalized-minor-mode real-global-auto-complete-mode
;  auto-complete-mode (lambda ()
;                       (if (not (minibufferp (current-buffer)))
;                           (auto-complete-mode 1))
;                       ))
;(real-global-auto-complete-mode t)


;set default modifier keys
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;; disable whitespace mode
(setq prelude-whitespace nil)

;; enable ac by default
(global-auto-complete-mode t)
;; add modes to auto-complete
;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;android MODE
;; Omit the next line if installed through ELPA
(require 'android-mode)
(custom-set-variables '(android-mode-sdk-dir "/usr/local/opt/android"))


;;go stuff
;; install go-code before
;; go install https://github.com/nsf/gocode
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

; set gopath somewhere
(add-hook 'before-save-hook 'gofmt-before-save)

;Call Gofmt before saving
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
;Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(defun my-go-mode-hook ()
; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; PHP stuff
(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


(add-hook 'php-mode-hook
          '(lambda ()
             (auto-complete-mode t)
             (require 'ac-php)
             (setq ac-sources  '(ac-source-php ) )
             (yas-global-mode 1)

             (ac-php-core-eldoc-setup ) ;; enable eldoc
             (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
             (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
             ))

;;;;;;;;;;; Typescript stuff
;; npm install -g typescript typescript-tools
;; If use bundled typescript.el,
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)

;; Key binding
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(setq tss-implement-definition-key "C-c i")

;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "tss")

;; Do setting recommemded configuration
(tss-config-default)


;;;;;;;;;;; ES6 stuff
; npm install -g eslint babel-eslint eslint-plugin-react tern
; use web-mode for .jsx files


(require 'web-mode)
;(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
;(setq web-mode-content-types-alist
;      '(("jsx"  . "\\.js?\\'")))

(require 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.js?\\'" . rjsx-mode))
(setq rjsx-mode-content-types-alist
      '(("jsx"  . "\\.js?\\'")))

;;http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)
;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")
;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; js indent and stuff
;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))



(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)
;; adjust indents for web-mode to 2 spaces
;(defun my-web-mode-hook ()
;  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
;  (setq web-mode-markup-indent-offset 2)
;  (setq web-mode-css-indent-offset 2)
;  (setq web-mode-code-indent-offset 2))
;(add-hook 'web-mode-hook  'my-web-mode-hook)

;(add-to-list 'load-path "~.emacs.d/tern/")

;(require 'auto-complete)


;(require 'tern-auto-complete)

(autoload 'tern-mode "tern.el" nil t)
(add-hook 'web-mode-hook (lambda () (tern-mode t)))

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))



(add-hook 'rjsx-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))


(add-hook 'web-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))




; add .tern-project :
;{ "plugins": {"node": {}}}

;; install following emacs packages:
;; flycheck, js2-mode, json-mode, web-mode, (osx -> exec-path-from-shell)




;; python development make sure virtual-env is install and
;; run jedi:install-server
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-p") 'prev-window)

(defun prev-window ()
  (interactive)
  (other-window -1))







