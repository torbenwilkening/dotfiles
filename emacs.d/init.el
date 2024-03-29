;;; init.el ends here

;;package loading and repos
(require 'package)
(setq load-prefer-newer t
      package-enable-at-startup nil
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t
      use-package-always-ensure t) ;; makes sure thst every package is available

(use-package exec-path-from-shell :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;; copy path
(exec-path-from-shell-copy-env "PATH")


;;;;;;;;;;;;;;;;;
;; defaults ;;;;;
;;;;;;;;;;;;;;;;;
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq package-check-signature nil)

;; backups/saves
(setq make-backup-files nil)
;(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
;; (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;(setq savehist-file "~/.emacs.d/savehist")
;;(savehist-mode 1)
;(setq history-length t)
;(setq history-delete-duplicates t)
;;(setq savehist-save-minibuffer-history 1)
;;(setq savehist-additional-variables
;;      '(kill-ring
;;        search-ring
;;        regexp-search-ring))

;; disable by default the toolbar
(tool-bar-mode -1)

;; enable battery mode
(display-battery-mode 1)

;; some special shortcuts

;; use CTRL+SHIFT+<arrow> for window movement
(define-key global-map (kbd "s-<up>") 'windmove-up)
(define-key global-map (kbd "s-<down>") 'windmove-down)
(define-key global-map (kbd "s-<left>") 'windmove-left)
(define-key global-map (kbd "s-<right>") 'windmove-right)

;; ibuffer with focus
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-b") 'ibuffer)
(global-set-key (kbd "M-b") 'switch-to-buffer)

;; better cursor movement
(global-set-key (kbd "M-<down>")
                (lambda () (interactive) (forward-line  5)))
(global-set-key (kbd "M-<up>")
                (lambda () (interactive) (forward-line -5)))

;; selection
;;(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; recent files selection
(global-set-key (kbd "C-c f") 'recentf-open-more-files)
(global-set-key (kbd "C-S-s") 'ag-project)

;; window splitting
(global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "C-2") 'split-window-vertically)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-0") 'delete-window)

(global-set-key (kbd "C-p") 'previous-buffer)
(global-set-key (kbd "C-n") 'next-buffer)

;; projectile
(global-set-key (kbd "C-c p s") 'projectile-switch-project)

;; debug keys
(global-set-key (kbd "C-d") 'dap-hydra)


;; undo key CTRL+u
(global-set-key (kbd "C-u") 'undo)

;; comment and uncomments
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

;; org mode
(setq org-agenda-files (quote ("~/Projects/org/calendar.org")))


;;; synchronize exchange to org agenda
;;ORGANISED_EXCHANGE_ORIGIN=~/Projects/calendar.ics
;;ORGANISED_EXCHANGE_DESTINATION=~/Projects/org/calendar.org

(defun organised-exchange ()
  "Sync Outlook Calendar ics with Org Agenda."
  (interactive)
  (if (get-buffer "calendar.org")
      (kill-buffer "calendar.org"))
  (shell-command "~/Projects/scripts/organised-exchange/bin/eto")
  (message "Calendar imported!"))




;; multi cursor
(use-package multiple-cursors :ensure t)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; other packages

;;;;;;;;;;;;;;;;;;;;;
;; company ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;


(use-package company
  :ensure t
  :defer t
  :init (global-company-mode t)
  :config
  (setq company-tooltip-limit 5
	company-idle-delay .1
	company-echo-delay 0
	completion-ignore-case nil
	company-minimum-prefix-length 1 ; only after first character
	company-begin-commands '(self-insert-command))) ; only after typing
(setq-local company-dabbrev-downcase nil)  ;; removed downcase annoyance


(use-package company-box
  :init (setq company-box-enable-icon (display-graphic-p))
  :hook (company-mode . company-box-mode))
;; go to ~/.emacs.d/elpa/26.1/develop/company-box-xxx/images
;; and run mogrify -resize 50% *.png if images are too big


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck syntax checks here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (global-flycheck-mode t))
(use-package flycheck-color-mode-line
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


;; default scrollbar
(scroll-bar-mode -1)

;; disable company annoyance on enter / tab
;(setq tab-always-indent t)

;; ansi-term stuff
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; disable whitespace mode by default
(global-whitespace-mode 0)
(whitespace-mode 0)
;; default size
(when window-system
;;  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 150 60))
;; tramp settings
(use-package tramp
  :ensure t
  :config
  (setq tramp-verbose 9
        tramp-default-method "ssh"))

;; which-key show possible commands in minibuffer
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

;; yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)


;; osx fixes
(when (eq system-type 'darwin)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-key-is-meta t)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil))
;; a little bit window transparency
(set-frame-parameter (selected-frame) 'alpha '(99 99))

;; disable guru mode by default
;; M-x customize-option prelude-guru -> disable it

;; only emacs 26
;;(when (version<= "26.0.50" emacs-version )
;;  (global-display-line-numbers-mode))

;; how to disable whitespace mode forever:
;; customize-group RET prelude
;; prelude-whitespace set to off (default is on)

;; get linum right
(use-package nlinum
  :init
  (progn
    (add-hook 'prog-mode-hook 'nlinum-mode)
    (add-hook 'text-mode-hook 'nlinum-mode)
    (setq nlinum-format "%4d")))
;; show some git changes behind linum
(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :init (global-git-gutter-mode))

;; to check history
(use-package git-timemachine
  :ensure t
  :defer t)

;;change management
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
    (setq undo-tree-visualizer-diff t)))

(use-package yasnippet :ensure t)

;; yasnippet
(use-package yasnippet :ensure t)
(yas-global-mode 1)

;;; searching
(use-package ag
  :commands (ag ag-files ag-regexp ag-project ag-dired helm-ag)
  :config (setq ag-highlight-search t
                ag-reuse-buffers t))

;;; communication
(use-package circe :ensure t)


;;;;;;;;;;;;;;;;;;;;;
;; projectile etc ;;;
;;;;;;;;;;;;;;;;;;;;;

;; ivy
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (bind-key "C-c C-r" 'ivy-resume))


(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy)
  (projectile-mode))

(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-c s" . counsel-ag))

(use-package counsel-projectile
  :ensure t
  :config
  :bind ("C-c p f" . counsel-projectile-find-file)
  :config
  (counsel-projectile-mode))

(ivy-mode 1)

(use-package swiper :ensure t)
(global-set-key (kbd "C-s") 'swiper)  ;; replaces i-search with swiper
(global-set-key (kbd "M-x") 'counsel-M-x) ;; Gives M-x command counsel features
(global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; gives C-x C-f counsel features

;;;;;;;;;;;;;;;;;
;; magit ;;;;;;;;
;;;;;;;;;;;;;;;;;

(use-package magit :ensure t)
(global-set-key (kbd "C-c C-g") 'magit-status)
(add-hook 'magit-mode-hook (lambda () (local-set-key (kbd "C-o") #'magit-diff-visit-file-other-window)))
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)

(defun blame ()
  (interactive)
  (when magit-buffer-file-name
    (user-error "Blob buffers aren't supported"))
  (setq-local magit-blame-show-headings nil)
  (let ((magit-blame-read-only nil))
    (magit-blame)))


;;;;;;;;;;;;;;;;;
;; treemacs ;;;;;
;;;;;;;;;;;;;;;;;

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
    (treemacs-resize-icons 10) ;; compile emacs with imagemagic

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

; all-the-icons

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))


;; (use-package treemacs-icons-material
;;   :after treemacs material
;;   :ensure t
;;   :config (treemacs-icons-material-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)


;;;;;;;;;;;;;;;;;
;; alltheicons ;;
;;;;;;;;;;;;;;;;;
(use-package all-the-icons :ensure t)
;; to use fonts run M-x
;; all-the-icons-install-fonts RET

;;;;;;;;;;;;;;;;;
;; doom theme ;;;
;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :config
  ;; global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t) ; change theme here (looks good: doom-material doom-one doom-dark+ doom-solarized-dark)
  ;; enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; modeline specific
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; M-x customize-group RET doom-modeline RET or set the variables
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-indent-info nil)
(setq doom-modeline-checker-simple-format t)
(setq doom-modeline-number-limit 99)
(setq doom-modeline-vcs-max-length 20)
(setq doom-modeline-persp-name nil)
(setq doom-modeline-display-default-persp-name nil)
(setq doom-modeline-lsp t)

;; Whether display the GitHub notifications. It requires `ghub' package.
(setq doom-modeline-github nil)
;;(setq doom-modeline-github-interval (* 30 60))
(setq doom-modeline-github-interval nil)

;; Whether display the modal state icon.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal-icon nil)
;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
(setq doom-modeline-mu4e nil)
;; Whether display the gnus notifications.
(setq doom-modeline-gnus t)
;; Wheter gnus should automatically be updated and how often (set to nil to disable)
;;(setq doom-modeline-gnus-timer 2)
(setq doom-modeline-gnus-timer nil)
;; Whether display the IRC notifications. It requires `circe' or `erc' package.
(setq doom-modeline-irc t)
;; Function to stylize the irc buffer names.
;; (setq doom-modeline-irc-stylize 'identity)

;; Whether display the environment version.
(setq doom-modeline-env-version t)
;; Or for individual languages
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-perl t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-enable-elixir t)
(setq doom-modeline-env-enable-rust t)

;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")

;; What to dispaly as the version while a new one is being loaded
(setq doom-modeline-env-load-string "...")

;; Hooks that run before/after the modeline version string is updated
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)

;; Nice Dashboard when you start emacs
(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "Welcome my master")
  (setq dashboard-items '((projects . 10)
                          (bookmarks . 5)
                          (recents  . 5)))
  (dashboard-setup-startup-hook))
(add-to-list 'dashboard-items '(agenda) t)

;; Set the banner
(setq dashboard-startup-banner "~/.logo.png")
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.png" which displays whatever image you would prefer
(setq dashboard-center-content t)




;; Show new mails in modeline
;; (use-package mu4e-alert :ensure t)
;; Choose the style you prefer for desktop notifications
;; If you are on Linux you can use
;; 1. notifications - Emacs lisp implementation of the Desktop Notifications API
;; 2. libnotify     - Notifications using the `notify-send' program, requires `notify-send' to be in PATH
;; On Mac OSX you can set style to
;; 1. notifier      - Notifications using the `terminal-notifier' program, requires `terminal-notifier' to be in PATH
;; 1. growl         - Notifications using the `growl' program, requires `growlnotify' to be in PATH
;; (if (eq system-type 'darwin) (mu4e-alert-set-default-style 'notifier))
;; (if (eq system-type 'gnu/linux) (mu4e-alert-set-default-style 'libnotify))
;; (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)


(use-package pretty-hydra :ensure t)
;(use-package hydra
;  :ensure t
;  :defer t
;  :init
;  (defhydra hydra-zoom (global-map "<f2>")
;    "zoom"
;    ("g" text-scale-increase)
;    ("l" text-scale-decrease)))

;; dashboard keys
;;(global-set-key (kbd "C-o") (lambda () (interactive) (switch-to-buffer "*dashboard*")
;;(global-set-key (kbd "C-o") (lambda () (interactive) (my-dashboard-hydra/body)))

(pretty-hydra-define my-default-hydra
  (:color blue :quit-key "q")
   (
    "General"
    (("o" (lambda () (interactive) (switch-to-buffer "*dashboard*")) "Go to Dashboard" :toggle nil)
     ("s" (lambda () (interactive) (switch-to-buffer "*scratch*")) "Go to Scratch Buffer" :toggle nil)
     ("7" comment-or-uncomment-region "(Un)comment Region" :toggle nil)
     ("f", indent-region "Indent Region" :toggle nil)
     ("h" git-timemachine :toggle nil))
    "Ruby"
    (("r" rubocop-mode-check-current-file "Check Rubocop" :toggle nil)
     ("i" inf-ruby "REPL" :toggle nil)
     ("j" projectile-rails-find-current-test "Go to Spec" :toggle nil)
     ("t" run-rspec-test-at-point "Run at point" :toggle nil)
     ("T" run-rspec-test-file "Run all" :toggle nil))
    )
   )
(global-set-key (kbd "C-r") (lambda () (interactive) (my-default-hydra/body)))

;; text scaling
;;(defhydra hydra-zoom (global-map "C-+")
;;  "zoom"
;;  ("+" text-scale-increase "in")
;;  ("-" text-scale-decrease "out"))



;;;;;;;;;;;;;;;;;
;; lsp ;;;;;;;;;;
;;;;;;;;;;;;;;;;;

;; check supported languages: https://github.com/emacs-lsp/lsp-mode
;; for ruby: gem install solargraph
;; for python: pip3 install python-language-server
;; for vue.js: npm install -g vue-language-server
;; for typescript: npm i -g javascript-typescript-langserver
;; for go: go get golang.org/x/tools/gopls@latest
;; for Dockerfile: npm install -g dockerfile-language-server-nodejs
;; for less/sass: npm install -g vscode-css-languageserver-bin
;; for html: npm install -g vscode-html-languageserver-bin
;; for json: npm i -g vscode-json-languageserver
;; for yaml: npm install -g yaml-language-server

(use-package lsp-mode
  :ensure t
  :init (setq lsp-inhibit-message t
              lsp-eldoc-render-all t
              lsp-keymap-prefix "s-l")
  :hook
  (html-mode . lsp)
  (css-mode . lsp)
  (less-css-mode . lsp)
  (scss-mode . lsp)
  (enh-ruby-mode . lsp)
  (ruby-mode . lsp)
  (scala-mode . lsp)
  (java-mode . lsp)
  (python-mode . lsp)
  (groovy-mode . lsp)
  (yaml-mode . lsp)
  (json-mode . lsp)
  (web-mode . lsp)
  (js-mode . lsp)
  (sh-mode . lsp)
  (elixir-mode . lsp)
  (go-mode . lsp)
  (dockerfile-mode . lsp)
  (vue-mode . lsp)
  (vue-html-mode . lsp)
  (markdown-mode . lsp)
  ;;(graphql-mode . lsp)
  (lsp-mode . lsp-lens-mode))

;; install vscode language servers
;; npm install -g dockerfile-language-server-nodejs
;; npm install -g yaml-language-server
;; npm install -g vscode-css-languageserver-bin
;; npm install -g vscode-html-languageserver-bin
;; npm install -g dockerfile-language-server-nodejs
;; npm install -g vue-language-server

;; this is used for "error running timer lsp--on-idle" why?
(setq lsp-enable-links nil)



(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable nil
	
	;; some sideline tweaks if you want to enable it
	;; shows only lint and errors in the sideline
	lsp-ui-sideline-show-code-actions nil
	lsp-ui-sideline-show-symbol nil
	
	lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-show-hover nil
	lsp-eldoc-hook nil ;; disable in minibuffer
        lsp-ui-sideline-update-mode 'point))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;;(use-package company-lsp
;;  :after  company
;;  :ensure t
;;  :config
;;  (add-hook 'java-mode-hook (lambda () (push 'company-lsp company-backends)))
;;  (add-hook 'python-mode-hook (lambda () (push 'company-lsp company-backends)))
;;  (add-hook 'groovy-mode-hook (lambda () (push 'company-lsp company-backends)))
  ;;(add-hook 'ruby-mode-hook (lambda () (push 'company-lsp company-backends)))
;;  (add-hook 'enh-ruby-mode-hook (lambda () (push 'company-lsp company-backends)))
;;  (add-hook 'scala-mode-hook (lambda () (push 'company-lsp company-backends)))
;;  (add-hook 'yaml-mode-hook (lambda () (push 'company-lsp company-backends)))
;;  (add-hook 'json-mode-hook (lambda () (push 'company-lsp company-backends)))
;;  (add-hook 'web-mode-hook (lambda () (push 'company-lsp company-backends)))
;;  (add-hook 'sh-mode-hook (lambda () (push 'company-lsp company-backends)))
;;  (add-hook 'go-mode-hook (lambda () (push 'company-lsp company-backends)))
;;  (add-hook 'dockerfile-mode-hook (lambda () (push 'company-lsp company-backends)))
;;  (setq company-lsp-enable-snippet t
;;        company-lsp-cache-candidates t))



;; keys for find references and definitions to
(define-key lsp-ui-mode-map (kbd "C-RET") #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map (kbd "<C-return>") #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map (kbd "M-RET") #'lsp-ui-peek-find-references)
(define-key lsp-ui-mode-map (kbd "<M-return>") #'lsp-ui-peek-find-references)

;;(push 'company-lsp company-backends)
;;(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; flycheck lsp
;; (require 'lsp-ui-flycheck)
;(with-eval-after-load 'lsp-mode
;  (add-hook 'lsp-after-open-hook (lambda () (lsp-flycheck-enable t))))

;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;;;;;;;;;;;;;;;;;
;;; dap-mode ;;;;
;;;;;;;;;;;;;;;;;
(use-package dap-mode :ensure t)

(dap-ui-mode 1)
;; enables mouse hover support
(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
(tooltip-mode 1)
;; displays floating panel with debug buttons
;; requies emacs 26+
(dap-ui-controls-mode 1)

;; dap-ruby

(require 'dap-ruby)
;; call dap-ruby-setup

;; python

(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))
(require 'dap-python)
;; call `pip install jedi`


(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")


(use-package css-mode
  :ensure t
  :mode "\\.css\\'")


(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")


(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\|\\.yml\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json")

(use-package graphql-mode
  :ensure t)

;;(use-package flycheck :ensure t
;;  :config
;;  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


;; auto add end and close parens
(use-package smartparens :ensure t)
(require 'smartparens-config)
(smartparens-global-mode)
(show-smartparens-global-mode t)


;;;;;;;;;;;;;;;;;
;;; groovy;;;;;;;
;;;;;;;;;;;;;;;;;
(use-package groovy-mode :ensure t)


;;;;;;;;;;;;;;;;;
;;; ruby ;;;;;;;;
;;;;;;;;;;;;;;;;;


;; ruby test functions
  (defun run-rspec-test-file ()
    (interactive)
    (let ((filename (ruby-test-find-file))
	  (default-directory (ruby-test-rails-root (ruby-test-find-file))))
      (ruby-test-run-command (ruby-test-spec-command filename))))

  (defun run-rspec-test-at-point ()
    (interactive)
    (let* ((filename (ruby-test-find-file))
	   (default-directory (ruby-test-rails-root filename))
           (buffername (get-file-buffer filename)))
      (with-current-buffer buffername
        (let ((line (line-number-at-pos (point))))
	  (ruby-test-run-command (ruby-test-spec-command filename line))))
      )
    )

(setq compilation-window-height 12)
(global-unset-key (kbd "C-r"))

(use-package enh-ruby-mode
  :ensure t
;;  :bind (("C-r c" . inf-ruby)
;;	 ("C-r f" . projectile-rails-goto-file-at-point)
;;	 ("C-r t" . ruby-test-run-at-point)
;;	 ("C-r r" . rake)
  ;;	 ("C-r s" . projectile-rails-find-current-spec))
  )

(use-package smartparens-ruby :ensure smartparens)
(use-package ruby-test-mode :ensure t)
(use-package rubocop :ensure t)
(use-package bundler :ensure t)
(use-package rbenv :ensure t)
(use-package rake :ensure t)

(setq ruby-test-execution-environment "RAILS_ENV=test")

;; some flycheck fix, make sure in /usr/local/bin/rubocop is this script:
;#!/bin/bash
;bundle exec rubocop "$@"

(setq flycheck-ruby-rubocop-executable "/usr/local/bin/rubocop")

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'")


(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))

(use-package projectile-rails :ensure t)
(projectile-rails-global-mode)
(setq enh-ruby-add-encoding-comment-on-save nil
      enh-ruby-deep-indent-paren nil
      enh-ruby-deep-indent-construct nil
      enh-ruby-hanging-brace-indent-level 2)

(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.zshrc && printf $PATH")))

(add-to-list 'load-path "~/.emacs.d/vendor")

;; ruby mode
(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . enh-ruby-mode))
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'enh-ruby-mode-hook 'flycheck-mode)
(add-hook 'enh-ruby-mode-hook 'ruby-test-mode)
(add-hook 'enh-ruby-mode-hook #'rubocop-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup) ;; enable byebug and pry

(add-hook 'enh-ruby-mode-hook #'lsp-ui-sideline-mode)

;; ruby settings
(setq ruby-insert-encoding-magic-comment nil)
;; remove default faces for enh-ruby-mode
;;(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)


;;;;;;;;;;;;;;;;;
;;; scala ;;;;;;;
;;;;;;;;;;;;;;;;;

(use-package scala-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )
;; Add metals backend for lsp-mode
(use-package lsp-metals
  :config (setq lsp-metals-treeview-show-when-views-received t))


;;;;;;;;;;;;;;;;;
;;; elixir (use-package elixir-mode :ensure t);;;;;;
;;;;;;;;;;;;;;;;;
(use-package elixir-mode :ensure t :init (add-to-list 'exec-path "~/src/elixir-ls"))
(use-package alchemist :ensure t)




(defvar just-tab-keymap (make-sparse-keymap) "Keymap for just-tab-mode")
(define-minor-mode just-tab-mode
  "Just want the TAB key to be a TAB"
  :global t :lighter " TAB" :init-value 0 :keymap just-tab-keymap
  (define-key just-tab-keymap (kbd "TAB") 'self-insert-command))




(setq-default indent-tabs-mode nil)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; typescript and javascript ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(use-package nvm :ensure t)

(use-package vue-mode
  :ensure t
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (css-indent-offset 2)
  (indent-tabs-mode nil)
  (js-indent-level 2)
  :config
  ;; https://github.com/dgutov/mmm-mode/issues/99
  (add-to-list 'mmm-save-local-variables '(syntax-ppss-table buffer))

  (add-hook 'vue-mode-hook #'lsp)
  (add-hook 'vue-mode-hook #'lsp-ui-sideline-mode))

;; webmode with js / ts / vue minor modes
(use-package web-mode
  :custom
  (electric-indent-mode nil)
  (lsp-enable-indentation nil)
  (web-mode-enable-auto-indentation nil)
  (lsp-enable-on-type-formatting nil)
;;  :config
  ;; https://github.com/dgutov/mmm-mode/issues/99
;;  (add-to-list 'mmm-save-local-variables '(syntax-ppss-table buffer))


  :ensure t)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))

;; vue mode fix
;;(add-to-list 'mmm-save-local-variables '(c-current-comment-prefix region))

;;;;;;;;;;;;;;;;;
;;; mermaid js ;;
;;;;;;;;;;;;;;;;;

;; install mermaid cli: https://github.com/mermaidjs/mermaid.cli
(use-package mermaid-mode :ensure t)
;; open mmd files and start mermaid mode
;; useful shortcuts in mermaid-mode
;; C-c C-c to compile to an image
;; C-c C-o to open in the live editor
;; C-c C-d to open the official doc


;;;;;;;;;;;;;;;;;
;;; go ;;;;;;;;;;
;;;;;;;;;;;;;;;;;

(use-package go-mode :ensure t)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))


;;;;;;;;;;;;;;;;;
;;; sql ;;;;;;;;;
;;;;;;;;;;;;;;;;;
(use-package sqlup-mode :ensure t)
(use-package sql-indent :ensure t)

(add-hook 'sql-mode-hook 'lsp)
(add-hook 'sql-mode-hook 'sqlup-mode)
;(add-hook 'sql-mode-hook 'sql-indent)

;;(setq lsp-sqls-workspace-config-path nil)
;;(setq lsp-sqls-connections
;;    '(((driver . "mysql") (dataSourceName . "root@tcp(127.0.0.1:3306)/dbname"))))
;;(use-package sql-mode
;;  :ensure t
;;  :custom
;;  (sql-capitalize-keywords t)
;;  (sql-backend 'lsp)
;;  (lsp-sqls-workspace-config-path nil))


;; some fixes
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))



;; osx fix
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))



;; fix for some reasons
(setq lsp-enabled-clients nil)

(setq c-basic-offset 2)


(exec-path-from-shell-copy-env "RUBYOPT")
(exec-path-from-shell-copy-env "RAILS_ENV")

(exec-path-from-shell-copy-env "GO_PATH")
(exec-path-from-shell-copy-env "GO_ROOT")

(exec-path-from-shell-copy-env "PATH")

