
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

;;;;;;;;;;;;;;;;;
;; defaults ;;;;;
;;;;;;;;;;;;;;;;;

(use-package company :ensure t)
;;(use-package company-quickhelp :ensure t)
;;(company-quickhelp-mode)
(use-package company-box
  :init (setq company-box-enable-icon (display-graphic-p))
  :hook (company-mode . company-box-mode))
;; go to ~/.emacs.d/elpa/26.1/develop/company-box-xxx/images
;; and run mogrify -resize 50% *.png if images are too big

;; syntax highlighting
(use-package flycheck :ensure t)
(use-package flymake :ensure t)

;; default scrollbar
(scroll-bar-mode -1)

;; some company tweaks
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .1)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;; company mode autocompletion on second character
(setq company-minimum-prefix-length 1)
;; disable only lowercase in company autocompletion
;;(setq company-dabbrev-downcase nil)
;;(setq company-require-match nil)

;; disable company annoyance on enter / tab
(setq tab-always-indent t)

;; ansi-term stuff
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; disable whitespace mode by default
(global-whitespace-mode 0)
(whitespace-mode 0)
;; default size
(when window-system
;;  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 150 50))
;; copy path
(exec-path-from-shell-copy-env "PATH")
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

;; which-key show possible commands in minibuffer
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

;; osx fixes
(when (eq system-type 'darwin)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-key-is-meta t)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil))

;; disable guru mode by default
;; M-x customize-option prelude-guru -> disable it

;; only emacs 26
;;(when (version<= "26.0.50" emacs-version )
;;  (global-display-line-numbers-mode))

;; how to disable whitespace mode forever:
;; customize-group RET prelude
;; prelude-whitespace set to off (default is on)


;;;;;;;;;;;;;;;;;
;; magit ;;;;;;;;
;;;;;;;;;;;;;;;;;

(use-package magit :ensure t)
(global-set-key (kbd "C-c C-g") 'magit-status)
(add-hook 'magit-mode-hook (lambda () (local-set-key (kbd "C-o") #'magit-diff-visit-file-other-window)))
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)


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



(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

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
  (load-theme 'doom-solarized-dark t) ; change theme here (looks good: doom-material doom-one doom-dark+ doom-solarized-dark)
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
;;(setq doom-modeline-irc t)
(setq doom-modeline-irc nil)
;; Function to stylize the irc buffer names.
(setq doom-modeline-irc-stylize 'identity)

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


(use-package lsp-mode
  :ensure t
  :init (setq lsp-inhibit-message t
              lsp-eldoc-render-all t
              lsp-highlight-symbol-at-point nil
              lsp-keymap-prefix "s-l")
  :hook
  (enh-ruby-mode . lsp)
  (scala-mode . lsp)
  (java-mode . lsp)
  (python-mode . lsp))

(use-package company-lsp
  :after  company
  :ensure t
  :config
  (add-hook 'java-mode-hook (lambda () (push 'company-lsp company-backends)))
  (add-hook 'python-mode-hook (lambda () (push 'company-lsp company-backends)))
  (add-hook 'enh-ruby-mode-hook (lambda () (push 'company-lsp company-backends)))
  (add-hook 'scala-mode-hook (lambda () (push 'company-lsp company-backends)))
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-sideline-update-mode 'point))

;; keys for find references and definitions to 
(define-key lsp-ui-mode-map (kbd "C-RET") #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map (kbd "<C-return>") #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map (kbd "M-RET") #'lsp-ui-peek-find-references)
(define-key lsp-ui-mode-map (kbd "<M-return>") #'lsp-ui-peek-find-references)

(push 'company-lsp company-backends)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)


;;;;;;;;;;;;;;;;;
;;; ruby ;;;;;;;;
;;;;;;;;;;;;;;;;;

(use-package enh-ruby-mode :ensure t)
(use-package smartparens-ruby :ensure smartparens)
(use-package flymake-ruby :ensure t)
(use-package projectile-rails :ensure t)
(setq enh-ruby-add-encoding-comment-on-save nil
      enh-ruby-deep-indent-paren nil
      enh-ruby-deep-indent-construct nil
      enh-ruby-hanging-brace-indent-level 2)

(add-hook 'en-ruby-mode-hook 'flycheck-mode)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . enh-ruby-mode))
(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.zshrc && printf $PATH")))
(projectile-rails-global-mode)
(add-hook 'enh-ruby-mode-hook 'flymake-ruby-load)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
(global-set-key (kbd "C-c r r") 'inf-ruby)

(use-package ruby-test-mode :ensure t)
(add-hook 'enh-ruby-mode-hook 'ruby-test-mode)
;;(lambda (buf strg)
;;  (switch-to-buffer-other-window "*compilation*")
;;  (read-only-mode)
;;  (goto-char (point-max))
;;  (local-set-key (kbd "q")
;;                 (lambda () (interactive) (quit-restore-window)))))

;; rubocop
(use-package rubocop :ensure t)

;; ruby settings
(setq ruby-insert-encoding-magic-comment nil)
;; remove default faces for enh-ruby-mode
;; (remove-hook 'enh-ruby-mode-hook 'erm-define-faces)


;;;;;;;;;;;;;;;;;
;;; python ;;;;;;
;;;;;;;;;;;;;;;;;
;; everything already configured with lsp



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


;;;;;;;;;;;;;;;;;
;;; node.js;;;;;;
;;;;;;;;;;;;;;;;;

(use-package js3-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))
(add-to-list 'company-backends 'company-tern)
(add-hook 'js3-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))


;;;;;;;;;;;;;;;;;
;;; web ;;;;;;;;;
;;;;;;;;;;;;;;;;;

(use-package web-mode)
(defun my-web-mode-hook ()
 "Hook for `web-mode'."
  (set (make-local-variable 'company-backends)
       '(company-tern company-web-html company-yasnippet company-files)))
(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html))
                          (company-mode t)))

;; enable javascript completion between <script>...</script> etc.
(advice-add 'company-tern :before
            #'(lambda (&rest _)
                (if (equal major-mode 'web-mode)
                    (let ((web-mode-cur-language
                           (web-mode-language-at-pos)))
                      (if (or (string= web-mode-cur-language "javascript")
                              (string= web-mode-cur-language "jsx"))
                          (unless tern-mode (tern-mode))
                        (if tern-mode (tern-mode -1)))))))


;;;;;;;;;;;;;;;;;
;;; go ;;;;;;;;;;
;;;;;;;;;;;;;;;;;

(use-package go-mode)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))


;;;;;;;;;;;;;;;;;
;;; sql ;;;;;;;;;
;;;;;;;;;;;;;;;;;

;; some fixes
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
