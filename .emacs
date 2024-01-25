(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(straight-use-package 'use-package)


;;;;;;;;;;;;;
;; welcome ;;
;;;;;;;;;;;;;


(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
;;(setq initial-major-mode 'markdown-mode)
(add-hook 'after-init-hook 'my-startup-scratch-hook)
(defun my-startup-scratch-hook ()
  (with-current-buffer "*scratch*"
    (hide-mode-line-mode)))


;;;;;;;;;;;;;
;; general ;;
;;;;;;;;;;;;;


;; default window size
(when window-system
;;  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 120 45))

;; backup and autosave
(setq make-backup-files nil)
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs-saves/" t)))

;; default window sizesf
(setq compilation-window-height 12)

;; yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; osx fixes
(when (eq system-type 'darwin)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-key-is-meta t)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil))

;; some fixes
(add-to-list 'image-types 'svg)

;; env variables (before emacs 29)
;;(straight-use-package 'exec-path-from-shell)
;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))
;;(exec-path-from-shell-copy-env "PATH")

;; setup world clocks
(setq world-clock-list
      '(
        ("EST" "Toronto")
        ;;      ("PST" "PST")
        ;;      ("UTC" "UTC")
        ("Europe/Berlin" "Hamburg")))

(setq world-clock-time-format "%a, %d. %b %R %Z")


;; spaces instead of tabs by default
(setq-default indent-tabs-mode nil)

;; vterm (install libvterm on your machine)
(straight-use-package 'vterm)

;;;;;;;;;;;;;
;; theming ;;
;;;;;;;;;;;;;

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))
(display-battery-mode 1)
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; a little bit window transparency is an option
;; (set-frame-parameter (selected-frame) 'alpha '(99 99))

;; hide modeline in certain major modes
(straight-use-package 'hide-mode-line)
(add-hook 'eshell-mode-hook 'hide-mode-line-mode)

;; all the icons
;(straight-use-package 'all-the-icons-dired)
;(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;(setq all-the-icons-dired-monochrome t)

;; general theme
(straight-use-package 'doom-themes)
(load-theme 'doom-monokai-pro t)
(with-eval-after-load 'doom-themes
  (doom-themes-treemacs-config))

;; great themes:
;;
;;(load-theme 'doom-gruvbox t)
;;(load-theme 'doom-nord t)
;;(load-theme 'doom-opera t)



;; treemacs and config
(straight-use-package 'treemacs)
(progn
  (setq treemacs-space-between-root-nodes nil
        ))
  ;;(treemacs-resize-icons 16))
(straight-use-package 'treemacs-magit)


;; nerd icons

;(straight-use-package 'all-the-icons)
;(straight-use-package 'all-the-icons-dired)
;(straight-use-package 'treemacs-all-the-icons)
;(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;(setq all-the-icons-dired-monochrome t)

(doom-themes-treemacs-config)

(straight-use-package 'all-the-icons-nerd-fonts)

(straight-use-package 'nerd-icons)
(require 'nerd-icons)

(straight-use-package 'treemacs-nerd-icons)
(require 'treemacs-nerd-icons)
(treemacs-load-theme "nerd-icons")

(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(setq nerd-icons-font-family "Ubuntu Nerd Font")

;; if its lagging during render try this
;;(setq inhibit-compacting-font-caches t)

(setq nerd-icons-color-icons t)

;; ibuffer
(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
(setq nerd-icons-ibuffer-icon t)

;; modeline
(straight-use-package 'doom-modeline)
(setq ;;doom-themes-treemacs-theme "nerd-icons"
      doom-modeline-vcs-max-length 20
      doom-modeline-icon (display-graphic-p)
      doom-modeline-major-mode-color-icon t
      doom-modeline-persp-name nil
      doom-modeline-display-default-persp-name nil
      doom-modeline-buffer-encoding nil)


;; notifications
(use-package alert
  :straight t
  :config
  (if (eq system-type 'darwin)
      (setq
       ;; alert-default-style 'notifier
       alert-default-style 'osx-notifier
       )))


;; chatgpt
(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el")))

;; $ cat ~/.authinfo
;; machine api.openai.com password <APIKEY>
(setq chatgpt-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com"))


;; if you are using the "pass" password manager
;;(setq chatgpt-shell-openai-key
;;      (lambda ()
;;        ;; (auth-source-pass-get 'secret "openai-key") ; alternative using pass support in auth-sources
;;        ;; (nth 0 (process-lines "pass" "show" "openai-key"))))

;; some functions

(require 'url)
(require 'json)

(defun insert-json-at-point ()
  "Get data from URL and insert the response."
  (url-retrieve "https://api.example.com/data.json"
                (lambda (status)
                  (goto-char (point-min))
                  (re-search-forward "^$")
                  (let ((data (json-read)))
                    (kill-buffer)
                    (insert (format "JSON data: %S" data))))))

(defun insert-random-text-at-point ()
  "Insert the response of the URL at the current cursor position."
  (interactive)
  (url-insert-file-contents "http://metaphorpsum.com/paragraphs/1"))


    

;;;;;;;;;;;;;;;;;;
;; lsp features ;;
;;;;;;;;;;;;;;;;;;



;; company
(straight-use-package 'company-mode)
(straight-use-package 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)
(setq company-tooltip-limit 5
      company-idle-delay .1
      company-echo-delay 0
      company-minimum-prefix-length 1 ; after first character
      company-begin-commands '(self-insert-command) ; only after typing
      completion-ignore-case nil)


;;;;;;;;;;;;;;;;;;
;; global modes ;;
;;;;;;;;;;;;;;;;;;


(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'doom-modeline-mode)

;; no need for whitespace mode by default
(global-whitespace-mode 0)
(whitespace-mode 0)

;; delete marked region when typing
(delete-selection-mode 1)



(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("C-l"   . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-S-s" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"   . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  ;;(advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-file-register
  ;;  consult--source-recent-file consult--source-project-recent-file
  ;;  ;; :preview-key "M-."
  ;;  :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)


(use-package vertico
  :straight t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; (straight-use-package 'mini-frame)
;; (custom-set-variables
;;  '(mini-frame-show-parameters
;;    '((top . 80)
;;      (width . 0.7)
;;      (left . 0.5))))
;; (mini-frame-mode 1)

(use-package marginalia
  :straight t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))



;; flycheck
(straight-use-package 'flycheck)
(global-flycheck-mode)
(flymake-mode -1)

;; yasnippet
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(yas-global-mode 1)

;; enable smartparens for certain languages
(straight-use-package 'smartparens)
(require 'smartparens-config)
(add-hook 'typescript-mode-hook #'smartparens-mode)
(add-hook 'volar-vue-mode-hook #'smartparens-mode)
(add-hook 'vetur-vue-mode-hook #'smartparens-mode)
(add-hook 'web-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'go-mode-hook #'smartparens-mode)
(add-hook 'json-mode-hook #'smartparens-mode)
(add-hook 'graphql-mode-hook #'smartparens-mode)

(straight-use-package 'multiple-cursors)
; @todo keybindings

;; if you want line numbers
;; (display-line-numbers-mode)

;; git changes in line numbers
(straight-use-package 'git-gutter-fringe)
(global-git-gutter-mode)

;; string inflection
(straight-use-package 'string-inflection)
; @todo make hydra


;; eglot
(straight-use-package 'project)
(require 'project)
(global-set-key (kbd "s-f") 'project-find-file)
(global-set-key (kbd "s-s") 'project-switch-project)


(straight-use-package 'eglot)
(require 'eglot)


;;;;;;;;;;;;;;;;;;
;; magit and vc ;;
;;;;;;;;;;;;;;;;;;


(straight-use-package 'magit)
(global-set-key (kbd "C-c C-g") 'magit-status)
;; @todo enable forge for gitlab
;; https://magit.vc/manual/forge/Getting-Started.html
;; (straight-use-package 'forge)
;; (with-eval-after-load 'magit
;;   (require 'forge))
;; (straight-use-package 'magit-todos)

(straight-use-package 'git-timemachine)


;;;;;;;;;;;;;;;;;
;; major modes ;;
;;;;;;;;;;;;;;;;;

;; tree-sitter until emacs 29
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)


;; yaml
(straight-use-package 'yaml-mode) ; yaml-language-server
(add-hook 'yaml-mode-hook #'eglot-ensure)

;; docker
(straight-use-package 'dockerfile-mode) ; dockerfile-language-server-nodejs
(add-hook 'dockerfile-mode-hook #'eglot-ensure)

;;json
(straight-use-package 'json-mode)

;; graphql
(straight-use-package 'graphql-mode)
(add-hook 'graphql-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs '(graphql-mode . ("graphql-lsp" "server" "-m" "stream")))

;; kubernetes
(straight-use-package 'kubernetes)
(setq kubernetes-poll-frequency 3600
      kubernetes-redraw-frequency 3600)

;; ruby
;; maybe: smartparens-ruby rubocop bundler rbenv ruby-test-mode haml-mode
(straight-use-package 'enh-ruby-mode)
(add-hook 'enh-ruby-mode-hook #'eglot-ensure)
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; scala
(straight-use-package 'scala-mode)
(add-hook 'scala-mode-hook #'eglot-ensure)
(straight-use-package 'sbt-mode)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

;; go
(straight-use-package 'go-mode)
(add-hook 'go-mode-hook #'eglot-ensure)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; elixir
(straight-use-package 'elixir-mode)
(add-hook 'elixir-mode-hook #'eglot-ensure)
(straight-use-package 'alchemist)
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))

;; python
(straight-use-package 'python-mode)
(add-hook 'python-mode-hook #'eglot-ensure) ;pip install python-language-server
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; markdown
(straight-use-package 'markdown-mode) ; brew install marksman
(add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
(add-hook 'markdown-mode-hook #'eglot-ensure)

;; mermaid
(straight-use-package 'mermaid-mode)

;; rust
(straight-use-package 'rust-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)


;; web development
;; @todo make eslint relative to the project, not global
;; @todo disable flymake in favor of flycheck

;; html / css
(straight-use-package 'web-mode)
(add-hook 'web-mode-hook #'eglot-ensure)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

;; typescript / javascript
(straight-use-package 'typescript-mode)
(add-hook 'typescript-mode-hook 'js-modes-indent-hook)
(add-hook 'typescript-mode-hook #'eglot-ensure)
(add-hook 'typescript-mode-hook 'flycheck-mode)
(add-to-list 'eglot-server-programs '(typescript-mode . ("bunx" "typescript-language-server" "--stdio")))
(flycheck-add-mode 'javascript-eslint 'typescript-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(defun js-modes-indent-hook ()
  "Indent settings for js-mode, js2-mode and typescript mode"
  (setq js-indent-level 2)
  (setq typescript-indent-level 2)
  (setq js-jsx-indent-level 2))

;; vue2 with vetur
(define-derived-mode vetur-vue-mode web-mode "Vue2"
  "A major mode derived from vue-mode with vetur language server")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vetur-vue-mode))
(add-hook 'vetur-vue-mode-hook #'eglot-ensure)
(add-hook 'vetur-vue-mode-hook 'vue-modes-indent-hook)
(add-hook 'vetur-vue-mode-hook 'flycheck-mode)
(add-to-list 'eglot-server-programs '(vetur-vue-mode "bunx" "vls"))
(flycheck-add-mode 'javascript-eslint 'vetur-vue-mode)

;; vue3 with volar (currently disabled until stable)
(define-derived-mode volar-vue-mode web-mode "Vue3"
  "A major mode derived from vue-mode with volar language server")
(add-hook 'volar-vue-mode-hook #'eglot-ensure)
(add-hook 'volar-vue-mode-hook 'vue-modes-indent-hook)
(add-hook 'volar-vue-mode-hook 'flycheck-mode)
(add-to-list 'eglot-server-programs '(volar-vue-mode "bunx" "vue-language-server" "--stdio"))
(flycheck-add-mode 'javascript-eslint 'volar-vue-mode)


(defun vue-modes-indent-hook ()
  "Indent settings for web-mode"
  (setq web-mode-script-padding 0)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-style-padding 0)
  (setq web-mode-code-indent-offset 2))


;; @todo vue3 with volar
;; (add-to-list 'eglot-server-programs
;;              '(volar-vue-mode . (eglot-volar "vue-language-server" "--stdio")))
;; (defclass eglot-volar (eglot-lsp-server) ()
;;   :documentation "A custom class for volar")
;; (cl-defmethod eglot-initialization-options ((server eglot-volar))
;;   "Passes through required volar initialization options"
;;   (let*
;;       ((serverPath
;;         (expand-file-name
;;          "lib"
;; 	 "/Users/torbenwilkening/.asdf/installs/nodejs/20.8.0/lib/node_modules/typescript"
;; 	 )))
;;     (list :typescript
;;           (list :tsdk serverPath)
;;           :languageFeatures
;;           (list :completion
;;                 (list :defaultTagNameCase "both"
;;                       :defaultAttrNameCase "kebabCase"
;;                       :getDocumentNameCasesRequest nil
;;                       :getDocumentSelectionRequest nil)
;;                 :diagnostics
;;                 (list :getDocumentVersionRequest nil))
;;           :documentFeatures
;;           (list :documentFormatting
;;                 (list :defaultPrintWidth 100
;;                       :getDocumentPrintWidthRequest nil)
;;                 :documentSymbol t
;;                 :documentColor t))))

;; @todo fix this paths
;;"/Users/torbenwilkening/.asdf/installs/nodejs/16.19.0/lib"
;; /Users/torbenwilkening/.asdf/installs/nodejs/16.19.0/lib/node_modules/typescript
;;(string-trim-right (shell-command-to-string "npm list --global --parseable typescript | head -n1"))
;; non-osx: (shell-command-to-string "npm list --global --parseable typescript | head -n1 | tr -d \"\n\""))))




;;;;;;;;;;;;;;;;;;;
;; key shortcuts ;;
;;;;;;;;;;;;;;;;;;;


;; window movement
;; use s+<arrow>
(define-key global-map (kbd "s-<up>") 'windmove-up)
(define-key global-map (kbd "s-<down>") 'windmove-down)
(define-key global-map (kbd "s-<left>") 'windmove-left)
(define-key global-map (kbd "s-<right>") 'windmove-right)
;; window splitting
(global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "C-2") 'split-window-vertically)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-0") 'delete-window)

;; buffer movement
;; ibuffer usually with b
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;(global-set-key (kbd "M-b") 'ibuffer)

;;(global-set-key (kbd "C-b") 'counsel-switch-buffer)  ;'switch-to-buffer)
;; default m+k for kill current buffer

(global-set-key (kbd "s-p") 'previous-buffer)
(global-set-key (kbd "s-n") 'next-buffer)
(global-set-key (kbd "s-k") 'kill-current-buffer)
(global-set-key (kbd "s-e") 'treemacs)
;;(global-set-key (kbd "C-S-s") 'ag-project)

;; replaces i-search with swiper
;;(global-set-key (kbd "C-s") 'swiper)


;; default find file
;;(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; gives M-x command counsel features
;;(global-set-key (kbd "M-x") 'counsel-M-x)

;; editor movement
;; use M+L for go to line

;;(global-set-key (kbd "M-l") 'goto-line)
;; use M+<arrow> for multiple lines
(global-set-key (kbd "M-<down>")
                (lambda () (interactive) (forward-line  8)))
(global-set-key (kbd "M-<up>")
                (lambda () (interactive) (forward-line -8)))

;; code modification
(global-set-key (kbd "C-u") 'undo)
;;(global-set-key (kbd "C-m") 'mc/edit-lines)
(global-set-key (kbd "C-f") 'consult-line)

;; eglot
(global-set-key (kbd "<C-return>") 'xref-find-definitions)
(global-set-key (kbd "<M-return>") 'xref-find-references)
(global-set-key (kbd "<s-return>") 'eglot-code-actions)

;; searching

;; compiler

;; multi cursor
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;

;; help
;;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
;;(global-set-key (kbd "<f1> l") 'counsel-find-library)


