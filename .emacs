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

(defun ar/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (let* ((buffer-read-only)
           (image-path "~/.logo.png")
           (image (create-image image-path))
           (size (image-size image))
           (height (cdr size))
           (width (car size))
           (top-margin (floor (/ (- (window-height) height) 2)))
           (left-margin (floor (/ (- (window-width) width) 2)))
           (prompt-title "Welcome to Emacs!"))
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n ))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      (insert (make-string (floor (/ (- (window-width) (string-width prompt-title)) 2)) ?\ ))
      (insert prompt-title))
    (setq cursor-type nil)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "q") 'kill-this-buffer)))

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

(when (< (length command-line-args) 2)
  (add-hook 'emacs-startup-hook (lambda ()
                                  (when (display-graphic-p)
                                    (ar/show-welcome-buffer)))))



;;;;;;;;;;;;;
;; general ;;
;;;;;;;;;;;;;


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

;; env variables
(straight-use-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "PATH")


;;;;;;;;;;;;;
;; theming ;;
;;;;;;;;;;;;;


(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-battery-mode 1)
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; a little bit window transparency is an option
;; (set-frame-parameter (selected-frame) 'alpha '(99 99))

(straight-use-package 'hide-mode-line)
(add-hook 'eshell-mode-hook 'hide-mode-line-mode)

(straight-use-package 'all-the-icons)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(straight-use-package 'doom-themes)

;; treemacs and config
(straight-use-package 'treemacs)
(global-set-key (kbd "s-t") 'treemacs)
(progn
  (setq treemacs-space-between-root-nodes nil)
  (treemacs-resize-icons 16))
(straight-use-package 'treemacs-projectile)
(straight-use-package 'treemacs-magit)
;;(straight-use-package 'treemacs-all-the-icons)
(straight-use-package 'treemacs-icons-dired)

(straight-use-package 'doom-modeline)
;;(load-theme 'doom-gruvbox t)
;;(load-theme 'doom-monokai-pro t)
(load-theme 'doom-nord t)
(setq doom-themes-treemacs-theme "doom-colors"
      doom-modeline-vcs-max-length 20
      doom-modeline-icon (display-graphic-p)
      doom-modeline-major-mode-color-icon t
      doom-modeline-persp-name nil
      doom-modeline-display-default-persp-name nil
      doom-modeline-buffer-encoding nil)


;;;;;;;;;;;;;;;;;;
;; lsp features ;;
;;;;;;;;;;;;;;;;;;


;; eglot
(straight-use-package 'eglot)
(require 'eglot)

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

;; projectile, ivy, counsel, swiper, ag
(straight-use-package 'projectile)
(setq projectile-project-search-path '("~/Projects/"))
(setq projectile-auto-discover nil)

;; @todo find file explorer for projectile
(straight-use-package 'project-explorer)

(straight-use-package 'ivy)
(ivy-mode 1)

(straight-use-package 'counsel)
(straight-use-package 'counsel-projectile)
(counsel-projectile-mode)

(straight-use-package 'swiper)

;; ag
(straight-use-package 'ag)
(setq ag-highlight-search t
      ag-reuse-buffers t)

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

;; line numbers
(display-line-numbers-mode)

;; git changes in line numbers
(straight-use-package 'git-gutter-fringe)
(global-git-gutter-mode)

;; string inflection
(straight-use-package 'string-inflection)
; @todo make hydra


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
(add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))
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
(add-to-list 'eglot-server-programs '(vetur-vue-mode "vls"))
(flycheck-add-mode 'javascript-eslint 'vetur-vue-mode)

;; vue3 with volar (currently disabled until stable)
(define-derived-mode volar-vue-mode web-mode "Vue3"
  "A major mode derived from vue-mode with volar language server")
(add-hook 'volar-vue-mode-hook #'eglot-ensure)
(add-hook 'volar-vue-mode-hook 'vue-modes-indent-hook)
(add-hook 'volar-vue-mode-hook 'flycheck-mode)
(add-to-list 'eglot-server-programs '(volar-vue-mode "vue-language-server", "--stdio"))
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
;;              '(vue-mode . (eglot-volar "vue-language-server" "--stdio")))
;; (defclass eglot-volar (eglot-lsp-server) ()
;;   :documentation "A custom class for volar")
;; (cl-defmethod eglot-initialization-options ((server eglot-volar))
;;   "Passes through required volar initialization options"
;;   (let*
;;       ((serverPath
;;         (expand-file-name
;;          "lib"
;; 	 "/Users/torbenwilkening/.asdf/installs/nodejs/16.19.0/lib/node_modules/typescript"
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
;;
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
(global-set-key (kbd "C-b") 'ibuffer)
(global-set-key (kbd "M-b") 'switch-to-buffer)
;; default m+k for kill current buffer
(global-set-key (kbd "s-p") 'previous-buffer)
(global-set-key (kbd "s-n") 'next-buffer)
(global-set-key (kbd "s-k") 'kill-current-buffer)
(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "s-s") 'projectile-switch-project)
(global-set-key (kbd "s-e") 'project-explorer-open)
;; replaces i-search with swiper
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-S-s") 'ag-project)
;; default find file
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; gives M-x command counsel features
(global-set-key (kbd "M-x") 'counsel-M-x)

;; editor movement
;; use M+L for go to line

(global-set-key (kbd "M-l") 'goto-line)
;; use M+<arrow> for multiple lines
(global-set-key (kbd "M-<down>")
                (lambda () (interactive) (forward-line  8)))
(global-set-key (kbd "M-<up>")
                (lambda () (interactive) (forward-line -8)))

;; code modification
(global-set-key (kbd "C-u") 'undo)
;;(global-set-key (kbd "C-m") 'mc/edit-lines)

;; eglot
(global-set-key (kbd "<C-return>") 'xref-find-definitions)
(global-set-key (kbd "<s-return>") 'xref-find-references)
(global-set-key (kbd "<M-return>") 'eglot-code-actions)

;; searching

;; compiler

;; multi cursor
;;(global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;

;; help
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
