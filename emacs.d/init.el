

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
;;(use-package company-box
;;  :init (setq company-box-enable-icon (display-graphic-p))
;;  :hook (company-mode . company-box-mode))
;; go to ~/.emacs.d/elpa/26.1/develop/company-box-xxx/images
;; and run mogrify -resize 50% *.png if images are too big

;; default scrollbar
(scroll-bar-mode -1)

;; some company tweaks
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .1)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;; company mode autocompletion on second character
(setq company-minimum-prefix-length 2)
;; disable only lowercase in company autocompletion
(setq company-dabbrev-downcase nil)
(setq company-require-match nil)

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

;; magit stuff
(use-package magit :ensure t)
(global-set-key (kbd "C-c C-g") 'magit-status)

;; osx fixes
(when (eq system-type 'darwin)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-key-is-meta t)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil))

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

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; ruby
(use-package enh-ruby-mode :ensure t)
(use-package flymake-ruby :ensure t)
(use-package projectile-rails :ensure t)
(use-package robe :ensure t)

(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . enh-ruby-mode))

;; remove previous faces for enh-ruby-mode
(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.zshrc && printf $PATH")))
(projectile-rails-global-mode)

(add-hook 'enh-ruby-mode-hook 'flymake-ruby-load)
(global-set-key (kbd "C-c r r") 'inf-ruby)

(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

(use-package ruby-test-mode :ensure t)
(add-hook 'enh-ruby-mode-hook 'ruby-test-mode)
;;(lambda (buf strg)
;;  (switch-to-buffer-other-window "*compilation*")
;;  (read-only-mode)
;;  (goto-char (point-max))
;;  (local-set-key (kbd "q")
;;                 (lambda () (interactive) (quit-restore-window)))))

;; robe for company and jump to
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; rubocop
(use-package rubocop :ensure t)

;; ruby settings
(setq ruby-insert-encoding-magic-comment nil)
;; remove default faces for enh-ruby-mode
(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

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



