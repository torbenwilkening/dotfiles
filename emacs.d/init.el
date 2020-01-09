;;; init.el ends here


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))


;; default stuff

(eval-when-compile (require 'use-package))
(use-package company :ensure t)
(scroll-bar-mode -1)
;; some company tweaks
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .1)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;; company mode autocompletion on first character
(setq company-minimum-prefix-length 0)
;; disable only lowercase in company autocompletion
(setq company-dabbrev-downcase nil)
;; disable whitespace mode by default
(global-whitespace-mode 0)
(whitespace-mode 0)
;; default size
(when window-system
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 140 60))
;; for osx keyboards
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(global-set-key (kbd "s-l") '(lambda () (interactive) (insert "@")))




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
