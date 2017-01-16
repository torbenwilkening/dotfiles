
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
  '(go-mode go-autocomplete android-mode flycheck js2-mode json-mode web-mode auto-complete tern-auto-complete))

(defun own-packages-installed-p ()
  (loop for p in own-packages
        when (not (packages-installed-p p)) do (return nil)
        finally (return t)))

;; install if not installed
(dolist (p own-packages)
  (when (not (package-installed-p p))
    (package-install p)))




;set default modifier keys
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;; disable whitespace mode
(setq prelude-whitespace nil)

;; enable ac by default
(global-auto-complete-mode t)

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
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;(setenv "GOPATH" "/Users/torben/Projects/go/greenHyve")
;(add-to-list 'exec-path "/Users/torben/Projects/go/greenHyve/bin")
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


;;;;;;;;;;; ES6 stuff
; npm install -g eslint babel-eslint eslint-plugin-react tern
; use web-mode for .jsx files

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(setq web-mode-content-types-alist
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
(require 'tern-auto-complete)
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'web-mode-hook (lambda () (tern-mode t)))

;; install following emacs packages:
;; flycheck, js2-mode, json-mode, web-mode, (osx -> exec-path-from-shell)
