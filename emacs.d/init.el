

;; melpa stuff
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)


;;; Uncomment the modules you'd like to use and restart Prelude afterwards
(require 'prelude-c)
(require 'prelude-clojure)
(require 'prelude-coffee)
(require 'prelude-common-lisp)
;(require 'prelude-css)
(require 'prelude-emacs-lisp)
(require 'prelude-erc)
(require 'prelude-erlang)
(require 'prelude-elixir)
(require 'prelude-haskell)
;(require 'prelude-js)
(require 'prelude-latex)
(require 'prelude-lisp)
(require 'prelude-mediawiki)
(require 'prelude-org)
(require 'prelude-perl)
(require 'prelude-python)
(require 'prelude-ruby)
(require 'prelude-scala)
(require 'prelude-scheme)
;(require 'prelude-scss)
;(require 'prelude-web)
(require 'prelude-xml)


;; check if packages are installed and if not install
;;  company-web-html typescript-mode
;;(defvar own-packages
;;  '(company company-tern go-company go-mode tide jedi-core jedi company-jedi android-mode flycheck js2-mode json-mode web-mode websocket jss ))

;;(defun own-packages-installed-p ()
;;  (loop for p in own-packages
;;        when (not (packages-installed-p p)) do (return nil)
;;        finally (return t)))

;; install if not installed
;;(dolist (p own-packages)
;;  (when (not (package-installed-p p))
;;    (package-install p)))


;; grails stuff
(require 'grails)

;; python stuff
(require 'company-jedi)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; nodejs stuff
;; for debugging check https://indium.readthedocs.io/en/latest/setup.html
(require 'company)
(require 'company-web-html)
(require 'company-tern)
(require 'company-go)

;; jsmode stuff
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))


;; webmode stuff
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
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
;; some go company tweaks
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

;; company mode autocompletion on first character
(setq company-minimum-prefix-length 1)
;; disable only lowercase in company autocompletion
(setq company-dabbrev-downcase nil)
;; default font size
(set-face-attribute 'default nil :height 100)


;; typescript stuff
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
;(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "ts" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
;(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
;; M-: key for GoToDefinition
;; M-; key for GoBack
;; M-- key for FindUsages

(add-hook 'tide-mode-hook
          (lambda () (local-set-key (kbd "M-m") 'tide-references)))




;; try this for react:
(add-to-list 'magic-mode-alist
             '("\\(import.*from \'react\';\\|\/\/ @flow\nimport.*from \'react\';\\)" . rjsx-mode))

;; indium js development
;; https://indium.readthedocs.io/en/latest/installation.html
(unless (package-installed-p 'indium)
  (package-install 'indium))
;; and setup your project : https://indium.readthedocs.io/en/latest/setup.html
(require 'indium)
(add-hook 'js2-mode-hook #'indium-interaction-mode)
; some .tern-project file example for ~/.tern-project
;{
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
