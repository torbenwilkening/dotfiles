

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
;;  '(company company-tern go-company go-mode jedi-core jedi company-jedi android-mode flycheck js2-mode json-mode web-mode websocket jss ))

;;(defun own-packages-installed-p ()
;;  (loop for p in own-packages
;;        when (not (packages-installed-p p)) do (return nil)
;;        finally (return t)))

;; install if not installed
;;(dolist (p own-packages)
;;  (when (not (package-installed-p p))
;;    (package-install p)))




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
