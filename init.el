(load-file (expand-file-name "bootstrap.el" user-emacs-directory))

(let ((straight-current-profile 'corgi))
  (use-package corgi-defaults)
  (use-package corgi-editor)
  (use-package corkey
    :config
    (corkey-mode 1)
    (corkey/install-bindings '(corgi-keys keys) '(corgi-signals signals)))
  (use-package corgi-commands)
  (use-package corgi-clojure)
  (use-package corgi-emacs-lisp)
  (use-package corgi-stateline)
  )

(use-package magit)
(use-package org
  :config
  (require 'org-tempo))
(use-package markdown-mode)
(use-package yaml-mode)
(use-package inf-clojure)
(use-package hcl-mode)
(use-package typescript-mode)
(use-package dockerfile-mode)
(use-package groovy-mode)
(use-package buttercup)

;; (straight-freeze-versions)
(server-start)

;; Support Home/End in terminals
(global-set-key (kbd "M-[ 1 ~") 'beginning-of-line)
(global-set-key (kbd "M-[ 4 ~") 'end-of-line)
(global-set-key (kbd "<select>") 'end-of-line)

;; use with ,,<letter>, e.g. `,,g' runs (user/go)
(set-register ?k "#_clj (do (require 'kaocha.repl) (kaocha.repl/run))")
(set-register ?K "#_clj (do (require 'kaocha.repl) (kaocha.repl/run-all))")
(set-register ?r "#_clj (do (require 'user :reload) (user/reset))")
(set-register ?g "#_clj (user/go)")

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-bright t))

(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

