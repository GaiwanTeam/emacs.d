(load-file (expand-file-name "bootstrap.el" user-emacs-directory))

(let ((straight-current-profile 'corgi))
  (use-package corgi-defaults)
  (use-package corgi-editor)

  (use-package corgi-commands)
  (use-package corgi-clojure
    :config
    (when (executable-find "bb")
      (corgi/cider-jack-in-babashka))
    (corgi/enable-cider-connection-indicator)
    ;;(load-file "/home/arne/github/clojure-mode/clojure-mode.el")
    )
  (use-package corgi-emacs-lisp)
  (use-package corgi-stateline)

  (use-package corkey
    :config
    (corkey-mode 1)
    (corkey/load-and-watch)))

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
(use-package rainbow-mode)
(use-package pkg-info)

(server-start)
(global-display-line-numbers-mode 1)

;; use with ,,<letter>, e.g. `,,g' runs (user/go)
(set-register ?k "#_clj (do (require 'kaocha.repl) (kaocha.repl/run))")
(set-register ?K "#_clj (do (require 'kaocha.repl) (kaocha.repl/run-all))")
(set-register ?r "#_clj (do (require 'user :reload) (user/reset))")
(set-register ?g "#_clj (user/go)")
(set-register ?b "#_clj (user/browse)")
(set-register ?c "#_clj ((requiring-resolve 'nextjournal.clerk/serve!) {})")
(set-register ?, "#_clj (nextjournal.clerk/show! \"{{buffer-file-name}}\")")


(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-bright t))

(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

(let ((local-config (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load-file local-config)))

(with-current-buffer (get-buffer-create "*scratch-clj*")
  (clojure-mode))

(with-current-buffer (get-buffer-create "*scratch*")
  (lisp-interaction-mode))

(use-package html-to-hiccup)
