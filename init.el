(load-file (expand-file-name "bootstrap.el" user-emacs-directory))

(setq warning-minimum-level :error)
(setq warning-minimum-log-level :error)

(let ((straight-current-profile 'corgi))
  (use-package corgi-defaults)
  (use-package corgi-editor)

  (use-package corgi-commands)
  (use-package corgi-clojure)
  (use-package corgi-emacs-lisp)
  (use-package corgi-stateline)
  (use-package corgi-bindings)

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
;;(use-package clj-refactor)

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
(set-register ?p "#_clj (user/portal)")
(set-register ?P "#_cljs (user/portal)")


(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

;;(require 'corgi-clojure-cider-extras)
;;(require 'corgi-cider-connection-indicator)

(setq cider-connection-message-fn
      nil )

(setq recentf-max-saved-items 100)

(when (executable-find "bb")
  (corgi/cider-jack-in-babashka))
(run-at-time nil (* 5 60) 'recentf-save-list)
(corgi/enable-cider-connection-indicator)

cider-connected-hook

(use-package visual-fill-column)

(with-current-buffer (get-buffer-create "*scratch-clj*")
  (clojure-mode))

(with-current-buffer (get-buffer-create "*scratch*")
  (lisp-interaction-mode))

(let ((local-config (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load-file local-config)))

(use-package html-to-hiccup)

(setq warning-minimum-level :warning)
(setq warning-minimum-log-level :warning)

(put-clojure-indent 'reflect/extend-signatures '(1 :form (1)))
(put-clojure-indent 'sc.api/letsc '(1))

;; (eval-after-load 'projectile
;;   (setq projectile-project-root-files-bottom-up
;;         (cons "deps.edn"
;;               projectile-project-root-files-bottom-up)))
