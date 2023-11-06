
(set-frame-font "Iosevka Fixed SS14-28")

(setq cider-repl-display-help-banner nil
      cider-repl-pop-to-buffer-on-connect nil
      cider-allow-jack-in-without-project t
      cljr-suppress-no-project-warning t
      split-width-threshold 100
      company-minimum-prefix-length 1
      cider-use-xref t
      confirm-kill-processes nil
      ;; lsp-lens-enable nil
      ;; lsp-signature-auto-activate nil
      ;; lsp-diagnostics-provider :none
      ;; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ;; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )

(use-package adoc-mode)
(use-package browse-url)
(use-package edit-indirect)
(use-package just-mode)
(use-package web-mode)
(use-package dotenv-mode)
(use-package grip-mode)
(use-package go-mode)
(use-package restclient)
(use-package html-to-hiccup)
(use-package clj-refactor)
(use-package piglet-emacs)
(use-package adoc-mode)



;; (use-package lsp-mode
;;   :config
;;   (when (not (file-exists-p (expand-file-name ".cache/lsp/clojure/clojure-lsp" user-emacs-directory)))
;;     (lsp-install-server nil 'clojure-lsp))
;;   ;; (lsp-install-server t 'clojure-lsp)
;;   (add-hook 'clojure-mode-hook 'lsp)
;;   (add-hook 'clojurescript-mode-hook 'lsp)
;;   (add-hook 'clojurec-mode-hook 'lsp))

;; Compute
(setq sql-postgres-login-params nil
      sql-user ""
      sql-postgres-program "/home/arne/Compute/monorepo/projects/csp-billing-dev/bin/cs_psql"
      sql-database "")
(defalias 'sql-get-login 'ignore)

(setq desktop-restore-frames nil)
(desktop-save-mode 1)

(provide 'nextjournal)

(load-theme 'sanityinc-tomorrow-night t)

(setq grip-github-password "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")


;; make sure you've set your default project with:
;; gcloud config set project <project-name>

(require 'tramp)
(add-to-list
 'tramp-methods
 '("gcssh"
   (tramp-login-program        "gcloud compute ssh --zone europe-west1-b")
   (tramp-login-args           (("%h")))
   (tramp-async-args           (("-q")))
   (tramp-remote-shell         "/bin/sh")
   (tramp-remote-shell-args    ("-c"))
   (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                ("-o" "UserKnownHostsFile=/dev/null")
                                ("-o" "StrictHostKeyChecking=no")))
   (tramp-default-port         22)))

;; ... after which it's as easy as:
;;
;; C-x C-f /gcssh:compute-instance:/path/to/filename.clj
;; tramp-methods
;; (setq-default display-line-numbers 'relative)

;; (load-file "etrace.el")
;; (load-file "tailwind_cheatsheet.el")
;; (load-file "memoize.el")
;; (memoize 'cider-resolve-var)

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)
(setq org-confirm-babel-evaluate
      (lambda (lang body)
        (when (equal 'clojure lang)
          :allow)))
