;; Allow Ctrl-u to scroll up a page like vim
(setq evil-want-C-u-scroll t)

(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)
;; Option or Alt is naturally 'Meta'
(setq mac-option-modifier 'meta)
;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
(setq mac-right-option-modifier 'nil)

(defun ox/open-init-el ()
  (interactive)
  (find-file (expand-file-name "local.ox.el" user-emacs-directory)))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; You will most likely need to adjust this font size for your system!
(defvar ox/default-font-size 180)
(defvar ox/default-variable-font-size 180)
(set-face-attribute 'default nil :font "Iosevka" :height ox/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height ox/default-font-size)

;; Unfortunately emacs launched from `.app` launcher does not get the full exec path which our shell has. Let's fix that
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (fset 'evil-visual-update-x-selection 'ignore)
  (setq evil-kill-on-visual-paste nil)
  (setq-default evil-symbol-word-search t)
  :config
  (evil-mode t)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (require 'evil-maps)
  (define-key evil-motion-state-map "L" nil)
  (define-key evil-motion-state-map "M" nil)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Use another key to go into normal / escape mode. I have it configured as `qp`
(use-package evil-escape
  :config
  (setq-default evil-escape-key-sequence "qp")
  (evil-escape-mode))

(use-package evil-cleverparens
  :after (evil smartparens)
  :commands evil-cleverparens-mode
  :init
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (setq evil-cleverparens-complete-parens-in-yanked-region t)
  :config
  (setq evil-cleverparens-use-s-and-S nil)
  (evil-define-key '(normal visual) evil-cleverparens-mode-map
    "s" nil
    "S" nil
    "{" nil
    "}" nil
    "[" nil
    "]" nil
    (kbd "M-[") nil
    (kbd "<tab>") 'evil-jump-item))

(use-package projectile
  :init
  (setq projectile-create-missing-test-files t)
  (setq projectile-project-search-path '("~/projects/" "~/playground/" "~/projects/lambdaisland"))
  (defun ox/refresh-projects-dir ()
    (interactive)
    ;; (projectile-discover-projects-in-directory "~/projects")
    (projectile-discover-projects-in-search-path))
  :config
  (projectile-global-mode))

;; command-log-mode is useful for displaying a panel showing each key binding
;; you use in a panel on the right side of the frame. Great for live streams and
;; screencasts!
(use-package command-log-mode)

(use-package forge
  :after magit
  :config
  (transient-append-suffix 'forge-dispatch '(0)
    ["Forge browse"
     ("@" "browse" forge-browse)])
  (transient-append-suffix 'forge-dispatch '(0)
    ["PR"
     ("p c" "pullreq checkout" forge-checkout-pullreq)]
    )
  (transient-append-suffix 'forge-dispatch '(0)
    ["Edit"
     ("e p" "post" forge-edit-post)
     ("e a" "assignees" forge-edit-topic-assignees)
     ("e r" "review requests" forge-edit-topic-review-requests)]))

(use-package git-link
  :config
  (setq git-link-open-in-browser t
        git-link-use-commit t))

(use-package emojify)
(use-package gitmoji
  :straight nil
  :load-path "~/projects/emacs-gitmoji")

(use-package default-text-scale)

(use-package html-to-hiccup
  :straight nil
  :load-path "~/projects/html-to-hiccup")

(load-file "./tailwind_cheatsheet.el")
(message "loading secrets...")
(load-file (expand-file-name (concat user-emacs-directory "/secrets.el")))
(message "secrets loaded")


(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))                                                                                    (add-hook 'evil-normal-state-entry-hook (lambda () (send-string-to-terminal "\033[0 q")))

;; `evil-terminal-cursor-changer' utilizes custom terminal escape sequences
;; (which work in most, but not all, terminals) to adjust the appearance of the
;; Emacs cursor based on which Vim mode is currently active. Note that this
;; package is only required when running in a terminal (hence the `unless').
(use-package evil-terminal-cursor-changer
  :config
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate)
    (setq evil-insert-state-cursor 'bar)
    ))
