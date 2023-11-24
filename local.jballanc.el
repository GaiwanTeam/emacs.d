;; ========
;; Packages
;; ========

;; Enhancements  to `evil'
;; -----------------------

;; `evil-escape' extends the usual behavior of <esc> in evil to allow escaping
;; from *nearly* everything (not just insert mode). This package also allows you
;; to specify a two-character sequence that will trigger <esc> in insert mode
;; (but I don't use that feature).
(use-package evil-escape)

;; `evil-smartparens' prevents certain Vim-style commands (like 'D' to delete to
;; the end of a line) from leaving a buffer with un-balanced delimiters.
(use-package evil-smartparens
  :after (evil smartparens)
  :commands evil-smartparens-mode
  :init
  (add-hook 'smartparens-enabled-hook
            #'evil-smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook
            #'evil-smartparens-mode))

;; `evil-visualstar' adds the ability to highlight a region in visual mode and
;; then use '*' or '#' to search the buffer for the highlighted region. This is
;; especially useful with 'hyphenated-terms' where the default behaviour of '*'
;; in evil will only search for the token before the '-'.
(use-package evil-visualstar)

;; `evil-terminal-cursor-changer' utilizes custom terminal escape sequences
;; (which work in most, but not all, terminals) to adjust the appearance of the
;; Emacs cursor based on which Vim mode is currently active. Note that this
;; package is only required when running in a terminal (hence the `unless').
(use-package evil-terminal-cursor-changer
  :config
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate)))


;; Additional Tools
;; ----------------

;; `rg' uses ripgrep (instead of grep) to search. Requires installation of `rg'
;; independant of the Emacs package install.
(use-package rg)

;; `neotree' is a port of Vim's venerable NERDTree package to Emacs.
;; Technically, it's not as capable as Treemacs, but I prefer it for the
;; keybindings and config I can set up.
(use-package neotree)


;; UI Enhancements
;; ---------------

;; In Emacs, everything is a buffer. Some buffers correspond to files on disk.
;; The rest are...other things. `solair-mode' helps you distinguish between the
;; two categories by subtly darkening the background color of file-based
;; buffers.
(use-package solaire-mode
  :config
  (solaire-global-mode +1))

;; `all-the-icons' is a package that installs and works with various icon fonts
;; for fun little embellishments.
(use-package all-the-icons)

;; `doom-themes' is an alternative theming library (extracted from the
;; `doomemacs' project). I like it's version of Solar.
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-oksolar-dark t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))


;; ========
;; Settings
;; ========

;; Enable `winner-mode' which tracks the layout of windows in Emacs, making it
;; easy to `winner-undo' and return to a prior layout.
(winner-mode)

;; General settings (`setq' can take an arbitrary number of `variable value'
;; pairs)
(setq
 ;; This first variable is needed for a function later on
 cider-use-kaocha nil

 ;; Customize CIDER to be quieter and less complain-y
 cider-repl-display-help-banner nil
 cider-repl-pop-to-buffer-on-connect nil
 cider-allow-jack-in-without-project t
 cljr-suppress-no-project-warning t

 ;; Turn off the default persistent highlighting when searching with '/'
 evil-ex-search-persistent-highlight nil

 ;; Allow point to move past end-of-line (useful for eval-ing forms without
 ;; having to move to the line below)
 evil-move-beyond-eol t
 evil-move-cursor-back nil

 ;; Configure my Evil cursors
 evil-insert-state-cursor 'bar
 evil-replace-state-cursor 'hbar

 ;; With autorefresh on, neotree can really slow things down if you leave the
 ;; buffer open
 neo-autorefresh nil

 ;; Neotree should jump to the current file in the panel by default and revert
 ;; to its default size
 neo-smart-open t
 neo-reset-size-on-open t

 ;; Hybrid projectile indexing allows use of filters but isn't as slow as native
 projectile-indexing-method 'hybrid)

;; Add a few Magit options that are normally only available at a higher
;; `transient-default-level', but which are useful to have at hand
(transient-append-suffix 'magit-fetch "-p"
  '("-t" "Fetch all tags" ("-t" "--tags")))
(transient-append-suffix 'magit-pull "-r"
  '("-a" "Autostash" "--autostash"))


;; =========
;; Functions
;; =========

(defun +jballanc/space-after ()
  "Very simple function that exists simply to be bound to a key and enable
  entering a space *after* the point"
  (interactive)
  (save-excursion (insert " ")))

(defun +jballanc/bol-or-bti ()
  "Initially move the point to the first non-whitespace character on the line.
  Run again, move the point to the absolute start of the line. Further
  invocations toggle between these two positions."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (if (= oldpos (point))
        (beginning-of-line))))

(defun +jballanc/toggle-kaocha ()
  "This function, in combination with `+jballanc/smart-test-run-test', enables
  quickly switching between projects that utilize Kaocha as their test running
  and those that use the default Clojure runner"
  (interactive)
  (if cider-use-kaocha
      (progn
        (message "Switching to Clojure test runner")
        (setq cider-use-kaocha nil))
    (progn
      (message "Switching to Kaocha test runner")
      (setq cider-use-kaocha t))))

(defun +jballanc/smart-test-run-test ()
  "Switch between the two kinds of Clojure test runners"
  (interactive)
  (cider-eval-buffer)
  (if cider-use-kaocha
      (kaocha-runner-run-test-at-point)
    (cider-test-run-test)))

(defun +neotree/collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (if (and (file-directory-p node)
             (neo-buffer--expanded-node-p node))
        (+neotree/collapse)
      (neotree-select-up-node))))

(defun +neotree/collapse ()
  "Collapse a neotree node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (when (file-directory-p node)
      (neo-buffer--set-expand node nil)
      (neo-buffer--refresh t))
    (when neo-auto-indent-point
      (neo-point-auto-indent))))

(defun +neotree/expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (cond ((file-directory-p node)
           (neo-buffer--set-expand node t)
           (neo-buffer--refresh t)
           (when neo-auto-indent-point
             (forward-line)
             (neo-point-auto-indent)))
          (t
           (call-interactively #'neotree-enter)))))
