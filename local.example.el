;; Example local.el file to set some extra things up the way you like them.
;; You can symlink this in and version it with your dotfiles.

(set-frame-font "Iosevka Fixed SS14-14")
(setq cider-repl-display-help-banner nil)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-allow-jack-in-without-project t)
(setq cljr-suppress-no-project-warning t)
(setq desktop-restore-frames nil)
(desktop-save-mode 1)
