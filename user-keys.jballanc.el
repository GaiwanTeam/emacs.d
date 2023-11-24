;;; -*- no-byte-compile: t -*-

(bindings
 (global
  ("C-s" "<esc>" evil-escape)
  ("C-u" "Scroll up" evil-scroll-up))

 (normal
  ("l" "Open" :expand-or-open)
  ("h" "Close" :collapse-or-up)
  ("SPC" "Leader"
   ("b" "Buffers"
    ("p" "Previous buffer" previous-buffer)
    ("n" "Next buffer" next-buffer))
   ("i" "Jump next" evil-jump-forward)
   ("q"
    ("q" "Quit" save-buffers-kill-terminal))
   ("t" "Test"
    ("k" "Toggle Kaocha" +jballanc/toggle-kaocha)
    ("t" "Run tests" :run-tests))
   ("w" "Window"
    ("l" "Go to window on right" evil-window-right)
    ("h" "Go to window on left" evil-window-left)
    ("j" "Go to window below" evil-window-down)
    ("k" "Go to window above" evil-window-up)
    ("L" "Move window right" evil-window-move-far-right)
    ("s" "Split window horizontally" evil-window-split)
    ("v" "Split window vertically" evil-window-vsplit)
    ("u" "Undo" winner-undo)
    ("m" "Maximize"
     ("m" "Maximize window" delete-other-windows)))))

 (normal|visual
  ("SPC" "Leader"
   ("g" "Git"
    ("B" "Git blame" magit-blame)
    ("g" "Magit" magit))
   ("k" "S-exp"
    ("j" "Join" sp-join-sexp)
    ("o" "Open" sp-split-sexp)
    ("b" "Barf" sp-forward-barf-sexp)
    ("B" "Barf backward" sp-backward-barf-sexp)
    ("r" "Raise S-exp" sp-raise-sexp)
    ("s" "Slurp" sp-forward-slurp-sexp)
    ("S" "Slurp backward" sp-backward-slurp-sexp)
    ("t" "Transpose" sp-transpose-sexp)
    ("w" "Wrap with ()" sp-wrap-round)
    ("[" "Wrap with []" sp-wrap-square)
    ("{" "Wrap with {}" sp-wrap-curly))
   ("o" "Open"
    ("p" "Open Neotree" neotree-toggle)
    ("u" "Open URL at point" browse-url-at-point)
    ("s" "Edit string at point" string-edit-at-point))
   ("s" "Search"
    ("p" "Search in project" projectile-ripgrep))))

 (normal|visual|insert
  ("C-a" "Start of line" +jballanc/bol-or-bti)
  ("C-e" "End of line" end-of-line))

 (insert
  ("C-o" "Newline below" open-line)
  ;; This is actually "ctrl-SPC", but for some reason Emacs interprets that sequence as 'C-@'
  ("C-@" +jballanc/space-after)))
