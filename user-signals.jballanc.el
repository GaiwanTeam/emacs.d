;;; -*- no-byte-compile: t -*-

((neotree-mode (:expand-or-open
                +neotree/expand-or-open

                :collapse-or-up
                +neotree/collapse-or-up))
 (clojure-mode (:run-tests
                +jballanc/smart-test-run-test))
 (cider-mode (:open-other
              cider-repl-switch-to-other))
 (magit-mode (:do-tab
              magit-section-toggle)))
