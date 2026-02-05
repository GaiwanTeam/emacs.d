;;; -*- no-byte-compile: t -*-

;; This is your user signals file, here you configure how certain signals are
;; handled in specific modes.

(

 (lisp-mode ( :eval/last-sexp sly-eval-last-expression
              :eval/buffer sly-eval-buffer
              :eval/last-sexp-pprint sly-pprint-eval-last-expression
              :eval/region sly-eval-region
              :eval/outer-sexp sly-eval-defun
              :repl/toggle sly
              ))
 )
