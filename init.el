;;;; em
;;;; rj


(defun global-map-set-kbd (cmd-string fcn)
  (define-key global-map (kbd cmd-string) fcn))

(require 'package)
(setq package-selected-packages
      '(modus-themes vertico orderless marginalia))
(package-initialize)

(require 'org)
(require 'org-tempo)
(global-map-set-kbd "C-c a" #'org-agenda)
(global-map-set-kbd "C-c c" #'org-capture)
(global-map-set-kbd "C-c l" #'org-store-link)
(global-map-set-kbd "C-c C-l" #'org-insert-link)

(require 'windmove)
(global-map-set-kbd "C-M-<up>" #'windmove-up)
(global-map-set-kbd "C-M-<down>" #'windmove-down)
(global-map-set-kbd "C-M-<left>" #'windmove-left)
(global-map-set-kbd "C-M-<right>" #'windmove-right)

(setq-default line-spacing .1)
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1)
(setq-default scroll-margin 0)
(setq-default next-screen-context-lines 0)
(require 'modus-themes)
(modus-themes-load-themes)
(modus-themes-load-vivendi)
(global-map-set-kbd "<f8>" #'modus-themes-toggle)

(require 'vertico)
(vertico-mode)

(require 'marginalia)
(marginalia-mode)

(require 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)
      completion-category-overrides '((file (styles basic partial-completion)))
      enable-recursive-minibuffers t)
