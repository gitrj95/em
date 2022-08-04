;;;; em init
;;;; rj


(require 'package)
(setq package-archives nil)
(package-initialize)

(defun global-map-set-kbd (cmd-string fcn)
  (define-key global-map (kbd cmd-string) fcn))

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
(global-map-set-kbd "C-M-S-<up>" #'windmove-swap-states-up)
(global-map-set-kbd "C-M-S-<down>" #'windmove-swap-states-down)
(global-map-set-kbd "C-M-S-<left>" #'windmove-swap-states-left)
(global-map-set-kbd "C-M-S-<right>" #'windmove-swap-states-right)

(setq-default line-spacing .1)
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1)
(setq-default scroll-margin 0)
(setq-default next-screen-context-lines 0)
(require 'modus-themes)
(modus-themes-load-themes)
(modus-themes-load-vivendi)
(global-map-set-kbd "<f8>" #'modus-themes-toggle)

(require 'savehist)
(savehist-mode)

(require 'recentf)
(recentf-mode)

(require 'vertico)
(vertico-mode)

(require 'corfu)
(define-key corfu-map (kbd "SPC") 'corfu-insert-separator)
(global-corfu-mode)

(require 'popon)
(require 'corfu-terminal)
(unless (display-graphic-p)
  (corfu-terminal-mode))

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(require 'marginalia)
(marginalia-mode)

(global-map-set-kbd "C-x C-b" #'ibuffer)
(global-map-set-kbd "M-s O" #'multi-occur)
(global-map-set-kbd "M-g i" #'imenu)

(require 'embark)
(setq-default prefix-help-command #'embark-prefix-help-command)
(global-map-set-kbd "C-c e a" #'embark-act)
(global-map-set-kbd "C-c e d" #'embark-dwim)

(setq em-etc-directory
      (file-truename (concat user-emacs-directory "etc/")))
(mapcar #'load (directory-files em-etc-directory t "elc?$"))
