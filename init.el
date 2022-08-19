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

(global-map-set-kbd "C-x u" #'undo-only)
(global-map-set-kbd "C-x R" #'undo-redo)

(require 'modus-themes)
(setq-default line-spacing .1)
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1)
(setq-default scroll-margin 0)
(setq-default next-screen-context-lines 0)
(modus-themes-load-themes)
(modus-themes-load-vivendi)
(global-map-set-kbd "<f8>" #'modus-themes-toggle)

(require 'savehist)
(setq history-length 10)
(savehist-mode)

(require 'saveplace)
(save-place-mode)

(require 'recentf)
(recentf-mode)

(require 'vertico)
(vertico-mode)
(vertico-multiform-mode)
(setq vertico-multiform-commands
      '((imenu buffer)))
(setq vertico-multiform-categories
      '((file buffer)))

(require 'vertico-directory)
(define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
(add-hook #'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(require 'vertico-quick)
(define-key vertico-map (kbd "M-q") #'vertico-quick-insert)
(define-key vertico-map (kbd "C-q") #'vertico-quick-exit)

(require 'corfu)
(define-key corfu-map (kbd "SPC") #'corfu-insert-separator)
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
