;;;; em base
;;;; rj


(require 'package)
(setq package-selected-packages
      '(modus-themes vertico orderless marginalia consult embark embark-consult eglot))
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

(require 'consult)
(recentf-mode)
(global-map-set-kbd "C-x C-b" #'ibuffer)
(global-map-set-kbd "C-x b" #'consult-buffer)
(global-map-set-kbd "M-g m" #'consult-mark)
(global-map-set-kbd "M-g M" #'consult-global-mark)
(global-map-set-kbd "M-g i" #'consult-imenu)
(global-map-set-kbd "M-g I" #'consult-imenu-multi)
(global-map-set-kbd "C-h a" #'consult-apropos)
(global-map-set-kbd "C-h d" #'consult-man)
(global-map-set-kbd "C-x r r" #'consult-register)
(global-map-set-kbd "C-x r s" #'consult-register-store)
(global-map-set-kbd "C-x r l" #'consult-register-load)
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))
(setq consult-project-root-function
      (lambda ()
        (when-let (project (project-current))
          (car (project-roots project)))))
(advice-add #'register-preview :override #'consult-register-window)
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(require 'embark)
(setq-default prefix-help-command #'embark-prefix-help-command)
(global-map-set-kbd "C-c e a" #'embark-act)
(global-map-set-kbd "C-c e d" #'embark-dwim)

(require 'embark-consult)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(setq em-etc-directory (file-truename "etc/"))
(unless (file-directory-p em-etc-directory)
  (make-directory em-etc-directory))
(mapcar #'load
	(directory-files em-etc-directory t "elc?$"))
