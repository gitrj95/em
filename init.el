;;;; em init
;;;; rj


(setq custom-file (make-temp-file "emacs-custom"))
(when (native-comp-available-p)
  (setq-default native-comp-async-report-warnings-errors nil))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(require 'package)
(package-initialize)

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(custom-set-variables
 '(use-package-enable-imenu-support t))
(eval-when-compile (require 'use-package))

(use-package exec-path-from-shell :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package org
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c C-l" . org-insert-link)))

(use-package windmove
  :bind
  (("C-M-<up>" . windmove-up)
   ("C-M-<down>" . windmove-down)
   ("C-M-<left>" . windmove-left)
   ("C-M-<right>" . windmove-right)
   ("C-M-S-<up>" . windmove-swap-states-up)
   ("C-M-S-<down>" . windmove-swap-states-down)
   ("C-M-S-<left>" . windmove-swap-states-left)
   ("C-M-S-<right>" . windmove-swap-states-right)))

(use-package vundo
  :bind (("C-x u" . vundo)))

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package vertico
  :demand t
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  (setq vertico-multiform-commands '((imenu buffer))
        vertico-multiform-categories '((file buffer)))
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)
              ("M-q" . vertico-quick-insert)))

(use-package corfu
  :demand t
  :config
  (global-corfu-mode)
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator)))

(use-package corfu-terminal
  :demand t
  :after corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode)))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :demand t
  :config (marginalia-mode))

(use-package embark
  :demand t
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-c e a" . embark-act)
   ("C-c e d" . embark-dwim)))

(use-package consult
  :ensure t
  :bind
  ("M-y" . consult-yank-pop)
  ("M-g l" . consult-line)
  ("M-g i" . consult-imenu)
  ("M-g o" . consult-outline)
  ("M-g I" . consult-imenu-multi)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-s m" . consult-multi-occur)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-g f" . consult-find)
  ("M-X" . consult-mode-command)
  ("C-x b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-c k" . consult-keep-lines)
  ("C-c f" . consult-focus-lines)
  (:map minibuffer-local-map
        ("M-h" . consult-history))
  (:map isearch-mode-map
        ("M-g l" . consult-line))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (register-preview-function #'consult-register-format)
  (consult-narrow-key "<")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :hook
  ((embark-collect-mode completion-list-mode) . consult-preview-at-point-mode)
  (minibuffer-setup . choose-completion-in-region)
  :config
  (defun choose-completion-in-region ()
    "Use default `completion--in-region' unless we are not completing."
    (when minibuffer-completion-table
      (setq-local completion-in-region-function #'completion--in-region)))
  (when (eq (window-system) 'w32)
    (setq consult-find-args
          (replace-regexp-in-string "\\*" "\\\\*" consult-find-args)))
  (advice-add #'register-preview :override #'consult-register-window)
  (setf (alist-get 'log-edit-mode consult-mode-histories)
        'log-edit-comment-ring))

(use-package embark-consult :after (embark consult))

(use-package wgrep)

(use-package eglot
  :config
  (electric-pair-mode))

(use-package vc
  :bind
  (:map vc-prefix-map
        ("R" . vc-rename-file)
        ("d" . vc-dir-root)
        ("c" . vc-git-commit)))

(use-package magit
  :bind ("C-x g" . magit))

(use-package modus-themes
  :demand t
  :config
  (load-theme 'modus-vivendi :no-confirm)
  (set-face-attribute 'default nil :family "Iosevka")
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
  (set-face-attribute 'default nil :height 160)
  (setq-default line-spacing .1)
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1)
  (setq-default scroll-margin 0)
  (setq-default next-screen-context-lines 0)
  :bind
  (("<f8>" . modus-themes-toggle)))

(use-package emacs
  :config
  (savehist-mode)
  (save-place-mode)
  (recentf-mode)
  (setq dictionary-server "dict.org")
  (setq gc-cons-threshold 100000000)
  :bind
  (("M-s d" . dictionary-search)
   ("C-x P" . proced)))

;;; load etc
(setq em-etc-directory
      (file-truename (concat user-emacs-directory "etc/")))
(mapcar #'load (directory-files em-etc-directory t "elc?$"))
