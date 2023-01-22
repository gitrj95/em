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
  :init
  (vertico-mode)
  :bind
  (:map vertico-map
        ("M-DEL" . vertico-directory-delete-word)
        ("M-q" . vertico-quick-insert)))

(use-package corfu
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("M-q" . corfu-quick-insert)))

(use-package corfu-terminal
  :after corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode)))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :config (marginalia-mode))

(use-package embark
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-c e a" . embark-act)
   ("C-c e d" . embark-dwim)))

(use-package consult
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
  ("C-c a" . consult-org-agenda)
  ("C-c h" . consult-org-heading)
  ("C-c k" . consult-keep-lines)
  ("C-c f" . consult-focus-lines)
  (:map minibuffer-local-map
        ("M-h" . consult-history))
  (:map isearch-mode-map
        ("M-g l" . consult-line))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult--regexp-compiler #'consult--orderless-regexp-compiler)
  :hook
  ((embark-collect-mode completion-list-mode) . consult-preview-at-point-mode)
  :config
  (advice-add #'register-preview :override #'consult-register-window))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-dir
  :bind
  ("C-x C-d" . consult-dir)
  (:map vertico-map
        ("C-x C-d" . consult-dir)
        ("C-x C-j" . consult-dir-jump-file)))

(use-package wgrep)

(use-package eglot)

(use-package eat
  :hook
  (eshell-mode . eat-eshell-mode)
  (eshell-mode . eat-eshell-visual-command-mode))

(use-package consult-eglot
  :after (consult eglot)
  :bind
  (:map eglot-mode-map
        ("C-c s" . consult-eglot-symbols)))

(use-package buffer-env
  :hook
  (hack-local-variables . buffer-env-update))

(use-package dogears :demand t
  :init
  (dogears-mode) 
  :custom
  (dogears-idle 1)
  (dogears-hooks nil)
  :bind
  (("M-g d r" . dogears-remember)
   ("M-g d g" . dogears-go)
   ("M-g d b" . dogears-back)
   ("M-g d f" . dogears-forward)
   ("M-g d s" . dogears-sidebar)
   ("M-g d l" . dogears-list)))

(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  :hook
  ((tree-sitter-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package vc
  :bind
  (:map vc-prefix-map
        ("R" . vc-rename-file)
        ("d" . vc-dir-root)
        ("c" . vc-git-commit)))

(use-package magit
  :bind ("C-x g" . magit))

(use-package org
  :bind
  (("C-c l" . org-store-link)))

(use-package denote
  :custom
  (denote-directory (expand-file-name "~/.deft"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  :hook
  (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n i" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n l" . denote-link-find-file)
   ("C-c n r" . denote-dired-rename-file)
   ("C-c n d" . denote-date)
   ("C-c n s" . denote-subdirectory)))

(use-package xeft)

(use-package ef-themes
  :demand t
  :config
  (ef-themes-load-random)
  (set-face-attribute 'default nil :family "Iosevka")
  (set-face-attribute 'default nil :height 160)
  (setq-default line-spacing .1
                scroll-preserve-screen-position t
                scroll-conservatively 1
                scroll-margin 0
                next-screen-context-lines 0
                cursor-type 'box)
  :bind
  (("<f8>" . ef-themes-load-random)))

(use-package savehist
  :init (savehist-mode))

(use-package saveplace
  :init (save-place-mode))

(use-package recentf
  :init (recentf-mode))

(use-package elec-pair
  :init (electric-pair-mode))

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (dictionary-server "dict.org")
  (gc-cons-threshold 100000000)
  :bind
  (("M-s d" . dictionary-search)
   ("C-x C-b" . ibuffer)
   ("C-x P" . proced)))

;;; load etc
(setq em-etc-directory
      (file-truename (concat user-emacs-directory "etc/")))
(mapcar #'load (directory-files em-etc-directory t "elc?$"))
