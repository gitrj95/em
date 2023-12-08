;;;; em init
;;;; rj

(eval-when-compile (require 'use-package))
(use-package emacs
  :custom
  (package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
  (use-package-always-ensure t)
  (read-process-output-max (* 4 1024 1024))
  (enable-recursive-minibuffers t)
  (use-package-enable-imenu-support t)
  (org-startup-indented t)
  (native-comp-async-report-warnings-errors nil)
  (custom-file (make-temp-file "emacs-custom"))
  :init
  (package-initialize)
  (setq em-notes-directory "~/notes")
  (when (string= system-type "darwin")
    (when-let ((ls-exe (executable-find "gls")))
      (setq dired-use-ls-dired t
            insert-directory-program ls-exe)))
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (let ((expanded-f (expand-file-name em-notes-directory)))
    (unless (file-directory-p expanded-f)
      (make-directory expanded-f)))
  :bind
  (("C-<left>" . previous-buffer)
   ("C-<right>" . next-buffer)))

(use-package ef-themes
  ;; use this for gui-related things
  :custom
  (mode-line-format
   '("%e" mode-line-front-space
     (:propertize
      ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
      display
      (min-width
       (1.0)))
     mode-line-frame-identification mode-line-buffer-identification " "
     " " mode-line-misc-info mode-line-end-spaces))
  (display-time-default-load-average nil)
  :init
  (set-face-attribute 'default nil :family "Iosevka Comfy Fixed")
  (set-face-attribute 'default nil :height 160)
  (setq-default line-spacing .1
                scroll-preserve-screen-position t
                scroll-conservatively 1
                scroll-margin 0
                next-screen-context-lines 0
                cursor-type 'box)
  (display-time-mode +1)
  (column-number-mode +1)
  (scroll-bar-mode -1)
  (pixel-scroll-precision-mode +1)
  (defun em-load-light-theme ()
    (interactive)
    (ef-themes-load-random 'light))
  (defun em-load-dark-theme ()
    (interactive)
    (ef-themes-load-random 'dark))
  :bind
  (("<f7>" . em-load-light-theme)
   ("<f8>" . em-load-dark-theme)))


(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH"))
  :init
  (when (or (memq window-system '(mac ns x))
	    (daemonp))
    (exec-path-from-shell-initialize)))

(use-package windmove
  :bind
  (("C-M-<up>" . windmove-up)
   ("C-M-<down>" . windmove-down)
   ("C-M-<left>" . windmove-left)
   ("C-M-<right>" . windmove-right)))

(unless (package-installed-p 'trail)
  (package-vc-install "https://github.com/gitrj95/trail.el"))
(use-package trail
  :after savehist
  :custom
  (trail-mark-around-functions
   '(xref-find-definitions xref-find-references org-open-at-point))
  (trail-ring-max 100)
  :init
  (trail-mode)
  :bind
  ("C-M-=" . trail-mark)
  ("C-M-'" . trail-list)
  ("C-M-[" . trail-find-and-jump-previous)
  ("C-M-]" . trail-find-and-jump-next))

(use-package vundo
  :demand t
  :bind ("C-x u" . vundo))

(use-package expreg
  :bind
  ("C-@" . expreg-expand)
  ("C-#" . expreg-contract))

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

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
  (corfu-echo-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("M-q" . corfu-quick-insert)))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-terminal
  :if
  (not (display-graphic-p))
  :init
  (corfu-terminal-mode))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

(use-package embark
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-;" . embark-act)
   ("C-'" . embark-dwim)))

(use-package consult
  :bind
  ("M-y" . consult-yank-pop)
  ("M-g g" . consult-goto-line)
  ("M-g l" . consult-line)
  ("M-g L" . consult-line-multi)
  ("M-g i" . consult-imenu)
  ("M-g o" . consult-outline)
  ("M-g I" . consult-imenu-multi)
  ("M-g m" . consult-mark)
  ("M-g M" . consult-global-mark)
  ("M-g f" . consult-find)
  ("M-s i" . consult-info)
  ("M-s m" . consult-man)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-X" . consult-mode-command)
  ("C-x b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-c k" . consult-keep-lines)
  ("C-c f" . consult-focus-lines)
  ("M-`" . consult-register-store)
  ("C-`" . consult-register-load)
  ("C-M-`" . consult-register)
  (:map minibuffer-local-map
        ("M-h" . consult-history))
  (:map isearch-mode-map
        ("M-g l" . consult-line))
  :custom
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :hook
  ((embark-collect-mode completion-list-mode) . consult-preview-at-point-mode)
  :init
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

(use-package avy
  :bind
  ("M-g c" . avy-goto-char-timer)
  (:map isearch-mode-map
        ("M-g c" . avy-isearch)))

(use-package jinx
  :custom
  (global-jinx-modes t)
  :init
  (global-jinx-mode)
  :bind
  ("M-$" . jinx-correct))

(use-package wgrep)

(use-package eglot
  :custom
  (eglot-sync-connect 0)
  :init
  (setq eglot-stay-out-of '(flymake))
  (add-hook
   'eglot--managed-mode-hook
   (lambda ()
     ;; don't clobber diagnostics backends
     (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
     (flymake-mode 1)))
  :config
  (define-key eglot-mode-map (kbd "<f5>") #'eglot-format)
  (define-key eglot-mode-map (kbd "<f6>") #'eglot-rename))

(use-package hl-todo
  :custom
  (hl-todo-keyword-faces
   '(("TODO" . hl-line-face)
     ("NOTE" . hl-line-face)
     ("DEBUG" . compilation-error-face)
     ("FIXME" . compilation-error-face)))
  :init
  (global-hl-todo-mode)
  (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake))

(use-package consult-eglot
  :after (consult eglot)
  :config
  (define-key eglot-mode-map (kbd "M-g s") #'consult-eglot-symbols)
  (define-key eglot-mode-map (kbd "M-g e e") #'consult-flymake)
  (define-key eglot-mode-map (kbd "M-g e n") #'flymake-goto-next-error)
  (define-key eglot-mode-map (kbd "M-g e p") #'flymake-goto-prev-error))

(use-package buffer-env)

(use-package eat
  :bind
  ("<f9>" . eshell)
  :custom
  (eat-term-name "eat-mono")
  :hook
  (eshell-mode . eat-eshell-mode)
  (eshell-mode . eat-eshell-visual-command-mode))

(use-package vc
  :bind
  (:map vc-prefix-map
        ("R" . vc-rename-file)
        ("d" . vc-dir-root)
        ("c" . vc-git-commit)))

(use-package magit
  :bind ("C-x g" . magit))

(use-package tmr)

(use-package org
  :custom
  (org-agenda-files `(,em-notes-directory))
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  :config
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)))

(use-package org-modern
  :if (display-graphic-p)
  :custom
  (org-modern-table nil)
  :init
  (global-org-modern-mode))

(unless (package-installed-p 'org-modern-indent)
  (package-vc-install "https://github.com/jdtsmith/org-modern-indent"))
(use-package org-modern-indent
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :after all-the-icons
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package denote
  :custom
  (denote-directory (expand-file-name em-notes-directory))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  :hook
  (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n l" . denote-link)
   ("C-c n i" . denote-find-link)
   ("C-c n b" . denote-find-backlink)
   ("C-c n B" . denote-backlinks)
   ("C-c n r" . denote-rename-file-using-front-matter)
   ("C-c n d" . denote-date)
   ("C-c n s" . denote-subdirectory)
   ("C-c n a" . denote-keywords-add)
   ("C-c n k" . denote-keywords-remove)))

(use-package consult-notes
  :custom
  (consult-notes-denote-dir nil)
  :config
  (consult-notes-denote-mode)
  :bind
  (("C-c n M-g" . consult-notes)
   ("C-c n M-s" . consult-notes-search-in-all-notes)))

(use-package pdf-tools
  :init
  (pdf-loader-install))

(use-package elfeed
  :bind
  ("C-x w" . elfeed))

(use-package notmuch
  :custom
  (notmuch-search-oldest-first nil))

(use-package savehist
  :init
  (savehist-mode)
  (setq savehist-additional-variables
        '(trail-ring)))

(use-package saveplace
  :init (save-place-mode))

(use-package recentf
  :init (recentf-mode))

(use-package repeat
  :init (repeat-mode))

(use-package proced
  :bind ("C-x P" . proced)
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm))))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

;;; load etc
(setq em-etc-directory
      (file-truename (concat user-emacs-directory "etc/")))
(mapcar #'load (directory-files em-etc-directory t "elc?$"))

;;; load light/dark theme via user
(let ((current-prefix-arg t))
  (call-interactively #'ef-themes-load-random))
