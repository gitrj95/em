;;;; em init
;;;; rj

;;; comms

(let ((expanded-f (expand-file-name em-notes-directory)))
  (unless (file-directory-p expanded-f)
    (make-directory expanded-f)))

(use-package org
  :custom
  (org-agenda-files `(,em-notes-directory))
  (org-startup-indented +1)
  :config
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)))

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
  (consult-notes-denote-mode +1)
  :bind
  (("C-c n M-g" . consult-notes)
   ("C-c n M-s" . consult-notes-search-in-all-notes)))

(use-package pdf-tools
  :config
  (pdf-loader-install))

(use-package elfeed
  :bind
  ("C-x w" . elfeed))

(use-package crdt)

(use-package notmuch
  :custom
  (notmuch-search-oldest-first nil))

;;; navigation

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

(use-package avy
  :bind
  ("M-g c" . avy-goto-char-timer)
  (:map isearch-mode-map
        ("M-g c" . avy-isearch)))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package consult-dir
  :bind
  ("C-x C-d" . consult-dir)
  (:map vertico-map
        ("C-x C-d" . consult-dir)
        ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-eglot
  :after (consult eglot))

;;; completion

(use-package vertico
  :custom
  (vertico-count 3)
  (enable-recursive-minibuffers)
  :init
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (vertico-mode)
  :bind
  ("M-R" . vertico-repeat)
  (:map vertico-map
	("M-N" . vertico-repeat-next)
	("M-P" . vertico-repeat-previous)
        ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :config (marginalia-mode))

(use-package embark
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-;" . embark-act)
   ("C-M-;" . embark-act-all)
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
  :config
  (advice-add #'register-preview :override #'consult-register-window)
  ;; NOTE: use `consult-completion-in-region' if vertico is enabled
  (setq completion-in-region-function
	(lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
		 args))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; gui

(use-package circadian
  :config
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package org-modern
  :if (display-graphic-p)
  :custom
  (org-modern-hide-stars nil)
  (org-modern-table nil)
  (org-modern-list
   '((?* . "•")
     (?+ . "‣")))
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(unless (package-installed-p 'org-modern-indent)
  (package-vc-install "https://github.com/jdtsmith/org-modern-indent"))
(use-package org-modern-indent
  :if (display-graphic-p)
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package hl-todo
  :custom
  (hl-todo-keyword-faces
   '(("TODO" . hl-line-face)
     ("NOTE" . hl-line-face)
     ("DEBUG" . compilation-error-face)
     ("FIXME" . compilation-error-face)))
  :config
  (global-hl-todo-mode)
  (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake))

;;; editing

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
  :config
  (setq eglot-stay-out-of '(flymake))
  (add-hook
   'eglot-managed-mode-hook
   (lambda ()
     ;; NOTE: don't clobber diagnostics backends
     (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
     (flymake-mode +1)))
  (defun em/eglot-toggle ()
    (interactive)
    (call-interactively
     (if (eglot-managed-p)
	 #'eglot-shutdown
       #'eglot)))
  :bind
  ("<f5>" . em/eglot-toggle)
  (:map eglot-mode-map
	("<f6>" . eglot-format)
	("<f7>" . eglot-rename)
	("M-g s" . consult-eglot-symbols)
	("M-g d" . consult-flymake)))

;;; env

(use-package savehist
  :init
  (savehist-mode)
  (setq savehist-additional-variables
        '(trail-ring vertico-repeat-history)))

(use-package buffer-env
  :init
  (add-hook 'hack-local-variables-hook #'buffer-env-update)
  (add-hook 'comint-mode-hook #'buffer-env-update))

(use-package eat
  ;; extant bug in `https://codeberg.org/akib/emacs-eat/issues/109'
  :bind
  ("<f9>" . eshell)
  :hook
  (eshell-mode . eat-eshell-mode)
  (eshell-mode . eat-eshell-visual-command-mode))

(use-package ansi-color
  :config
  (defun em/colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  (add-hook 'compilation-filter-hook #'em/colorize-compilation-buffer))

(use-package vc
  :bind
  (:map vc-prefix-map
        ("R" . vc-rename-file)
        ("d" . vc-dir-root)
        ("c" . vc-git-commit)))

(use-package magit
  :bind ("C-x g" . magit))

(use-package repeat
  :config (repeat-mode))

(use-package proced
  :bind ("C-x P" . proced)
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'em)
  (proced-sort 'rss)
  :config
  (add-to-list
   'proced-format-alist
   '(em user group pid tree pgrp sess rss pmem pcpu start time state (args comm))))

;;; load etc

(setq em-etc-directory
      (file-truename (concat user-emacs-directory "etc/")))
(mapc #'load (directory-files em-etc-directory t "elc?$"))
