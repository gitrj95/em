;;;; Em init
;;;; rj

;;; Comms

(let ((expanded-f (expand-file-name em-notes-directory)))
  (unless (file-directory-p expanded-f)
    (make-directory expanded-f)))

(use-package org
  :custom
  (org-agenda-files `(,em-notes-directory))
  (org-startup-indented 1)
  :config
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)))

(use-package org-present
  :defer t)

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
  (consult-notes-denote-mode 1)
  (setq consult-notes-file-dir-sources
	`(("All notes" ?a ,em-notes-directory)))
  :bind
  (("C-c n M-g" . consult-notes)
   ("C-c n M-s" . consult-notes-search-in-all-notes)))

(use-package crdt
  :defer t)

;;; Navigation

(setq isearch-lazy-count t
      search-ring-max 100
      regexp-search-ring-max 100)

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
  (trail-mode 1)
  :bind
  ("s-'" . trail-mark)
  ("s-[" . trail-find-and-jump-previous)
  ("s-]" . trail-find-and-jump-next))

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

;;; Completion & Commands

(use-package vertico
  :custom
  (vertico-count 3)
  (enable-recursive-minibuffers)
  :init
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (vertico-mode)
  :bind
  ("M-S-r" . vertico-repeat)
  (:map vertico-map
	("M-S-n" . vertico-repeat-next)
	("M-S-p" . vertico-repeat-previous)
        ("M-DEL" . vertico-directory-delete-word)))

(use-package cape
  :bind (("M-<tab>" . completion-at-point)
         ("C-c c d" . cape-dabbrev)
	 ("C-c c h" . cape-history)
         ("C-c c f" . cape-file)
         ("C-c c l" . cape-line)
         ("C-c c w" . cape-dict)
         ("C-c c :" . cape-emoji)
         ("C-c c \\" . cape-tex))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-tex))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :config (marginalia-mode 1))

(use-package embark
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  (embark-prompter #'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator
		       embark-highlight-indicator
		       embark-isearch-highlight-indicator))
  :bind
  (("C-;" . embark-act)
   ("C-s-;" . embark-act-all)))

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
  ("M-g F" . consult-fd)
  ("M-s i" . consult-info)
  ("M-s m" . consult-man)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-g h" . consult-mode-command)
  ("C-x b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-c k" . consult-keep-lines)
  ("C-c f" . consult-focus-lines)
  ("C-`" . consult-register-store)
  ("C-s-`" . consult-register-load)
  ("M-s-`" . consult-bookmark)
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
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use `consult-completion-in-region' if vertico is enabled
  (setq completion-in-region-function
	(lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
		 args)))
  :config
  ;; Configure new bookmark-view source
  (add-to-list 'consult-buffer-sources
               (list :name     "View"
                     :narrow   ?v
                     :category 'bookmark
                     :face     'font-lock-keyword-face
                     :history  'bookmark-view-history
                     :action   #'consult--bookmark-action
                     :items    #'bookmark-view-names)
               'append)
  ;; Modify bookmark source, such that views are hidden
  (setq consult--source-bookmark
	(plist-put
	 consult--source-bookmark :items
	 (lambda ()
           (bookmark-maybe-load-default-file)
           (mapcar #'car
                   (seq-remove (lambda (x)
				 (eq #'bookmark-view-handler
                                     (alist-get 'handler (cdr x))))
                               bookmark-alist))))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package kkp
  :config
  (global-kkp-mode 1)
  (define-key key-translation-map (kbd "M-<backspace>") (kbd "M-DEL")) ; FIXME: hack
  (define-key key-translation-map (kbd "M-<return>") (kbd "M-RET")))   ; FIXME: hack

;;; GUI

(use-package circadian
  :config
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package hl-todo
  :config
  (global-hl-todo-mode 1)
  (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake))

(use-package spacious-padding
  :config
  (spacious-padding-mode 1))

;;; Editing

(use-package jinx
  :bind
  ("M-$" . jinx-correct))

(use-package wgrep)

(use-package eglot
  :custom
  (eglot-sync-connect 0)
  (eglot-connect-timeout 10) ; NOTE: can just `C-g' out of it
  (project-vc-extra-root-markers '(".project"))
  :config
  (setq eglot-stay-out-of '(flymake))
  (add-hook
   'eglot-managed-mode-hook
   (lambda ()
     ;; NOTE: don't clobber diagnostics backends
     (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
     (flymake-mode 1)))
  (defun em/eglot-toggle ()
    "Toggles `eglot'."
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
	("<f8>" . compile)
	("M-g s" . consult-eglot-symbols)))

(use-package dape
  :custom
  (dape-buffer-window-arrangement 'gud))

(use-package rmsbolt)

(unless (package-installed-p 'eglot-booster)
  (package-vc-install "https://github.com/jdtsmith/eglot-booster.git"))
(use-package eglot-booster
  :config
  (add-hook
   'eglot-managed-mode-hook
   (lambda ()
     ;; NOTE: boost only if binary exists
     (if (eglot-managed-p)
	 (when (executable-find "emacs-lsp-booster")
	   (eglot-booster-mode 1))
       (eglot-booster-mode -1)))))

;;; Env

(use-package savehist
  :init
  (savehist-mode 1)
  (setq savehist-additional-variables
        (append savehist-additional-variables '(trail-ring vertico-repeat-history))))

(use-package buffer-env
  :config
  (add-hook 'hack-local-variables-hook #'buffer-env-update)
  (add-hook 'comint-mode-hook #'buffer-env-update))

(use-package eat
  ;; NOTE: extant bug in `https://codeberg.org/akib/emacs-eat/issues/109'
  :config
  (defun em/choose-term-interface (cmd)
    "Chooses a terminal interface among `em-terminal-modes-alist'."
    (interactive
     (list
      (let ((choice
	     (completing-read "Choose terminal interface: " em-terminal-modes-alist)))
	(cdr (assoc choice em-terminal-modes-alist)))))
    (call-interactively cmd))
  :bind
  ("<f9>" . em/choose-term-interface)
  :hook
  (eshell-mode . eat-eshell-mode)
  (eshell-mode . eat-eshell-visual-command-mode))

(use-package ansi-color
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

(use-package vc
  :bind
  (:map vc-prefix-map
        ("R" . vc-rename-file)
        ("d" . vc-dir-root)
        ("c" . vc-git-commit)))

(use-package magit
  :bind ("C-x g" . magit))

(use-package repeat
  :config (repeat-mode 1))

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

(use-package flymake
  :bind
  (:map flymake-mode-map
	("M-g d" . consult-flymake)))

(use-package bookmark-view
  :bind
  ("M-`" . bookmark-view-save))

;;; Load etc

(setq em-etc-directory
      (file-truename (concat user-emacs-directory "etc/")))
(mapc #'load (directory-files em-etc-directory t "elc?$"))
