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
  :config (pdf-loader-install))

(use-package crdt
  :defer t)

(use-package notmuch
  :defer t)

(use-package elfeed
  :defer t)

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
  (trail-mode +1)
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
  ("M-g c" . avy-goto-char-timer))

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

(use-package isearch-mb
  :custom
  (isearch-lazy-count t)
  (search-ring-max 100)
  (regexp-search-ring-max 100)
  :init
  (isearch-mb-mode +1)
  (add-to-list 'isearch-mb--after-exit #'avy-isearch)
  (define-key isearch-mb-minibuffer-map (kbd "M-g c") #'avy-isearch)
  (add-to-list 'isearch-mb--after-exit #'consult-line)
  (define-key isearch-mb-minibuffer-map (kbd "M-s l") #'consult-line)
  (add-to-list 'isearch-mb--after-exit #'embark-act)
  (define-key isearch-mb-minibuffer-map (kbd "C-;") #'embark-act))

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
  :config (marginalia-mode +1))

(use-package embark
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  (embark-prompter #'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator
		       embark-highlight-indicator
		       embark-isearch-highlight-indicator))
  :bind
  (("C-;" . embark-act)
   ("C-M-;" . embark-act-all)))

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
  ("C-s-`" . consult-bookmark)
  (:map minibuffer-local-map
        ("M-h" . consult-history))
  :custom
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :hook
  ((embark-collect-mode completion-list-mode) . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  ;; NOTE: use `consult-completion-in-region' if vertico is enabled
  (setq completion-in-region-function
	(lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
		 args)))
  :config
  ;; NOTE: configure new bookmark-view source
  (add-to-list 'consult-buffer-sources
               (list :name     "View"
                     :narrow   ?v
                     :category 'bookmark
                     :face     'font-lock-keyword-face
                     :history  'bookmark-view-history
                     :action   #'consult--bookmark-action
                     :items    #'bookmark-view-names)
               'append)

  ;; NOTE: modify bookmark source, such that views are hidden
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

;;; gui

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
  :hook
  (org-mode . org-modern-indent-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode +1)
  (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake))

(use-package spacious-padding
  :config
  (spacious-padding-mode +1))

;;; editing

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
     (flymake-mode +1)))
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

;;; env

(use-package savehist
  :init
  (savehist-mode +1)
  (setq savehist-additional-variables
        (append savehist-additional-variables '(trail-ring vertico-repeat-history))))

(use-package buffer-env
  :config
  (add-hook 'hack-local-variables-hook #'buffer-env-update)
  (add-hook 'comint-mode-hook #'buffer-env-update))

(use-package eat
  ;; extant bug in `https://codeberg.org/akib/emacs-eat/issues/109'
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
  :config (repeat-mode +1))

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
  ("s-`" . bookmark-view-save))

;;; load etc

(setq em-etc-directory
      (file-truename (concat user-emacs-directory "etc/")))
(mapc #'load (directory-files em-etc-directory t "elc?$"))
