;;;; em init
;;;; rj


(when (native-comp-available-p)
  (setq-default native-comp-async-report-warnings-errors nil))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-install 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

(defun load-em-pkgs (file)
  (let ((strings (with-temp-buffer
		   (insert-file-contents file)
		   (split-string (buffer-string) "\n" t))))
    (mapcar #'intern strings)))
(mapcar #'package-install
        (load-em-pkgs (concat user-emacs-directory "pkg.txt")))

(defun global-map-set-kbd (cmd-string fcn)
  (define-key global-map (kbd cmd-string) fcn))

(global-map-set-kbd "C-c a" #'org-agenda)
(global-map-set-kbd "C-c c" #'org-capture)
(global-map-set-kbd "C-c l" #'org-store-link)
(global-map-set-kbd "C-c C-l" #'org-insert-link)

(global-map-set-kbd "C-M-<up>" #'windmove-up)
(global-map-set-kbd "C-M-<down>" #'windmove-down)
(global-map-set-kbd "C-M-<left>" #'windmove-left)
(global-map-set-kbd "C-M-<right>" #'windmove-right)
(global-map-set-kbd "C-M-S-<up>" #'windmove-swap-states-up)
(global-map-set-kbd "C-M-S-<down>" #'windmove-swap-states-down)
(global-map-set-kbd "C-M-S-<left>" #'windmove-swap-states-left)
(global-map-set-kbd "C-M-S-<right>" #'windmove-swap-states-right)

(global-map-set-kbd "C-x u" #'vundo)
(global-map-set-kbd "C-x C-b" #'ibuffer)
(global-map-set-kbd "M-s O" #'multi-occur)
(global-map-set-kbd "M-g i" #'imenu)

(savehist-mode)
(save-place-mode)
(recentf-mode)

(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(require 'grep)
(when (executable-find "rg")
  (grep-apply-setting 'grep-find-command '("rg -n -H --no-heading -e ''" . 27)))
(global-map-set-kbd "C-c g" #'grep-find)

(set-frame-font "Iosevka 16" nil t)
(setq-default line-spacing .1)
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1)
(setq-default scroll-margin 0)
(setq-default next-screen-context-lines 0)
(electric-pair-mode)

(load-theme 'modus-vivendi t)
(global-map-set-kbd "<f8>" #'modus-themes-toggle)

(vertico-mode)
(vertico-multiform-mode)
(setq vertico-multiform-commands
      '((imenu buffer)))
(setq vertico-multiform-categories
      '((file buffer)))

(define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
(add-hook #'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(define-key vertico-map (kbd "M-q") #'vertico-quick-insert)

(global-corfu-mode)
(define-key corfu-map (kbd "SPC") #'corfu-insert-separator)

(unless (display-graphic-p)
  (corfu-terminal-mode))

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(marginalia-mode)

(setq-default prefix-help-command #'embark-prefix-help-command)
(global-map-set-kbd "C-c e a" #'embark-act)
(global-map-set-kbd "C-c e d" #'embark-dwim)

(setq em-etc-directory
      (file-truename (concat user-emacs-directory "etc/")))
(mapcar #'load (directory-files em-etc-directory t "elc?$"))

(setq gc-cons-threshold 100000000)
