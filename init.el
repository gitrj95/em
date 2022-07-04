;;;; em
;;;; rj


(defun global-map-set-kbd (cmd-string fcn)
  (define-key global-map (kbd cmd-string) fcn))

(require 'org)
(global-map-set-kbd "C-c a" #'org-agenda)
(global-map-set-kbd "C-c c" #'org-capture)
(global-map-set-kbd "C-c l" #'org-store-link)
(global-map-set-kbd "C-c C-l" #'org-insert-link)

(require 'windmove)
(global-map-set-kbd "C-M-<up>" #'windmove-up)
(global-map-set-kbd "C-M-<down>" #'windmove-down)
(global-map-set-kbd "C-M-<left>" #'windmove-left)
(global-map-set-kbd "C-M-<right>" #'windmove-right)

(require 'package)
(setq em-package-directory
      (concat user-emacs-directory "pkg/"))
(push em-package-directory package-directory-list)
(package-initialize)

(defun install-file-or-require (filename)
  (unless (package-installed-p filename)
    (package-install-file filename)))
(let ((packages (directory-files em-package-directory nil ".tar")))
  (defalias 'concat-em (apply-partially #'concat em-package-directory))
  (mapcar #'install-file-or-require (mapcar #'concat-em packages)))

(setq-default line-spacing .1)
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1)
(setq-default scroll-margin 0)
(setq-default next-screen-context-lines 0)
(require 'modus-themes)
(modus-themes-load-themes)
(modus-themes-load-vivendi)
(global-map-set-kbd "<f8>" #'modus-themes-toggle)

(require 'vertico)
(vertico-mode)
