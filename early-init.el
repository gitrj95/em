;;;; em early init
;;;; rj

;;; smoothness

(setq native-comp-jit-compilation t
      native-compile-prune-cache t
      native-comp-async-report-warnings-errors nil
      read-process-output-max (* 4 1024 1024))

(defvar em/gc-cons-threshold 100000000)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold em/gc-cons-threshold
                  gc-cons-percentage 0.1)))

(defun em/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun em/restore-garbage-collection-h ()
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold em/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'em/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'em/restore-garbage-collection-h)

;;; gui

(require-theme 'modus-themes)
(setq modus-themes-to-toggle '(modus-operandi modus-vivendi))
(load-theme 'modus-operandi)

(set-face-attribute 'default nil :family "Iosevka Comfy Fixed")
(set-face-attribute 'default nil :height 160)
(setq line-spacing .1
      scroll-preserve-screen-position t
      scroll-conservatively 1
      scroll-margin 0
      next-screen-context-lines 0
      cursor-type 'box)

(menu-bar-mode -1)
(setq display-time-default-load-average nil)
(display-time-mode +1)
(scroll-bar-mode -1)
(pixel-scroll-precision-mode +1)

;;; config

(setq em-notes-directory "~/notes"
      ;; e.g. chicago
      calendar-latitude 41.881832
      calendar-longitude -87.623177)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/"))
      use-package-always-ensure t
      use-package-enable-imenu-support t
      custom-file (make-temp-file "emacs-sink"))
(eval-when-compile (require 'use-package))
