;;;; em early init
;;;; rj

;;; smoothness

(let ((old-value (default-toplevel-value 'file-name-handler-alist)))
  (setq file-name-handler-alist nil)
  (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
  (put 'file-name-handler-alist 'initial-value old-value)
  (add-hook 'emacs-startup-hook :depth 101
	    (defun em/reset-file-handler-alist ()
              (setq file-name-handler-alist
		    (delete-dups (append file-name-handler-alist old-value))))))
(unless noninteractive
  (setq frame-inhibit-implied-resize t
	inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (advice-add #'display-startup-screen :override #'ignore)
  (setq initial-major-mode 'fundamental-mode
        initial-scratch-message nil))
(setq native-comp-jit-compilation t
      native-compile-prune-cache t
      native-comp-async-report-warnings-errors nil
      read-process-output-max (* 4 1024 1024))

(defvar em/gc-cons-threshold 100000000)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook :depth 101
          (lambda ()
            (setq gc-cons-threshold em/gc-cons-threshold
                  gc-cons-percentage 0.1)))

(defun em/defer-garbage-collection ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun em/restore-garbage-collection ()
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold em/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'em/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook #'em/restore-garbage-collection)

;;; gui

(require-theme 'modus-themes)
(setq modus-themes-to-toggle '(modus-operandi modus-vivendi))
(load-theme 'modus-operandi)

(set-face-attribute 'default nil :family "Iosevka Comfy Fixed")
(set-face-attribute 'default nil :height 160)
(setq scroll-preserve-screen-position t
      scroll-conservatively 1
      next-screen-context-lines 0)

(menu-bar-mode -1)
(setq display-time-default-load-average nil
      display-time-day-and-date t)
(display-time-mode 1)

(setq-default
 mode-line-format
 '("%e" mode-line-front-space
   (:propertize
    ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
    display
    (min-width
     (1.0)))
   mode-line-frame-identification mode-line-buffer-identification " " mode-line-modes mode-line-misc-info
   mode-line-end-spaces))

;;; config

(setq em-notes-directory "~/notes"
      ;; e.g. chicago
      calendar-latitude 41.881832
      calendar-longitude -87.623177
      em-terminal-modes-alist '(("eshell" . eshell) ("eat" . eat)))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/"))
      use-package-always-ensure t
      use-package-enable-imenu-support t
      custom-file (make-temp-file "emacs-sink"))
(eval-when-compile (require 'use-package))
