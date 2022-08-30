;;;; em install
;;;; rj


(defun load-pkg-list (root)
  (let* ((file (concat root "list.txt"))
	 (strings (with-temp-buffer
		    (insert-file-contents file)
		    (split-string (buffer-string) "\n" t))))
    (mapcar #'intern strings)))

(require 'package)
(setq package-archives nil)
(setq pkg-root (concat default-directory "pkg/"))
(defun calculate-package-roots (tail)
    (concat pkg-root tail))
(add-to-list 'package-archives `("gnu" . ,(calculate-package-roots "gnu/")))
(add-to-list 'package-archives `("nongnu" . ,(calculate-package-roots "nongnu/")))

(setq package-selected-packages (load-pkg-list pkg-root))
(package-install-selected-packages t)

(setq em-etc-directory (file-truename "etc/"))
(unless (file-directory-p em-etc-directory)
  (make-directory em-etc-directory))
