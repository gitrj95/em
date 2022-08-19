;;;; em install
;;;; rj


(require 'package)
(setq package-archives nil)
(setq pkg-root
      (concat default-directory "pkg/"))
(defun calculate-package-roots (tail)
    (concat pkg-root tail))
(add-to-list 'package-archives `("gnu" . ,(calculate-package-roots "gnu/")))
(add-to-list 'package-archives `("nongnu" . ,(calculate-package-roots "nongnu/")))
(setq package-selected-packages
      '(modus-themes vertico corfu-terminal orderless marginalia embark))
(package-install-selected-packages t)

(setq em-etc-directory (file-truename "etc/"))
(unless (file-directory-p em-etc-directory)
  (make-directory em-etc-directory))
