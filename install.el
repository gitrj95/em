;;;; em install
;;;; rj


(require 'package)
(setq package-selected-packages
      '(modus-themes vertico orderless marginalia compat consult embark embark-consult eglot))
(setq em-etc-directory (file-truename "etc/"))
(unless (file-directory-p em-etc-directory)
  (make-directory em-etc-directory))
(setq em-package-directory (file-truename "pkg/"))
(mapcar #'package-install-file
	(directory-files em-package-directory t "tar"))
