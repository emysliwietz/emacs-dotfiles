(require 'package)
(package-initialize)
(add-to-list 'package-archives 
	     '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
 	     '("gnu" . "https://elpa.gnu.org/packages/"))

(defun package-refresh-contents-async ()
  (interactive)
  (package-refresh-contents t)
  (message "Refreshing packages"))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Test out packages
(use-package try
    :defer
    :ensure t)

(provide 'package-management)

