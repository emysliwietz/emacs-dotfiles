;;; package --- Linting instructions
;;; Commentary:


;;; Code:
(use-package flycheck
  :ensure t
  :ensure-system-package gawk
  :ensure-system-package gcc
  :ensure-system-package cppcheck
					; :ensure-system-package hadolint ;; Docker
  :ensure-system-package tidy
  :ensure-system-package shellcheck
  :ensure-system-package python3-flake8
  :defer t
  :init (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'linting)
;;; linting.el ends here
