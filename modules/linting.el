;;; package --- Linting instructions
;;; Commentary:


;;; Code:
(use-package flycheck
  :ensure t
  :defer nil
  :ensure-system-package gawk
  :ensure-system-package gcc
  :ensure-system-package cppcheck
					; :ensure-system-package hadolint ;; Docker
  :ensure-system-package tidy
  :ensure-system-package shellcheck
  :ensure-system-package python3-flake8
  :init (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-google-cpplint
  :ensure t
  :defer nil)


(use-package google-c-style
  :ensure t
  :defer nil
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(use-package docker-compose-mode
  :ensure t
  :defer t)

(use-package nginx-mode
  :ensure t
  :defer t)

(use-package company-nginx
  :ensure t
  :defer t)

(provide 'linting)
;;; linting.el ends here
