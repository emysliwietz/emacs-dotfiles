;; Console
;;; Bash-Completion

(use-package bash-completion
  :defer t
  :ensure t
  :config
  (add-hook 'shell-dynamic-complete-functions
	    'bash-completion-dynamic-complete))


;;; Ansi-Term
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(global-set-key (kbd "C-ü") 'ansi-term)


(provide 'console)
