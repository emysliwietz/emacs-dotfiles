;; Languagetool
(use-package langtool
  :defer t
  :ensure t)

(setq langtool-http-server-host "localhost"
      langtool-http-server-port 8081)

(define-minor-mode langtool-mode
  "Grammar check with langtool"
  :lighter " LT" 
  (if langtool-mode
      (add-hook 'after-save-hook
                'langtool-check)
    (remove-hook 'after-save-hook
                'langtool-check)))


(defun langtool-mode-hook ()
  (langtool-mode 1))

;(add-hook 'org-mode-hook 'langtool-mode-hook)
(add-hook 'latex-mode-hook 'langtool-mode-hook)
(add-hook 'LaTeX-mode-hook 'langtool-mode-hook)

(provide 'languagetool)
