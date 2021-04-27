;; Languages

(use-package use-package-ensure-system-package
  :ensure t
  :defer t)

;;; LaTeX
;;;; AUCTeX

(use-package auctex
  :ensure t
  :defer t
  :ensure-system-package (latex . texlive-full)
  :config
  '(TeX-view-program-selection
    (quote
     (((output-dvi has-no-display-manager)
       "dvi2tty")
      ((output-dvi style-pstricks)
       "dvips and gv")
      (output-dvi "xdvi")
      (output-pdf "Zathura")
      (output-html "xdg-open"))))
  (setq TeX-auto-save t)                  ;自动保存
  (setq TeX-parse-self t)                 ;解析
  (setq-default TeX-master nil)
  (dolist (hook (list
		 'LaTeX-mode-hook
		 'latex-mode-hook
		 ))
    (add-hook hook 'turn-on-reftex))
  ;;;; Compile and View document
  (defun latex-compile ()
    (interactive)
    (save-buffer)
    (TeX-command "LaTeX" 'TeX-master-file)
    (TeX-command "Biber" 'TeX-master-file)
    (TeX-command "LaTeX" 'TeX-master-file)
    (TeX-command "LaTeX" 'TeX-master-file)
    )
  (eval-after-load 'latex
    '(define-key LaTeX-mode-map (kbd "C-c C-b") 'latex-compile))
  
  ;;;; Outline Mode
  (defun turn-on-outline-minor-mode ()
    (outline-minor-mode 1))
  
  (add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
  (add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
  (setq outline-minor-mode-prefix "\C-c \C-o") ; Or something else
  
  ;;;; Umlaute to LATEX
  (define-minor-mode latex-umlaut-mode
    "Toggle the umlaut keys being changed to latex escapes.
       With no argument, this command toggles the mode.
       Non-null prefix argument turns on the mode.
       Null prefix argument turns off the mode.

       When this mode is enabled, the umlaut keys insert the
       latex escape."
    ;; The initial value.
    nil
    ;; The indicator for the mode line.
    " UmLaUt"
    ;; The minor mode bindings.
    `(
      (,(kbd "ö") . "\\\"o")
      (,(kbd "Ö") . "\\\"O")
      (,(kbd "ä") . "\\\"a")
      (,(kbd "Ä") . "\\\"A")
      (,(kbd "ü") . "\\\"u")
      (,(kbd "Ü") . "\\\"U")
      (,(kbd "ß") . "{\\ss}")
      )
    )
  (add-hook 'LaTeX-mode-hook 'latex-umlaut-mode)
  
  (use-package latex-extra
    :ensure t
    :defer t))



;; Python
;;; Formating
(use-package py-autopep8
  :ensure t
  :ensure-system-package (autopep8 . "sudo pip3 install autopep8")
  :defer t
  )
; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(use-package blacken
  :ensure t
  :ensure-system-package (black . "sudo pip3 install black")
  :defer nil
  :config (add-hook 'python-mode-hook #'blacken-mode)
)

;;; Docstring
(use-package python-docstring
  :ensure t
  :defer t
  :config
  (defun insert-shell-command-output (command)
    (interactive)
    "Get shell COMMAND output and insert it at point in current
buffer."
    (insert (shell-command-to-string command)))
  
  (require 'cl)
  (defun kill-matching-buffers-just-do-it (REGEXP)
    "Kill buffers whose names match REGEXP, without asking."
    (interactive)
    (cl-letf (((symbol-function 'kill-buffer-ask) #'kill-buffer))
      (kill-matching-buffers REGEXP)))
  
  (defun pydocstyle-buffer ()
    "Insert my command output into the buffer."
    (interactive)
    (when (eq major-mode 'python-mode)
      (progn
					;  (kill-matching-buffers-just-do-it "\*Pydocstyle\*")
	(when (get-buffer "*Pydocstyle*")
	  (quit-windows-on "\*Pydocstyle\*")
	  )
	(setq docstring (shell-command-to-string (concat "pydocstyle --explain " (buffer-file-name))))
	(when (not (= (length docstring) 0))
	  (with-help-window "*Pydocstyle*" ; Whatever buffer name you like.
	    ;; Use `princ`, `prin1`, `terpri`, etc. to put text in the displayed buffer
	    (princ docstring))
	  )
	)))
  
  (add-hook 'after-save-hook 'pydocstyle-buffer)
  (add-hook 'python-mode-hook 'python-docstring-mode))

;;; Elpy

(use-package elpy
  :ensure t
  :config
  (add-hook 'python-mode-hook #'elpy-mode)
  (add-to-list 'auto-mode-alist '("\\.sage\\'" . python-mode))
  ;(define-key elpy-mode-map (kbd "TAB") 'elpy-folding-toggle-at-point)
  )


(use-package company-quickhelp
  :ensure t
  :defer t)
;(add-hook 'prog-mode-hook #'company-quickhelp-mode)
;(global-set-key (kbd "C-r") 'elpy-folding-toggle-at-point)

;;; Parens
(use-package smartparens
  :ensure t
  :defer t
  :config
  (smartparens-global-mode 1))


(provide 'languages)
