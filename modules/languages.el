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

;; C-c ? (or M-x TeX-doc) gives documentation for the symbol at point,
;; or for any package, command or document.
(load "auctex.el" nil t t)
(require 'tex-mik)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; when you open a file, ask for the master file but not changing any file
;; this will make the LaTeX-save-and-compile deal with multi-files project
(setq-default TeX-master 'shared)
;; LaTeX-mode for tex file
;; (add-hook 'plain-TeX-mode-hook 'LaTeX-mode)
(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
;; company-auctex
(use-package company-auctex
  :ensure t
  :defer t)
(require 'tex-buf)
;; (require 'auctex-latexmk)
;; (auctex-latexmk-setup)
;;
;; remove fdb_latexmk file will solve the problem
;; of the reference number instead of question mark (?)
(setq LaTeX-clean-intermediate-suffixes
	  (append LaTeX-clean-intermediate-suffixes
			  (list "\\.fdb_latexmk")))
(defun LaTeX-save-and-compile ()
  "Save and compile the tex project using latexmk.
If compilation fails, split the current window and open error-buffer
then jump to the error line, if errors corrected, close the error-buffer
window and close the *TeX help* buffer."
  (interactive)
  (progn
	;; ;; turn off smartparens because LaTeX-electric-left-right-brace
	;; ;; offers more for specific LaTeX mode
	;; ;; Since SP is always triggered later by sth., so put these two lines here
	;; (turn-off-smartparens-mode)
	;; (setq LaTeX-electric-left-right-brace t)
	(let ((TeX-save-query nil)
		  (TeX-process-asynchronous nil)
		  (master-file (TeX-master-file)))
	  (TeX-save-document "")
	  ;; clean all generated files before compile
	  ;; DO NOT do it when up-to-date, remove this line in proper time
	  (TeX-clean t)
	  (TeX-run-TeX "latexmk"
				   (TeX-command-expand "latexmk -pdflatex='pdflatex -file-line-error -synctex=1' -pdf %s" 'TeX-master-file)
				   master-file)
	  (if (plist-get TeX-error-report-switches (intern master-file))
		  ;; avoid creating multiple windows to show the *TeX Help* error buffer
		  (if (get-buffer-window (get-buffer "*TeX Help*"))
			  (TeX-next-error)
			(progn
			  (split-window-vertically -10)
			  (TeX-next-error)))
		;; if no errors, delete *TeX Help* window and buffer
		(if (get-buffer "*TeX Help*")
			(progn
			  (if (get-buffer-window (get-buffer "*TeX Help*"))
				  (delete-windows-on "*TeX Help*"))
			  (kill-buffer "*TeX Help*")))))))
(add-hook 'LaTeX-mode-hook
		  (lambda ()
			(setq LaTeX-item-indent 0)
			(visual-line-mode)
			(flyspell-mode)
			(setq-default TeX-newline-function 'advanced-return)
			;; make the code look like the pdf file, C-c C-o ... for commands
			;; If it should be activated in all AUCTEX modes, use TeX-mode-hook
			;; instead of LaTeX-mode-hook.
			(TeX-fold-mode 1)
			;; usepackage
			(setq tex-tree-roots t)
			(LaTeX-math-mode)
			;; this line have to be here to make company work
			(company-auctex-init)
			;; disable smartparens-mode completely and use
			;; LaTeX-electric-left-right-brace instead
			(push 'latex-mode sp-ignore-modes-list)
			(setq LaTeX-electric-left-right-brace t)
			;; the following line will inset braces after _ or ^
			;; unnecessarily most of time
			;; (setq TeX-electric-sub-and-superscript t)
			;; NOTE: C-c C-a to combine C-c C-c and C-c C-v
			;; C-u C-c C-c latexmk (or others like View) so you can change the command line
			;; jump: the following makes viewing the pdf right at the line of the tex file
			(add-to-list 'TeX-command-list
						 '("latexmk" "latexmk -pdflatex='pdflatex -file-line-error -synctex=1' -pdf %s"
						   TeX-run-command nil t :help "Run latexmk") t)
			(setq TeX-command-default "latexmk")
			(push '("%(masterdir)" (lambda nil (file-truename (TeX-master-directory))))
				  TeX-expand-list)
			(push '("Okular" "okular --unique %o#src:%n%(masterdir)%b")
				  TeX-view-program-list)
			(push '(output-pdf "Okular") TeX-view-program-selection)
			(TeX-source-correlate-mode)
			(server-force-delete)  ;; WARNING: Kills any existing edit server
			(setq TeX-source-correlate-method 'synctex
				  TeX-source-correlate-start-server t)
			;;
			(bind-keys :map LaTeX-mode-map
					   ;; default C-c C-e rebound and cannot be rebound
					   ("C-c C-x e" . LaTeX-environment)
					   ("C-c C-x s" . LaTeX-section)
					   ("C-c C-x m" . TeX-insert-macro)
					   ("C-x C-s" . LaTeX-save-and-compile)
					   ;; default C-c. not working and replaced by org-time-stamp
					   ("C-c m" . LaTeX-mark-environment)
					   ;; ("<tab>" . TeX-complete-symbol)
					   ("M-<return>" . LaTeX-insert-item)
					   )))
(setq LaTeX-command-section-level t)
;; C-c C-c without prompt, use Clean by default, to clean aux and log files
;; Use "Clean All" to clean files including generated pdf file
;; Or use M-x Tex-clean (Clean) and prefix(Clean All)
;; (setq TeX-command-force "Clean")
(setq TeX-clean-confirm nil)
;; RefTex -- built-in
;; Turn on RefTeX in AUCTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; Activate nice interface between RefTeX and AUCTeX
(setq reftex-plug-into-AUCTeX t)
;; magic-latex-buffer
;; (require 'magic-latex-buffer)
;; (add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
;; latex-preview-pane
;; (add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
(setq
 ;; Function for reading \includegraphics files
 LaTeX-includegraphics-read-file 'LaTeX-includegraphics-read-file-relative
 ;; Strip known extensions from image file name
 LaTeX-includegraphics-strip-extension-flag nil)
;; (setq LaTeX-section-hook
;;		  '(LaTeX-section-heading
;;			LaTeX-section-title
;;			LaTeX-section-toc
;;			LaTeX-section-section
;;			LaTeX-section-label))

;; outline, C-c @ prefix
(add-hook 'outline-minor-mode-hook
		  (lambda ()
			(require 'outline-magic)
			(bind-keys :map outline-minor-mode-map ("<C-tab>" . outline-cycle))))
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)

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
