(setq module-dir "~/.config/emacs/modules")

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun module-visit ()
  (interactive)
  (find-file module-dir))

(defun recompile-modules ()
  (interactive)
  (digit-argument nil)
  (byte-recompile-directory module-dir 0))

(defun config-reload ()
  (interactive)
  ;;(recompile-modules)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(global-set-key (kbd "C-c e f") 'config-visit)
(global-set-key (kbd "C-c e m") 'module-visit)
(global-set-key (kbd "C-c r") 'config-reload)

(provide 'config)
