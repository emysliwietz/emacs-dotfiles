(require 'proverif)

(setq auto-mode-alist
      (cons '("\\.horn$" . proverif-horn-mode) 
	    (cons '("\\.horntype$" . proverif-horntype-mode) 
		  (cons '("\\.pv[l]?$" . proverif-pv-mode) 
			(cons '("\\.pi$" . proverif-pi-mode) auto-mode-alist)))))
(autoload 'proverif-pv-mode "proverif" "Major mode for editing ProVerif code." t)
(autoload 'proverif-pi-mode "proverif" "Major mode for editing ProVerif code." t)
(autoload 'proverif-horn-mode "proverif" "Major mode for editing ProVerif code." t)
(autoload 'proverif-horntype-mode "proverif" "Major mode for editing ProVerif code." t)


(provide 'proverif-config)
