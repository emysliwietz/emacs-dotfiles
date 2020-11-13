;;; Performance stats
(make-thread (progn
	       (require 'symon)
	       (symon-mode 1)))


(provide 'symon-tweaks)
