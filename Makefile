install:
	cp ./init.el ~/.emacs.d/init.el
	cp ./early-init.el ~/.emacs.d/early-init.el

collect:
	cp ~/.emacs.d/init.el ./init.el
	cp ~/.emacs.d/early-init.el ./early-init.el
