install:
	echo "Overwrite installed files?"
	read d || exit
	cp ./init.el ~/.emacs.d/init.el
	cp ./early-init.el ~/.emacs.d/early-init.el
	cp ./abbrev_defs ~/.emacs.d/abbrev_defs

collect:
	cp ~/.emacs.d/init.el ./init.el
	cp ~/.emacs.d/early-init.el ./early-init.el
	cp ~/.emacs.d/abbrev_defs ./abbrev_defs
