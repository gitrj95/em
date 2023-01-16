install:
	mkdir -p  ~/.emacs.d/
	mkdir -p $(shell pwd)/etc
	ln -s $(shell pwd)/etc ~/.emacs.d/etc
	ln -s $(shell pwd)/init.el ~/.emacs.d/init.el
	ln -s $(shell pwd)/early-init.el ~/.emacs.d/early-init.el

clean:
	rm -rf ~/.emacs.d/

.PHONY: install clean
