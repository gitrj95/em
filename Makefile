.POSIX:
.force:

all: install

install: .force
	mkdir -p  ~/.emacs.d/
	ln -s $(shell pwd)/etc ~/.emacs.d/etc
	ln -s $(shell pwd)/init.el ~/.emacs.d/init.el
	ln -s $(shell pwd)/early-init.el ~/.emacs.d/early-init.el

clean: .force
	rm -rf ~/.emacs.d/
