.POSIX:
.force:

all: install

install: .force
	mkdir -p  ~/.emacs.d/
	ln -s $(PWD)/etc ~/.emacs.d/etc
	ln -s $(PWD)/init.el ~/.emacs.d/init.el
	ln -s $(PWD)/early-init.el ~/.emacs.d/early-init.el

clean: .force
	rm -rf ~/.emacs.d/
