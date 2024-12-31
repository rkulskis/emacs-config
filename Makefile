SHELL := /bin/bash

all: submodules install

submodules:
	git submodule update --init --recursive

install: universal-ctags
	ln -sf $$(pwd)/elisp/.emacs ~/.emacs
	sudo apt-get update

universal-ctags:
	@if ! command -v $@ &> /dev/null; then \
		echo "universal-ctags not found, installing..."; \
		cd ctags && ./autogen.sh && ./configure && make && sudo make install; \
	else \
		echo "universal-ctags is already installed"; \
	fi

clean:
	rm -rf *~
