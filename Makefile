all:   
	git submodule update --init --recursive
	ln -sf $$(pwd)/elisp/.emacs ~/.emacs
clean:
	rm -rf *~
