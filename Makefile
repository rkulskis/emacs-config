all:   
	git submodule update --init --recursive
	ln -sf $$(pwd)/config/.emacs ~/.emacs
clean:
	rm -rf *~
