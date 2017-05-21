EMACS = emacs --batch --no-site-file --no-site-lisp

all: crystal-wy.el

crystal-wy.el : crystal.wy
	$(EMACS) -L . -l semantic/wisent/grammar -f wisent-batch-make-parser $<

clean:
	rm -f *~
