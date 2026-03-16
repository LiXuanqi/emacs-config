.PHONY: load test byte-compile freeze thaw clean-elc

load:
	emacs --batch -Q -l init.el

test:
	emacs --batch -Q -L . -L lisp -l tests/run-tests.el -f ert-run-tests-batch-and-exit

byte-compile:
	emacs --batch -Q -f batch-byte-compile lisp/*.el

freeze:
	emacs --batch -Q -l init.el --eval "(straight-freeze-versions)"

thaw:
	emacs --batch -Q -l init.el --eval "(straight-thaw-versions)"

clean-elc:
	find . -path './straight' -prune -o -name '*.elc' -delete
