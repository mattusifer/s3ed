tramps3-lint:
	./self_lint.sh

tramps3-test:
	emacs -batch -l ert			\
		-l tramps3-constants.el		\
		-l tramps3-util.el		\
		-l tramps3-io.el		\
		-l tramps3-mode.el		\
		-l tramps3.el			\
		-l test/tramps3-tests.el	\
		-f ert-run-tests-batch-and-exit
