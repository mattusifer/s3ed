define INIT_PACKAGE_EL
(progn
  (require 'package)
  (push '("melpa" . "http://melpa.org/packages/") package-archives)
  (package-initialize))
endef
export INIT_PACKAGE_EL

tramps3-lint:
	emacs -Q -batch \
		 --eval "$${INIT_PACKAGE_EL}"	 \
		 -l tramps3-util.el		 \
		 -l tramps3-io.el		 \
		 -l tramps3-mode.el		 \
		 -l tramps3.el			 \
		 -f package-lint-batch-and-exit	 \
		 tramps3.el tramps3-util.el tramps3-io.el tramps3-mode.el tramps3.el test/tramps3-tests.el

tramps3-test:
	export PATH=${PWD}/test/:$$PATH &&		 \
		emacs -batch -l ert			 \
			--eval "$${INIT_PACKAGE_EL}"     \
			-l tramps3-util.el		 \
			-l tramps3-io.el		 \
			-l tramps3-mode.el		 \
			-l tramps3.el			 \
			-l test/tramps3-tests.el	 \
			-f ert-run-tests-batch-and-exit

tramps3-travis-test:
	export PATH=${PWD}/test/:$$PATH &&		 \
		${EMACS} -Q --batch -L ${PWD}		 \
			-l ert				 \
			-l tramps3-util.el		 \
			-l tramps3-io.el		 \
			-l tramps3-mode.el		 \
			-l tramps3.el			 \
			-l test/tramps3-tests.el	 \
			-f ert-run-tests-batch-and-exit
