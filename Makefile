define INIT_PACKAGE_EL
(progn
  (require 'package)
  (push '("melpa" . "http://melpa.org/packages/") package-archives)
  (package-initialize))
endef
export INIT_PACKAGE_EL

s3ed-lint:
	emacs -Q -batch \
		 --eval "$${INIT_PACKAGE_EL}" \
		 -l s3ed-util.el \
		 -l s3ed-io.el \
		 -l s3ed-mode.el \
		 -l s3ed.el \
		 -f package-lint-batch-and-exit \
		 s3ed.el s3ed-util.el s3ed-io.el s3ed-mode.el s3ed.el test/s3ed-tests.el

s3ed-test:
	PATH=${PWD}/test/:$$PATH emacs -batch -l ert \
			--eval "$${INIT_PACKAGE_EL}" \
			-l s3ed-util.el \
			-l s3ed-io.el \
			-l s3ed-mode.el \
			-l s3ed.el \
			-l test/s3ed-tests.el \
			-f ert-run-tests-batch-and-exit

s3ed-travis-test:
	PATH=${PWD}/test/:$$PATH ${EMACS} -Q --batch -L ${PWD} \
			-l ert \
			-l s3ed-util.el \
			-l s3ed-io.el \
			-l s3ed-mode.el \
			-l s3ed.el \
			-l test/s3ed-tests.el \
			-f ert-run-tests-batch-and-exit
