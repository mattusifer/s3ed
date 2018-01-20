#!/bin/sh -e

EMACS="${EMACS:=emacs}"

INIT_PACKAGE_EL="(progn
  (require 'package)
  (push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)
  (package-initialize))"

# Refresh package archives, because the test suite needs to see at least
# package-lint and cl-lib.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         --eval '(package-refresh-contents)' \
         --eval "(unless (package-installed-p 'cl-lib) (package-install 'cl-lib))"

# Byte compile, failing on byte compiler errors, or on warnings unless ignored
if [ -n "${EMACS_LINT_IGNORE+x}" ]; then
    ERROR_ON_WARN=nil
else
    ERROR_ON_WARN=t
fi

"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l tramps3-constants.el		\
         -l tramps3-util.el		\
         -l tramps3-io.el		\
         -l tramps3-mode.el		\
         -l tramps3.el			\
         --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
         -f batch-byte-compile \
         tramps3.el tramps3-constants.el tramps3-util.el tramps3-io.el tramps3-mode.el tramps3.el test/tramps3-tests.el

# Lint ourselves
# Lint failures are ignored if EMACS_LINT_IGNORE is defined, so that lint
# failures on Emacs 24.2 and below don't cause the tests to fail, as these
# versions have buggy imenu that reports (defvar foo) as a definition of foo.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l tramps3-constants.el		\
         -l tramps3-util.el		\
         -l tramps3-io.el		\
         -l tramps3-mode.el		\
         -l tramps3.el			\
         -f package-lint-batch-and-exit \
         tramps3.el tramps3-constants.el tramps3-util.el tramps3-io.el tramps3-mode.el tramps3.el test/tramps3-tests.el || [ -n "${EMACS_LINT_IGNORE+x}" ]
