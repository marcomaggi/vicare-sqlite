## Process this file with automake to produce Makefile.in

VICARE			= @VICARE@
testdir			= $(srcdir)
VICARE_FASL_DIRECTORY	= $(builddir)/../lib/fasl

VICARE_TEST_SEARCH_PATH	= $(srcdir):$(srcdir)/../lib:$(builddir)/../lib:$(VICARE_LIBRARY_PATH)
VICARE_TEST_FASL_PATH	= $(VICARE_FASL_DIRECTORY):$(VICARE_FASL_PATH)
VICARE_TEST_LD_PATH	= $(builddir)/../src/.libs:$(LD_LIBRARY_PATH)

# The variable "user_flags"  is for the user to be  set on the command
# line of "make", for example "user_flags=--print-loaded-libraries".
VICARE_TEST_FLAGS	= $(user_flags)
if WANT_VICARE_DEBUG
VICARE_TEST_FLAGS	+= --debug
endif
VICARE_TEST_ENV		= VICARE_LIBRARY_PATH=$(VICARE_TEST_SEARCH_PATH)	\
			  VICARE_FASL_PATH=$(VICARE_TEST_FASL_PATH)		\
			  LD_LIBRARY_PATH=$(VICARE_TEST_LD_PATH)		\
			  VICARE_SRC_DIR=$(srcdir) VICARE_BUILDDIR=$(builddir)	\
			  CHECK_TEST_NAME=$(name)
VICARE_TEST_RUN		= $(VICARE_TEST_ENV) $(GDB) $(VICARE) $(VICARE_TEST_FLAGS)

VICARE_SELECTED_TESTS	= $(testdir)/test-*$(file)*.sps
VICARE_SELECTED_DEMOS	= $(testdir)/demo-*$(file)*.sps

.PHONY: test tests demo demos

test tests:
	@echo
	@for f in $(VICARE_SELECTED_TESTS); do $(VICARE_TEST_RUN) $$f; done
	@echo

demo demos:
	@echo
	@for f in $(VICARE_SELECTED_DEMOS); do $(VICARE_TEST_RUN) $$f; done
	@echo

## --------------------------------------------------------------------
## interface to "make check"

# TESTS and TESTS_ENVIRONMENT are Automake variables.
TESTS_ENVIRONMENT	= VICARE_CHECK_QUIET=yes $(VICARE_TEST_RUN)
TESTS			= \
	test-vicare-sqlite3-plain-core.sps		\
	test-vicare-sqlite3-plain-connect.sps		\
	test-vicare-sqlite3-plain-exec.sps		\
	test-vicare-sqlite3-plain-statements.sps	\
	test-vicare-sqlite3-plain-blobs.sps		\
	test-vicare-sqlite3-plain-functions.sps		\
	test-vicare-sqlite3-plain-authorizer.sps

EXTRA_DIST = \
	test-vicare-sqlite3-plain-core.sps		\
	test-vicare-sqlite3-plain-connect.sps		\
	test-vicare-sqlite3-plain-exec.sps		\
	test-vicare-sqlite3-plain-statements.sps	\
	test-vicare-sqlite3-plain-statements-2.sps	\
	test-vicare-sqlite3-plain-blobs.sps		\
	test-vicare-sqlite3-plain-functions.sps		\
	test-vicare-sqlite3-plain-authorizer.sps	\
	\
	demo-for-documentation.sps

## --------------------------------------------------------------------
## interface to "make installcheck"

VICARE_INSTCHECK_LIBDIR		= $(libdir)/scheme
VICARE_INSTCHECK_SEARCH_PATH	= $(DESTDIR)$(VICARE_INSTCHECK_LIBDIR):$(VICARE_LIBRARY_PATH)
VICARE_INSTCHECK_FASL_PATH	= $(VICARE_FASL_PATH)
VICARE_INSTCHECK_LD_PATH	= $(DESTDIR)$(libdir):$(LD_LIBRARY_PATH)

VICARE_INSTCHECK_FLAGS	= --print-loaded-libraries $(user_flags)
VICARE_INSTCHECK_ENV	= VICARE_LIBRARY_PATH=$(VICARE_INSTCHECK_SEARCH_PATH)	\
			  VICARE_FASL_PATH=$(VICARE_INSTCHECK_FASL_PATH)	\
			  LD_LIBRARY_PATH=$(VICARE_INSTCHECK_LD_PATH)		\
			  VICARE_CHECK_QUIET=yes
VICARE_INSTCHECK_RUN	= $(VICARE_INSTCHECK_ENV) $(VICARE) $(VICARE_INSTCHECK_FLAGS)

installcheck-local:
	@for f in $(TESTS); do $(VICARE_INSTCHECK_RUN) $(srcdir)/$$f; done

### end of file
