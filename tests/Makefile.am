## Process this file with automake to produce Makefile.in

testdir			= $(srcdir)
VICARE_SELECTED_TESTS	= $(testdir)/test-*$(file)*.sps
VICARE_SELECTED_PROOFS	= $(testdir)/proof-*$(file)*.sps

VICARE_TEST_SEARCH_PATH	= $(srcdir):$(srcdir)/../lib:$(builddir)/../lib:$(VICARE_LIBRARY_PATH)

VICARE_FASL_DIRECTORY	= $(builddir)/../lib/fasl
VICARE_FLAGS		= $(user_flags)
if WANT_DEBUG
VICARE_FLAGS		+= --debug
endif

# The variable "user_flags"  is for the user to be  set on the command
# line of "make".
VICARE		= @VICARE@
VICARE_ENV	= VICARE_LIBRARY_PATH=$(VICARE_TEST_SEARCH_PATH)		\
		  VICARE_FASL_PATH=$(VICARE_FASL_DIRECTORY):$(VICARE_FASL_PATH)	\
		  CHECK_TEST_NAME=$(name)					\
		  VICARE_SRC_DIR=$(srcdir) VICARE_BUILDDIR=$(builddir)		\
		  LD_LIBRARY_PATH=$(builddir)/../src/.libs
VICARE_RUN	= $(VICARE_ENV) $(GDB) $(VICARE) $(VICARE_FLAGS)

.PHONY: test tests proof proofs

test tests:
	@echo
	@for f in $(VICARE_SELECTED_TESTS); do $(VICARE_RUN) $$f; done
	@echo

proof proofs:
	@echo
	@for f in $(VICARE_SELECTED_PROOFS); do $(VICARE_RUN) $$f; done
	@echo

### interface to "make check"

# TESTS and TESTS_ENVIRONMENT are Automake variables.
TESTS_ENVIRONMENT	= VICARE_CHECK_QUIET=yes $(VICARE_RUN)
TESTS			=	\
	test-vicare-sqlite3-plain-core.sps		\
	test-vicare-sqlite3-plain-connect.sps		\
	test-vicare-sqlite3-plain-exec.sps		\
	test-vicare-sqlite3-plain-statements.sps	\
	test-vicare-sqlite3-plain-blobs.sps

EXTRA_DIST =	\
	test-vicare-sqlite3-plain-core.sps		\
	test-vicare-sqlite3-plain-connect.sps		\
	test-vicare-sqlite3-plain-exec.sps		\
	test-vicare-sqlite3-plain-statements.sps	\
	test-vicare-sqlite3-plain-blobs.sps		\
	\
	proof-basic.sps

## --------------------------------------------------------------------
## interface to "make installcheck"

VICARE_INSTCHECK_FLAGS	= --print-loaded-libraries $(user_flags)
VICARE_INSTCHECK_ENV	= \
	VICARE_LIBRARY_PATH=$(DESTDIR)$(VICARE_LIBDIR):$(VICARE_LIBRARY_PATH)	\
	LD_LIBRARY_PATH=$(DESTDIR)$(libdir)					\
	VICARE_CHECK_QUIET=yes
VICARE_INSTCHECK_RUN	= $(VICARE_INSTCHECK_ENV) $(VICARE) $(VICARE_INSTCHECK_FLAGS)

installcheck-local:
	@for f in $(TESTS); do $(VICARE_INSTCHECK_RUN) $(srcdir)/$$f; done

### end of file
