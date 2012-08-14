## Process this file with automake to produce Makefile.in

libvicaredir	= $(libdir)/scheme
libvicare2dir	= $(libdir)/scheme

EXTRA_DIST	= \
	compile-all.sps					\
	vicare/databases/sqlite3/constants.sls.in

nobase_dist_libvicare_DATA	= \
	vicare/databases/sqlite3.sls			\
	vicare/databases/sqlite3/unsafe-capi.sls

nobase_nodist_libvicare2_DATA	= \
	vicare/databases/sqlite3/constants.sls		\
	vicare/databases/sqlite3/features.sls

dist_pkglibexec_SCRIPTS	= compile-all.sps

## --------------------------------------------------------------------

FASLDIRECTORY	= $(builddir)/fasl

VICARE_FLAGS	= --fasl-directory $(FASLDIRECTORY) $(user_flags)

VICARE		= @VICARE@
VICARE_ENV	= VICARE_LIBRARY_PATH=$(builddir):$(srcdir):$(VICARE_LIBRARY_PATH)	\
		  VICARE_FASL_PATH=$(FASLDIRECTORY):$(VICARE_FASL_PATH)			\
		  LD_LIBRARY_PATH=$(builddir)/../src/.libs
VICARE_RUN	= $(VICARE_ENV) $(VICARE) $(VICARE_FLAGS)

VICARE_INST	= $(bindir)/vicare
VICARE_INST_ENV	= VICARE_LIBRARY_PATH=$(VICARE_LIBRARY_PATH)
VICARE_INST_RUN	= $(VICARE_INST_ENV) $(VICARE_INST)

.PHONY: vfasl vfasl-installed

vfasl:
	test -d $(FASLDIRECTORY) || $(MKDIR_P) $(FASLDIRECTORY)
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-all.sps

vfasl-installed:
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-all.sps

## --------------------------------------------------------------------

noinst_PROGRAMS		= features
features_SOURCES	= features.c

vicare/databases/sqlite3/features.sls : features
	./features >$@

CLEANFILES	= vicare/databases/sqlite3/features.sls

### end of file
