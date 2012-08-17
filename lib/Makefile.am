## Process this file with automake to produce Makefile.in

libvicaredir	= $(libdir)/scheme
libvicare2dir	= $(libdir)/scheme

EXTRA_DIST	= \
	vicare/databases/sqlite3/constants.sls.in

# These files are hand-written.
nobase_dist_libvicare_DATA = \
	vicare/databases/sqlite3.sls			\
	vicare/databases/sqlite3/unsafe-capi.sls

# These files are created at configuration or make time.
nobase_nodist_libvicare2_DATA = \
	vicare/databases/sqlite3/constants.sls		\
	vicare/databases/sqlite3/features.sls

dist_pkglibexec_SCRIPTS		= compile-all.sps

# if WANT_NAUSICAA
# dist_pkglibexec_SCRIPTS		+= compile-nausicaa.sps
# nobase_dist_libvicare_DATA	+= nausicaa/databases/sqlite3.vicare.sls
# endif

## --------------------------------------------------------------------

VICARE_FASL_DIRECTORY		= $(builddir)/fasl
VICARE_COMPILE_LIBRARY_PATH	= $(builddir):$(srcdir):$(VICARE_LIBRARY_PATH)
VICARE_COMPILE_FASL_PATH	= $(VICARE_FASL_DIRECTORY):$(VICARE_FASL_PATH)
VICARE_COMPILE_LD_PATH		= $(builddir)/../src/.libs:$(LD_LIBRARY_PATH)

VICARE		= @VICARE@
# The variable "user_flags"  is for the user to be  set on the command
# line of "make", for example "user_flags=--print-loaded-libraries".
VICARE_COMPILE_FLAGS	= --fasl-directory $(VICARE_FASL_DIRECTORY) $(user_flags)
if WANT_VICARE_DEBUG
VICARE_COMPILE_FLAGS	+= --debug
endif
VICARE_COMPILE_ENV	= VICARE_LIBRARY_PATH=$(VICARE_COMPILE_LIBRARY_PATH)	\
			  VICARE_FASL_PATH=$(VICARE_COMPILE_FASL_PATH)		\
			  LD_LIBRARY_PATH=$(VICARE_COMPILE_LD_PATH)
VICARE_COMPILE_RUN	= $(VICARE_COMPILE_ENV) $(VICARE) $(VICARE_COMPILE_FLAGS)

VICARE_COMPILE_INST_FLAGS = --print-loaded-libraries
VICARE_COMPILE_INST_ENV	= VICARE_LIBRARY_PATH=$(VICARE_LIBRARY_PATH)
VICARE_COMPILE_INST_RUN	= $(VICARE_COMPILE_INST_ENV) $(VICARE) $(VICARE_COMPILE_INST_FLAGS)

VICARE_COMPILE_OPTIONS	= --compile-dependencies $(srcdir)/compile-all.sps

.PHONY : vfasl vfasl-installed

vfasl:
	test -d $(VICARE_FASL_DIRECTORY) || $(MKDIR_P) $(VICARE_FASL_DIRECTORY)
	$(VICARE_COMPILE_RUN) $(VICARE_COMPILE_OPTIONS)

vfasl-installed:
	$(VICARE_COMPILE_INST_RUN) $(VICARE_COMPILE_OPTIONS)

## --------------------------------------------------------------------

noinst_PROGRAMS		= features
features_SOURCES	= features.c

vicare/databases/sqlite3/features.sls : features
	./features >$@

## --------------------------------------------------------------------

CLEANFILES	= vicare/databases/sqlite3/features.sls

clean-local:
	-rm -rf $(VICARE_FASL_DIRECTORY)

### end of file
