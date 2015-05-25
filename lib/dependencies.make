## dependencies.make --
#
# Automatically built.

EXTRA_DIST +=  \
	lib/vicare/databases/sqlite3/constants.vicare.sls.in

lib/vicare/databases/sqlite3.fasl: \
		lib/vicare/databases/sqlite3.vicare.sls \
		lib/vicare/databases/sqlite3/constants.fasl \
		lib/vicare/databases/sqlite3/unsafe-capi.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_databases_sqlite3_fasldir = $(bundledlibsdir)/vicare/databases
lib_vicare_databases_sqlite3_vicare_slsdir  = $(bundledlibsdir)/vicare/databases
nodist_lib_vicare_databases_sqlite3_fasl_DATA = lib/vicare/databases/sqlite3.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_databases_sqlite3_vicare_sls_DATA = lib/vicare/databases/sqlite3.vicare.sls
endif
EXTRA_DIST += lib/vicare/databases/sqlite3.vicare.sls
CLEANFILES += lib/vicare/databases/sqlite3.fasl

lib/vicare/databases/sqlite3/constants.fasl: \
		lib/vicare/databases/sqlite3/constants.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_databases_sqlite3_constants_fasldir = $(bundledlibsdir)/vicare/databases/sqlite3
lib_vicare_databases_sqlite3_constants_vicare_slsdir  = $(bundledlibsdir)/vicare/databases/sqlite3
nodist_lib_vicare_databases_sqlite3_constants_fasl_DATA = lib/vicare/databases/sqlite3/constants.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_databases_sqlite3_constants_vicare_sls_DATA = lib/vicare/databases/sqlite3/constants.vicare.sls
endif
CLEANFILES += lib/vicare/databases/sqlite3/constants.fasl

lib/vicare/databases/sqlite3/unsafe-capi.fasl: \
		lib/vicare/databases/sqlite3/unsafe-capi.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_databases_sqlite3_unsafe_capi_fasldir = $(bundledlibsdir)/vicare/databases/sqlite3
lib_vicare_databases_sqlite3_unsafe_capi_vicare_slsdir  = $(bundledlibsdir)/vicare/databases/sqlite3
nodist_lib_vicare_databases_sqlite3_unsafe_capi_fasl_DATA = lib/vicare/databases/sqlite3/unsafe-capi.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_databases_sqlite3_unsafe_capi_vicare_sls_DATA = lib/vicare/databases/sqlite3/unsafe-capi.vicare.sls
endif
EXTRA_DIST += lib/vicare/databases/sqlite3/unsafe-capi.vicare.sls
CLEANFILES += lib/vicare/databases/sqlite3/unsafe-capi.fasl

lib/vicare/databases/sqlite3/features.fasl: \
		lib/vicare/databases/sqlite3/features.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_databases_sqlite3_features_fasldir = $(bundledlibsdir)/vicare/databases/sqlite3
lib_vicare_databases_sqlite3_features_vicare_slsdir  = $(bundledlibsdir)/vicare/databases/sqlite3
nodist_lib_vicare_databases_sqlite3_features_fasl_DATA = lib/vicare/databases/sqlite3/features.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_databases_sqlite3_features_vicare_sls_DATA = lib/vicare/databases/sqlite3/features.vicare.sls
endif
CLEANFILES += lib/vicare/databases/sqlite3/features.fasl


### end of file
# Local Variables:
# mode: makefile-automake
# End:
