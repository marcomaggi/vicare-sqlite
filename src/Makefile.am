## Process this file with automake to produce Makefile.in

vicare_sqlite_CURRENT	= @vicare_sqlite_VERSION_INTERFACE_CURRENT@
vicare_sqlite_REVISION	= @vicare_sqlite_VERSION_INTERFACE_REVISION@
vicare_sqlite_AGE	= @vicare_sqlite_VERSION_INTERFACE_AGE@
vicare_sqlite_VERSION	= $(vicare_sqlite_CURRENT):$(vicare_sqlite_REVISION):$(vicare_sqlite_AGE)

lib_LTLIBRARIES			= libvicare-sqlite.la
libvicare_sqlite_la_LDFLAGS	= -version-info $(vicare_sqlite_VERSION)
libvicare_sqlite_la_SOURCES	= vicare-sqlite.c

### end of file
