## Process this file with automake to produce Makefile.in

vicare_sqlite3_CURRENT	= @vicare_sqlite3_VERSION_INTERFACE_CURRENT@
vicare_sqlite3_REVISION	= @vicare_sqlite3_VERSION_INTERFACE_REVISION@
vicare_sqlite3_AGE	= @vicare_sqlite3_VERSION_INTERFACE_AGE@
vicare_sqlite3_VERSION	= $(vicare_sqlite3_CURRENT):$(vicare_sqlite3_REVISION):$(vicare_sqlite3_AGE)

lib_LTLIBRARIES			= libvicare-sqlite3.la
libvicare_sqlite3_la_LDFLAGS	= -version-info $(vicare_sqlite3_VERSION)
libvicare_sqlite3_la_SOURCES	= vicare-sqlite3.c

### end of file
